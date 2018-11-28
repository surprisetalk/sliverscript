
{-# LANGUAGE OverloadedStrings, UnboxedTuples, DuplicateRecordFields, Rank2Types #-}


-- MODULE ---------------------------------------------------------------------

module Parser where


-- IMPORTS --------------------------------------------------------------------

import Control.Exception ( assert )

import qualified Control.Applicative as Applicative ( Applicative(..), Alternative(..) )
import Control.Monad

import qualified Data.Set as Set

import Data.Binary (Binary, get, put)

import Data.Bits ((.&.), (.|.), shiftL)

import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Char8 as Char8

import qualified Data.Text as Text
import qualified Data.Text.Internal.Builder as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Char as Char

import Foreign.ForeignPtr ( ForeignPtr, withForeignPtr ) 
import Foreign.Storable ( peekByteOff )
import Foreign.Ptr ( plusPtr )
import GHC.ForeignPtr ( touchForeignPtr, unsafeForeignPtrToPtr )
import GHC.Word ( Word8 )


-- RESULT ---------------------------------------------------------------------

data Result a
  = Ok a State ParseError
  | Err ParseError


-- STATE ----------------------------------------------------------------------

data State
  = State
    { _source   :: ForeignPtr Word8
    , _offset   :: Int
    , _terminal :: Int
    , _indent   :: Int
    , _row      :: Int
    , _col      :: Int
    , _context  :: ContextStack
    }

getPosition :: Parser Position
getPosition
  = Parser
  $ \state@(State _ _ _ _ row col _) _ _ eok _ ->
      eok (Position row col) state noError

getIndent :: Parser Int
getIndent
  = Parser
  $ \state@(State _ _ _ indent _ _ _) _ _ eok _ ->
      eok indent state noError

getCol :: Parser Int
getCol
  = Parser
  $ \state@(State _ _ _ _ _ col _) _ _ eok _ ->
      eok col state noError

pushContext :: Position -> Context -> Parser ()
pushContext pos ctx
  = Parser
  $ \state@(State _ _ _ _ _ _ context) _ _ eok _ ->
      eok () (state { _context = (ctx, pos) : context }) noError

popContext :: a -> Parser a
popContext value
  = Parser
  $ \state@(State _ _ _ _ _ _ context) _ _ eok _ ->
      eok value (state { _context = tail context }) noError

setIndent :: Int -> Parser ()
setIndent indent
  = Parser
  $ \state _ _ eok _ ->
      eok () (state { _indent = indent }) noError



-- ERROR ----------------------------------------------------------------------

data Error
  = ErrorTODO
  -- = CommentOnNothing Region
  -- | UnexpectedPort Region Name
  -- | TypeWithBadDefinition Region Name Name
  -- | TypeWithoutDefinition Region Name
  | Parse Region (Maybe Region) Problem

data ParseError
  = ParseError Int Int Problem
 -- ParseError row col problem

data Problem
  = ProblemTODO
  | Tab
  | EndOfFile_Comment
  -- | EndOfFile_Shader
  -- | EndOfFile_String
  -- | EndOfFile_MultiString
  -- | EndOfFile_Char
  -- | NewLineInString
  -- | NewLineInChar
  -- | BadEscape Int EscapeProblem
  -- | BadChar Int
  -- | BadNumberDot Int
  -- | BadNumberEnd
  -- | BadNumberExp
  -- | BadNumberHex
  -- | BadNumberZero
  -- | FloatInPattern
  -- | BadShader Text.Text
  -- | BadUnderscore Int
  -- | BadOp BadOp ContextStack
  | Theories ContextStack [Theory]

data EscapeProblem
  = UnknownEscape
  | UnicodeSyntax
  | UnicodeRange
  | UnicodeLength Int String

data Theory
  = Expecting Next
  | Keyword String
  | Symbol String
  | LowVar
  | CapVar
  | InfixOp
  | Digit
  | BadSpace
  deriving
    ( Eq
    , Ord
    )

data Next
  = Decl
  | Expr
  | AfterOpExpr Name
  | ElseBranch
  | Arg
  | Pattern
  | Type
  | Listing
  | Exposing
  deriving
    ( Eq
    , Ord
    )
 
noError :: ParseError
noError
  = ParseError 0 0
  $ Theories [] []

expect :: Int -> Int -> ContextStack -> Theory -> ParseError
expect row col ctx theory
  = ParseError row col
  $ Theories ctx [theory]


-- NAME -----------------------------------------------------------------------

type Name
  = Text.Text

fromForeignPtr :: ForeignPtr Word8 -> Int -> Int -> Name
fromForeignPtr fptr offset len
  = Text.decodeUtf8 $ B.PS fptr offset len


-- CONTEXT --------------------------------------------------------------------

data Context
  ----------------
  = ExprIf
  | ExprLet
  | ExprFunc
  | ExprCase
  | ExprList
  | ExprTuple
  | ExprRecord
  ----------------
  -- | Definition Name
  -- | Annotation Name
  ----------------
  | TypeTuple
  | TypeRecord
  ----------------
  | PatternList
  | PatternTuple
  | PatternRecord
  ----------------
  -- | Module
  -- | Import
  -- | TypeUnion
  -- | TypeAlias
  -- | Infix
  -- | Port
  ----------------
  deriving
    ( Eq
    , Ord
    )

type ContextStack
  = [ ( Context, Position ) ]


-- REGIONS --------------------------------------------------------------------

data Region
  = Region
    { _start :: Position
    , _end   :: Position
    }
  deriving
    ( Eq
    , Ord
    )

data Position
  = Position
    { _line   :: Int
    , _column :: Int
    }
  deriving
    ( Eq
    , Ord
    )

merge :: Region -> Region -> Region
merge (Region start _) (Region _ end) =
  Region start end

zero :: Region
zero =
  Region (Position 0 0) (Position 0 0)

one :: Region
one =
  Region (Position 1 1) (Position 1 1)

-- encode :: Region -> Json.Value
-- encode (Region start end) =
--   Json.object
--     [ ("start", encodePosition start)
--     , ("end", encodePosition end)
--     ]
-- 
-- encodePosition :: Position -> Json.Value
-- encodePosition (Position line column) =
--   Json.object
--     [ ("line", Json.int line)
--     , ("column", Json.int column)
--     ]

instance Binary Region where
  get =
    Region <$> get <*> get

  put (Region start end) =
    do  put start
        put end

instance Binary Position where
  get =
    Position <$> get <*> get

  put (Position line column) =
    do  put line
        put column


-- PARSER ---------------------------------------------------------------------

newtype Parser a
  = Parser
    (
      forall b.
      State
      -> (a -> State -> ParseError -> b)  -- consumed ok
      -> (              ParseError -> b)  -- consumed err
      -> (a -> State -> ParseError -> b)  -- empty ok
      -> (              ParseError -> b)  -- empty err
      -> b
    )

type SParser a =
  Parser (a, Position, SPos)

newtype SPos = SPos Position

instance Functor Parser where
  {-# INLINE fmap #-}
  fmap f (Parser parser) =
    Parser $ \state cok cerr eok eerr ->
      let
        cok' x s e = cok (f x) s e
        eok' x s e = eok (f x) s e
      in
        parser state cok' cerr eok' eerr

instance Applicative.Applicative Parser where
    {-# INLINE pure #-}
    pure = return

    {-# INLINE (<*>) #-}
    (<*>) = ap

instance Applicative.Alternative Parser where
    {-# INLINE empty #-}
    empty = allTheOptionsFailed

    {-# INLINE (<|>) #-}
    (<|>) = oneOfHelp

instance Monad Parser where
  {-# INLINE return #-}
  return value =
    Parser $ \state _ _ eok _ ->
      eok value state noError

  {-# INLINE (>>=) #-}
  (Parser parser) >>= callback =
    Parser $ \state cok cerr eok eerr ->
      let
        cok1 x s1 e1 =
          let
            eok2 y s2 e2 = cok y s2 (mergeErrors e1 e2)
            eerr2 e2 = cerr (mergeErrors e1 e2)
          in
          case callback x of
            Parser parser2 -> parser2 s1 cok cerr eok2 eerr2

        eok1 x s1 e1 =
          let
            eok2 y s2 e2 = eok y s2 (mergeErrors e1 e2)
            eerr2 e2 = eerr (mergeErrors e1 e2)
          in
          case callback x of
            Parser parser2 -> parser2 s1 cok cerr eok2 eerr2
      in
        parser state cok1 cerr eok1 eerr

-- run :: Parser a -> B.ByteString -> Either Error a
-- run parser bytes
--   = runAt 1 1 parser bytes

run :: B.ByteString -> Either Error Program
run bytes
  = runAt 1 1 program bytes

runAt :: Int -> Int -> Parser a -> B.ByteString -> Either Error a
runAt startRow startColumn (Parser parser) (B.PS fp offset len)
  = case parser (State fp offset (offset + len) 0 startRow startColumn []) Ok Err Ok Err of

      Ok value _ _ ->
        Right value

      Err (ParseError row col problem) ->
        let
          pos
            = Position row col
          mkError overallRegion subRegion
            = Left $ Parse overallRegion subRegion problem
        in
          case problem of

            -- BadChar endCol ->
            --   mkError (Region pos (Position row endCol)) Nothing

            -- BadChar endCol ->
            --   mkError (Region pos (Position row endCol)) Nothing

            -- BadEscape width _ ->
            --   mkError (Region pos (Position row (col + width))) Nothing

            -- BadUnderscore badCol ->
            --   mkError (Region pos (Position row badCol)) Nothing

            -- BadOp _ ((_, start) : _) ->
            --   mkError (Region start pos) (Just (Region pos pos))

            -- Theories ((_, start) : _) _ ->
            --   mkError (Region start pos) (Just (Region pos pos))

            _ ->
              mkError (Region pos pos) Nothing

unsafeIndex :: ForeignPtr Word8 -> Int -> Word8
unsafeIndex fp offset =
  B.accursedUnutterablePerformIO $
    do  word <- peekByteOff (unsafeForeignPtrToPtr fp) offset
        touchForeignPtr fp
        return word

isWord :: ForeignPtr Word8 -> Int -> Int -> Word8 -> Bool
isWord fp offset terminal word =
  offset < terminal && unsafeIndex fp offset == word

getCharWidth :: ForeignPtr Word8 -> Int -> Int -> Word8 -> Int
getCharWidth _fp _offset _terminal word
  | word < 0x80 = 1
  | word < 0xc0 = error "Need UTF-8 encoded input. Ran into unrecognized bits."
  | word < 0xe0 = 2
  | word < 0xf0 = 3
  | word < 0xf8 = 4
  | True = error "Need UTF-8 encoded input. Ran into unrecognized bits."

isSubstring :: ForeignPtr Word8 -> Int -> Int -> ForeignPtr Word8 -> Int -> Int -> Bool
isSubstring subFp subOffset subLength fp offset terminal =
  offset + subLength <= terminal
  &&
  equals fp offset subFp subOffset subLength

equals :: ForeignPtr Word8 -> Int -> ForeignPtr Word8 -> Int -> Int -> Bool
equals fp1 offset1 fp2 offset2 len =
  B.accursedUnutterablePerformIO $
    withForeignPtr fp1 $ \ptr1 ->
    withForeignPtr fp2 $ \ptr2 ->
      do  i <- B.memcmp (plusPtr ptr1 offset1) (plusPtr ptr2 offset2) (fromIntegral len)
          return $! i == 0

-- Written weird to try to make TCE more likely
isNonNewlineAscii :: ForeignPtr Word8 -> Int -> Int -> Bool
isNonNewlineAscii fp offset terminal =
  if offset < terminal then
    let word = unsafeIndex fp offset in
    if word < 128 && word /= 0x0A {- \n -} then
      isNonNewlineAscii fp (offset + 1) terminal
    else
      False

  else
    True

oneOf :: [Parser a] -> Parser a
oneOf parsers =
  foldr oneOfHelp allTheOptionsFailed parsers

allTheOptionsFailed :: Parser a
allTheOptionsFailed =
  Parser $ \_ _ _ _ eerr ->
    eerr noError

oneOfHelp :: Parser a -> Parser a -> Parser a
oneOfHelp (Parser parser1) (Parser parser2) =
  Parser $ \state cok cerr eok eerr ->
    let
      eerr1 e1 =
        let
          eok2 y s e2 = eok y s (mergeErrors e1 e2)
          eerr2 e2 = eerr (mergeErrors e1 e2)
        in
          parser2 state cok cerr eok2 eerr2
    in
      parser1 state cok cerr eok eerr1

mergeErrors :: ParseError -> ParseError -> ParseError
mergeErrors e1@(ParseError r1 c1 p1) e2@(ParseError r2 c2 p2) =
  case compare r1 r2 of
    LT -> e2
    GT -> e1
    EQ ->
      case compare c1 c2 of
        LT -> e2
        GT -> e1
        EQ ->
          case (p1, p2) of
            (Theories _ [], Theories _ _) ->
              e2

            (Theories _ _, Theories _ []) ->
              e1

            (Theories ctx ts1, Theories _ ts2) ->
              ParseError r1 c1 (Theories ctx (ts1 ++ ts2))

            (Theories _ _, _) ->
              e2

            (_, _) ->
              e1
try :: Parser a -> Parser a
try (Parser parser) =
  Parser $ \state cok _ eok eerr ->
    parser state cok eerr eok eerr

deadend :: [Theory] -> Parser a
deadend thrys =
  Parser $ \(State _ _ _ _ row col ctx) _ _ _ eerr ->
    eerr (ParseError row col (Theories ctx thrys))

hint :: Next -> Parser a -> Parser a
hint next (Parser parser) =
  Parser $ \state@(State _ _ _ _ row col ctx) cok cerr eok eerr ->
    let
      eok' x s _ =
        eok x s (expect row col ctx (Expecting next))

      eerr' _ =
        eerr (expect row col ctx (Expecting next))
    in
      parser state cok cerr eok' eerr'

endOfFile :: Parser ()
endOfFile =
  Parser $ \state@(State _ offset terminal _ _ _ _) _ _ eok eerr ->
    if offset < terminal then
      eerr noError
    else
      eok () state noError

-- noFloatsAllowedInPatterns :: Parser a
-- noFloatsAllowedInPatterns =
--   Parser $ \(State _ _ _ _ row col _) _ cerr _ _ ->
--     cerr (ParseError row col FloatInPattern)

-- addLocation :: Parser a -> Parser (A.Located a)
-- addLocation parser =
--   do  start <- getPosition
--       value <- parser
--       end <- getPosition
--       return (A.at start end value)

inContext :: Position -> Context -> Parser a -> Parser a
inContext pos ctx parser =
  do  pushContext pos ctx
      a <- parser
      popContext a

spaces :: Parser ()
spaces =
  checkSpace =<< whitespace

noSpace :: Position -> SPos -> Parser ()
noSpace pos (SPos spos) =
  if pos == spos
    then return ()
    else deadend []

checkSpace :: SPos -> Parser ()
checkSpace (SPos (Position _ col)) =
  do  indent <- getIndent
      if col > indent && col > 1
        then return ()
        else deadend [BadSpace]

checkAligned :: SPos -> Parser ()
checkAligned (SPos (Position _ col)) =
  do  indent <- getIndent
      if col == indent
        then return ()
        else deadend [BadSpace]

checkFreshLine :: SPos -> Parser ()
checkFreshLine (SPos (Position _ col)) =
  if col == 1
    then return ()
else deadend [BadSpace]


-- WHITESPACE -----------------------------------------------------------------

whitespace :: Parser SPos
whitespace =
  Parser $ \(State fp offset terminal indent row col ctx) cok cerr _ _ ->
    case eatSpaces fp offset terminal row col of
      WErr err ->
        cerr err

      WOk newOffset newRow newCol ->
        let
          !spos = SPos (Position newRow newCol)
          !newState = State fp newOffset terminal indent newRow newCol ctx
        in
        cok spos newState noError

data WResult
  = WErr ParseError
  | WOk Int Int Int

eatSpaces :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> WResult
eatSpaces fp offset terminal row col =
  if offset >= terminal then
    WOk offset row col

  else
    case unsafeIndex fp offset of
      0x20 {-   -} ->
        eatSpaces fp (offset + 1) terminal row (col + 1)

      0x0A {- \n -} ->
        eatSpaces fp (offset + 1) terminal (row + 1) 1

      0x7B {- { -} ->
        eatMultiComment fp offset terminal row col

      0x2D {- - -} ->
        let !offset1 = offset + 1 in
        if offset1 < terminal && unsafeIndex fp offset1 == 0x2D {- - -} then
          eatLineComment fp (offset + 2) terminal row (col + 2)
        else
          WOk offset row col

      0x0D {- \r -} ->
        eatSpaces fp (offset + 1) terminal row col

      0x09 {- \t -} ->
        WErr (ParseError row col Tab)

      _ ->
        WOk offset row col

eatLineComment :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> WResult
eatLineComment fp offset terminal row col =
  if offset >= terminal then
    WOk offset row col

  else
    let !word = unsafeIndex fp offset in
    if word == 0x0A {- \n -} then
      eatSpaces fp (offset + 1) terminal (row + 1) 1
    else
      let !newOffset = offset + getCharWidth fp offset terminal word in
      eatLineComment fp newOffset terminal row (col + 1)

eatMultiComment :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> WResult
eatMultiComment fp offset terminal row col =
  let
    !offset1 = offset + 1
    !offset2 = offset + 2
  in
  if offset2 >= terminal then
    WOk offset row col

  else
    let
      !yesDash = unsafeIndex fp offset1 == 0x2D {- - -}
      !noBar   = unsafeIndex fp offset2 /= 0x7C {- | -}
    in
      if yesDash && noBar then
        case eatMultiCommentHelp fp offset2 terminal row (col + 2) 1 of
          WOk newOffset newRow newCol ->
            eatSpaces fp newOffset terminal newRow newCol

          err@(WErr _) ->
            err

      else
        WOk offset row col

eatMultiCommentHelp :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> Int -> WResult
eatMultiCommentHelp fp offset terminal row col openComments =
  if offset >= terminal then
    WErr (ParseError row col EndOfFile_Comment)

  else
    let !word = unsafeIndex fp offset in
    if word == 0x0A {- \n -} then
      eatMultiCommentHelp fp (offset + 1) terminal (row + 1) 1 openComments

    else if word == 0x2D {- - -} && isWord fp (offset + 1) terminal 0x7D {- } -} then
      if openComments == 1 then
        WOk (offset + 2) row (col + 2)
      else
        eatMultiCommentHelp fp (offset + 2) terminal row (col + 2) (openComments - 1)

    else if word == 0x7B {- { -} && isWord fp (offset + 1) terminal 0x2D {- - -} then
      eatMultiCommentHelp fp (offset + 2) terminal row (col + 2) (openComments + 1)

    else
      let !newOffset = offset + getCharWidth fp offset terminal word in
      eatMultiCommentHelp fp newOffset terminal row (col + 1) openComments

-- docComment :: Parser B.ByteString
-- docComment =
--   do  elmDocCommentOpen
--       Parser $ \(State fp offset terminal indent row col ctx) cok cerr _ _ ->
--         case eatMultiCommentHelp fp offset terminal row col 1 of
--           WErr err ->
--             cerr err
-- 
--           WOk newOffset newRow newCol ->
--             let
--               !comment = B.PS fp offset (newOffset - offset - 2)
--               !newState = State fp newOffset terminal indent newRow newCol ctx
--             in
--             cok comment newState noError

chompUntilDocs :: Parser Bool
chompUntilDocs =
  Parser $ \(State fp offset terminal indent row col ctx) cok _ _ _ ->
    let
      (# isStart, newOffset, newRow, newCol #) =
        eatDocs fp offset terminal row col

      !newState =
        State fp newOffset terminal indent newRow newCol ctx
    in
      cok isStart newState noError

eatDocs :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> (# Bool, Int, Int, Int #)
eatDocs fp offset terminal row col =
  if offset >= terminal then
    (# False, offset, row, col #)

  else if isDocsStart fp offset terminal then
    (# True, offset + 5, row, col + 5 #)

  else
    let !word = unsafeIndex fp offset in
    if word == 0x0A {- \n -} then
      eatDocs fp (offset + 1) terminal (row + 1) 1

    else
      let !newOffset = offset + getCharWidth fp offset terminal word in
      eatDocs fp newOffset terminal row (col + 1)

isDocsStart :: ForeignPtr Word8 -> Int -> Int -> Bool
isDocsStart =
  let (B.PS dfp doff dlen) = docsStart in
  isSubstring dfp doff dlen

docsStart :: B.ByteString
docsStart =
  "@docs"
  

-- PROGRAM --------------------------------------------------------------------

data Program
  = ProgramTODO

program :: Parser Program
program =
  hint Expr $
  do  start <- getPosition
      oneOf
        [
        ]
        -- [ let_ start
        -- , if_ start
        -- , case_ start
        -- , function start
        -- , do  expr <- possiblyNegativeTerm start
        --       end <- getPosition
        --       space <- whitespace
        --       exprHelp start (ExprState [] expr [] end space)
        -- ]
