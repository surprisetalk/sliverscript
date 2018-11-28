
{-# LANGUAGE OverloadedStrings, UnboxedTuples, DuplicateRecordFields, Rank2Types #-}


-- MODULE ---------------------------------------------------------------------

module ElmParser where


-- IMPORTS --------------------------------------------------------------------

import Control.Exception ( assert )

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


-- NAME -----------------------------------------------------------------------

type Name
  = Text.Text

fromForeignPtr :: ForeignPtr Word8 -> Int -> Int -> Name
fromForeignPtr fptr offset len
  = Text.decodeUtf8 $ B.PS fptr offset len


-- ERRORS ---------------------------------------------------------------------

data Error
  = CommentOnNothing Region
  | UnexpectedPort Region Name
  | TypeWithBadDefinition Region Name Name
  | TypeWithoutDefinition Region Name
  | Parse Region (Maybe Region) Problem

data ParseError
  = ParseError Int Int Problem
 -- ParseError row col problem

data Problem
  = Tab
  | EndOfFile_Comment
  | EndOfFile_Shader
  | EndOfFile_String
  | EndOfFile_MultiString
  | EndOfFile_Char
  | NewLineInString
  | NewLineInChar
  | BadEscape Int EscapeProblem
  | BadChar Int
  | BadNumberDot Int
  | BadNumberEnd
  | BadNumberExp
  | BadNumberHex
  | BadNumberZero
  | FloatInPattern
  | BadShader Text.Text
  | BadUnderscore Int
  | BadOp BadOp ContextStack
  | Theories ContextStack [Theory]

data EscapeProblem
  = UnknownEscape
  | UnicodeSyntax
  | UnicodeRange
  | UnicodeLength Int String

data BadOp
  = HasType
  | Equals
  | Arrow
  | Pipe
  | Dot

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


-- CONTEXT --------------------------------------------------------------------

data Context
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


-- ANNOTATION -----------------------------------------------------------------

data Located a =
  At Region a

at :: Position -> Position -> a -> Located a
at start end value =
  At (Region start end) value

-- merge :: Located a -> Located b -> value -> Located value
-- merge (At region1 _) (At region2 _) value =
--   At (merge region1 region2) value

map :: (a -> b) -> Located a -> Located b
map f (At info value) =
  At info (f value)

toValue :: Located a -> a
toValue (At _ value) =
  value

toRegion :: Located a -> Region
toRegion (At region _) =
  region

traverse :: (Functor f) => (a -> f b) -> Located a -> f (Located b)
traverse func (At region value) =
  At region <$> func value


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

run :: Parser a -> B.ByteString -> Either Error a
run parser bytes
  = runAt 1 1 parser bytes

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


type SParser a =
  Parser (a, Position, SPos)
  

-- WHITESPACE -----------------------------------------------------------------

newtype SPos = SPos Position

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

docComment :: Parser B.ByteString
docComment =
  do  elmDocCommentOpen
      Parser $ \(State fp offset terminal indent row col ctx) cok cerr _ _ ->
        case eatMultiCommentHelp fp offset terminal row col 1 of
          WErr err ->
            cerr err

          WOk newOffset newRow newCol ->
            let
              !comment = B.PS fp offset (newOffset - offset - 2)
              !newState = State fp newOffset terminal indent newRow newCol ctx
            in
            cok comment newState noError

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
  

-- SYMBOLS --------------------------------------------------------------------

underscore :: Parser ()
underscore =
  Parser $ \(State fp offset terminal indent row col ctx) cok cerr _ eerr ->
    if offset == terminal || unsafeIndex fp offset /= 0x5F {- _ -} then
      eerr noError
    else
      let
        !newOffset = offset + 1
        !newCol = col + 1
      in
      if getInnerWidth fp newOffset terminal > 0 then
        let (# _, badCol #) = chompInnerChars fp newOffset terminal newCol in
        cerr (ParseError row newCol (BadUnderscore badCol))
      else
        let !newState = State fp newOffset terminal indent row newCol ctx in
        cok () newState noError

-- binop :: Parser Name
-- binop =
--   Parser $ \(State fp offset terminal indent row col ctx) cok cerr _ eerr ->
--     let !newOffset = chompOps fp offset terminal in
--     if offset == newOffset then
--       eerr (expect row col ctx InfixOp)
-- 
--     else
--       let !length = newOffset - offset in
--       case fromForeignPtr fp offset length of
--         "."  -> cerr (ParseError row col (BadOp Dot ctx))
--         "|"  -> cerr (ParseError row col (BadOp Pipe ctx))
--         "->" -> cerr (ParseError row col (BadOp Arrow ctx))
--         "="  -> cerr (ParseError row col (BadOp Equals ctx))
--         ":"  -> cerr (ParseError row col (BadOp HasType ctx))
--         op   -> cok op (State fp newOffset terminal indent row (col + length) ctx) noError
-- 
-- 
-- chompOps :: ForeignPtr Word8 -> Int -> Int -> Int
-- chompOps fp offset terminal =
--   if offset < terminal && isBinopCharHelp (unsafeIndex fp offset) then
--     chompOps fp (offset + 1) terminal
--   else
--     offset
-- 
-- isBinopCharHelp :: Word8 -> Bool
-- isBinopCharHelp word =
--   word < 128 && Vector.unsafeIndex binopCharVector (fromIntegral word)
-- 
-- binopCharVector :: Vector.Vector Bool
-- binopCharVector =
--   Vector.generate 128 (\i -> IntSet.member i binopCharSet)
-- 
-- binopCharSet :: IntSet.IntSet
-- binopCharSet =
--   IntSet.fromList (map Char.ord "+-/*=.<>:&|^?%!")

{- We can some avoid allocation by declaring all available symbols here.
That means the `symbol` function should only be used within this file on
values tagged as NOINLIN
-}
symbol :: B.ByteString -> Parser ()
symbol sym@(B.PS symFp symOffset symLength) =
  let
    !theory =
      assert
        (isNonNewlineAscii symFp symOffset (symOffset + symLength))
        (Symbol (Char8.unpack sym))
  in
  Parser $ \(State fp offset terminal indent row col ctx) cok _ _ eerr ->
    if isSubstring symFp symOffset symLength fp offset terminal then
      let !newState = State fp (offset + symLength) terminal indent row (col + symLength) ctx in
      cok () newState noError

    else
      eerr (expect row col ctx theory)

-- equals :: Parser ()
-- equals =
--   symbol "="

rightArrow :: Parser ()
rightArrow =
  symbol "->"

hasType :: Parser ()
hasType =
  symbol ":"

comma :: Parser ()
comma =
  symbol ","

pipe :: Parser ()
pipe =
  symbol "|"

cons :: Parser ()
cons =
  symbol "::"

dot :: Parser ()
dot =
  symbol "."

doubleDot :: Parser ()
doubleDot =
  symbol ".."

minus :: Parser ()
minus =
  symbol "-"

lambda :: Parser ()
lambda =
  symbol "\\"

leftParen :: Parser ()
leftParen =
  symbol "("

rightParen :: Parser ()
rightParen =
  symbol ")"

leftSquare :: Parser ()
leftSquare =
  symbol "["

rightSquare :: Parser ()
rightSquare =
  symbol "]"

leftCurly :: Parser ()
leftCurly =
  symbol "{"

rightCurly :: Parser ()
rightCurly =
  symbol "}"

elmDocCommentOpen :: Parser ()
elmDocCommentOpen =
  symbol "{-|"

jsMultiCommentOpen :: Parser ()
jsMultiCommentOpen =
  symbol "/*"

jsMultiCommentClose :: Parser ()
jsMultiCommentClose =
  symbol "*/"

shaderBlockOpen :: Parser ()
shaderBlockOpen =
  symbol "[glsl|"


-- VARIABLES ------------------------------------------------------------------

upper :: Parser Name
upper =
  Parser $ \(State fp offset terminal indent row col ctx) cok _ _ eerr ->
    let (# newOffset, newCol #) = chompUpper fp offset terminal col in
    if offset == newOffset then
      eerr (expect row col ctx CapVar)
    else
      let name = fromForeignPtr fp offset (newOffset - offset) in
      cok name (State fp newOffset terminal indent row newCol ctx) noError

lower :: Parser Name
lower =
  Parser $ \(State fp offset terminal indent row col ctx) cok _ _ eerr ->
    let (# newOffset, newCol #) = chompLower fp offset terminal col in
    if offset == newOffset then
      eerr (expect row col ctx LowVar)
    else
      let name = fromForeignPtr fp offset (newOffset - offset) in
      if Set.member name reservedWords then
        eerr (expect row col ctx LowVar)
      else
        cok name (State fp newOffset terminal indent row newCol ctx) noError

reservedWords :: Set.Set Name
reservedWords =
  Set.fromList
    [ "if", "then", "else"
    , "case", "of"
    , "let", "in"
    , "type"
    , "module", "where"
    , "import", "exposing"
    , "as"
    , "port"
    ]

moduleName :: Parser Name
moduleName =
  Parser $ \(State fp offset terminal indent row col ctx) cok _ _ eerr ->
    let (# newOffset, newCol #) = chompUpper fp offset terminal col in
    case moduleNameHelp fp newOffset terminal newCol of
      Bad badCol ->
        eerr (expect row badCol ctx CapVar)

      Good end endCol ->
        let
          name = fromForeignPtr fp offset (end - offset)
          newState = State fp end terminal indent row endCol ctx
        in
        cok name newState noError

data ModuleName = Bad Int | Good Int Int

moduleNameHelp :: ForeignPtr Word8 -> Int -> Int -> Int -> ModuleName
moduleNameHelp fp offset terminal col =
  if isDot fp offset terminal then
    let
      offset1 = offset + 1
      (# newOffset, newCol #) = chompUpper fp offset1 terminal (col + 1)
    in
    if offset1 == newOffset then
      Bad newCol
    else
      moduleNameHelp fp newOffset terminal newCol

  else
    Good offset col

data Upper
  = Unqualified Name
  | Qualified Name Name

-- foreignUpper :: Parser Upper
-- foreignUpper =
--   Parser $ \(State fp offset terminal indent row col ctx) cok _ _ eerr ->
--     let (# start, end, newCol #) = foreignUpperHelp fp offset terminal col in
--     if start == end then
--       eerr (expect row newCol ctx CapVar)
--     else
--       let
--         newState = State fp end terminal indent row newCol ctx
--         name = fromForeignPtr fp start (end - start)
--         foreign =
--           if start == offset then
--             Unqualified name
--           else
--             let home = fromForeignPtr fp offset ((start - 1) - offset) in
--             Qualified home name
--       in
--       cok foreign newState noError
-- 
-- foreignUpperHelp :: ForeignPtr Word8 -> Int -> Int -> Int -> (# Int, Int, Int #)
-- foreignUpperHelp fp offset terminal col =
--   let
--     (# newOffset, newCol #) = chompUpper fp offset terminal col
--   in
--   if offset == newOffset then
--     (# offset, offset, col #)
-- 
--   else if isDot fp newOffset terminal then
--     foreignUpperHelp fp (newOffset + 1) terminal (newCol + 1)
-- 
--   else
--     (# offset, newOffset, newCol #)

foreignAlpha :: Parser Expr_
foreignAlpha =
  Parser $ \(State fp offset terminal indent row col ctx) cok _ _ eerr ->
    let (# start, end, newCol, varType #) = foreignAlphaHelp fp offset terminal col in
    if start == end then
      eerr (ParseError row newCol (Theories ctx [LowVar, CapVar]))
    else
      let
        newState = State fp end terminal indent row newCol ctx
        name = fromForeignPtr fp start (end - start)
      in
      if start == offset then
        if Set.member name reservedWords then
          eerr noError
        else
          cok (Var varType name) newState noError
      else
        let home = fromForeignPtr fp offset ((start - 1) - offset) in
        cok (VarQual varType home name) newState noError

foreignAlphaHelp :: ForeignPtr Word8 -> Int -> Int -> Int -> (# Int, Int, Int, VarType #)
foreignAlphaHelp fp offset terminal col =
  let
    (# lowerOffset, lowerCol #) = chompLower fp offset terminal col
  in
  if offset < lowerOffset then
    (# offset, lowerOffset, lowerCol, Value #)

  else
    let
      (# upperOffset, upperCol #) = chompUpper fp offset terminal col
    in
    if offset == upperOffset then
      (# offset, offset, col, Ctor #)

    else if isDot fp upperOffset terminal then
      foreignAlphaHelp fp (upperOffset + 1) terminal (upperCol + 1)

    else
      (# offset, upperOffset, upperCol, Ctor #)

isDot :: ForeignPtr Word8 -> Int -> Int -> Bool
isDot fp offset terminal =
  offset < terminal && unsafeIndex fp offset == 0x2e {- . -}

chompUpper :: ForeignPtr Word8 -> Int -> Int -> Int -> (# Int, Int #)
chompUpper fp offset terminal col =
  let width = getUpperWidth fp offset terminal in
  if width == 0 then
    (# offset, col #)
  else
    chompInnerChars fp (offset + width) terminal (col + 1)

getUpperWidth :: ForeignPtr Word8 -> Int -> Int -> Int
getUpperWidth fp offset terminal =
  if offset < terminal then
    getUpperWidthHelp fp offset terminal (unsafeIndex fp offset)
  else
    0

getUpperWidthHelp :: ForeignPtr Word8 -> Int -> Int -> Word8 -> Int
getUpperWidthHelp fp offset terminal word
  | 0x41 {- A -} <= word && word <= 0x5A {- Z -} = 1
  | word < 0xc0 = 0
  | word < 0xe0 = if Char.isUpper (getChar2 fp offset terminal word) then 2 else 0
  | word < 0xf0 = if Char.isUpper (getChar3 fp offset terminal word) then 3 else 0
  | word < 0xf8 = if Char.isUpper (getChar4 fp offset terminal word) then 4 else 0
  | True        = 0

chompLower :: ForeignPtr Word8 -> Int -> Int -> Int -> (# Int, Int #)
chompLower fp offset terminal col =
  let width = getLowerWidth fp offset terminal in
  if width == 0 then
    (# offset, col #)
  else
    chompInnerChars fp (offset + width) terminal (col + 1)

getLowerWidth :: ForeignPtr Word8 -> Int -> Int -> Int
getLowerWidth fp offset terminal =
  if offset < terminal then
    getLowerWidthHelp fp offset terminal (unsafeIndex fp offset)
  else
    0

getLowerWidthHelp :: ForeignPtr Word8 -> Int -> Int -> Word8 -> Int
getLowerWidthHelp fp offset terminal word
  | 0x61 {- a -} <= word && word <= 0x7A {- z -} = 1
  | word < 0xc0 = 0
  | word < 0xe0 = if Char.isLower (getChar2 fp offset terminal word) then 2 else 0
  | word < 0xf0 = if Char.isLower (getChar3 fp offset terminal word) then 3 else 0
  | word < 0xf8 = if Char.isLower (getChar4 fp offset terminal word) then 4 else 0
  | True        = 0

chompInnerChars :: ForeignPtr Word8 -> Int -> Int -> Int -> (# Int, Int #)
chompInnerChars fp offset terminal col =
  let width = getInnerWidth fp offset terminal in
  if width == 0 then
    (# offset, col #)
  else
    chompInnerChars fp (offset + width) terminal (col + 1)

getInnerWidth :: ForeignPtr Word8 -> Int -> Int -> Int
getInnerWidth fp offset terminal =
  if offset < terminal then
    getInnerWidthHelp fp offset terminal (unsafeIndex fp offset)
  else
    0

getInnerWidthHelp :: ForeignPtr Word8 -> Int -> Int -> Word8 -> Int
getInnerWidthHelp fp offset terminal word
  | 0x61 {- a -} <= word && word <= 0x7A {- z -} = 1
  | 0x41 {- A -} <= word && word <= 0x5A {- Z -} = 1
  | 0x30 {- 0 -} <= word && word <= 0x39 {- 9 -} = 1
  | word == 0x5F {- _ -} = 1
  | word < 0xc0 = 0
  | word < 0xe0 = if Char.isAlpha (getChar2 fp offset terminal word) then 2 else 0
  | word < 0xf0 = if Char.isAlpha (getChar3 fp offset terminal word) then 3 else 0
  | word < 0xf8 = if Char.isAlpha (getChar4 fp offset terminal word) then 4 else 0
  | True = 0

push :: Word8 -> Int -> Int
push word code =
  assert (word .&. 0xc0 == 0x80) (
    shiftL code 6 .|. fromEnum (word .&. 0x3f)
  )

getChar2 :: ForeignPtr Word8 -> Int -> Int -> Word8 -> Char
getChar2 fp offset terminal word =
  assert (offset + 2 <= terminal) (
    let
      word1 = word .&. 0x1f
      word2 = unsafeIndex fp (offset + 1)
      code = push word2 (fromEnum word1)
    in
    assert (0x80 <= code) (
      toEnum code
    )
  )

getChar3 :: ForeignPtr Word8 -> Int -> Int -> Word8 -> Char
getChar3 fp offset terminal word =
  assert (offset + 3 <= terminal) (
    let
      word1 = word .&. 0x0f
      word2 = unsafeIndex fp (offset + 1)
      word3 = unsafeIndex fp (offset + 2)
      code = push word3 (push word2 (fromEnum word1))
    in
    assert ((0x800 <= code && code < 0xd800) || (0xdfff < code && code < 0xfffe)) (
      toEnum code
    )
  )

getChar4 :: ForeignPtr Word8 -> Int -> Int -> Word8 -> Char
getChar4 fp offset terminal word =
  assert (offset + 4 <= terminal) (
    let
      word1 = word .&. 0x07
      word2 = unsafeIndex fp (offset + 1)
      word3 = unsafeIndex fp (offset + 2)
      word4 = unsafeIndex fp (offset + 3)
      code = push word4 (push word3 (push word2 (fromEnum word1)))
    in
    assert (0x10000 <= code && code < 0x110000) (
      toEnum code
    )
    )


-- TYPES ----------------------------------------------------------------------

-- term :: Parser Src.Type
-- term =
--   hint E.Type $
--     do  start <- getPosition
--         oneOf
--           [ app0
--           , variable start
--           , tuple start
--           , record start
--           ]
-- 
-- variable :: R.Position -> Parser Src.Type
-- variable start =
--   do  var <- Var.lower
--       end <- getPosition
--       return (A.at start end (Src.TVar var))
-- 
-- expression :: SParser Src.Type
-- expression =
--   hint E.Type $
--   do  start <- getPosition
--       (tipe1, end1, pos1) <-
--         oneOf
--           [ app start
--           , (,,) <$> term <*> getPosition <*> whitespace
--           ]
--       oneOf
--         [ do  checkSpace pos1
--               Symbol.rightArrow
--               spaces
--               (tipe2, end2, pos2) <- expression
--               let tipe = A.at start end2 (Src.TLambda tipe1 tipe2)
--               return ( tipe, end2, pos2 )
--         , return ( tipe1, end1, pos1 )
--         ]
-- 
-- app0 :: Parser Src.Type
-- app0 =
--   do  start <- getPosition
--       upper <- Var.foreignUpper
--       end <- getPosition
--       let region = R.Region start end
--       return $ A.At region $
--         case upper of
--           Var.Unqualified name ->
--             Src.TType region name []
-- 
--           Var.Qualified home name ->
--             Src.TTypeQual region home name []
-- 
-- app :: R.Position -> SParser Src.Type
-- app start =
--   do  upper <- Var.foreignUpper
--       nameEnd <- getPosition
--       namePos <- whitespace
--       (args, end, pos) <- eatArgs [] nameEnd namePos
-- 
--       let region = R.Region start nameEnd
--       let tipe =
--             case upper of
--               Var.Unqualified name ->
--                 Src.TType region name args
-- 
--               Var.Qualified home name ->
--                 Src.TTypeQual region home name args
-- 
--       return ( A.at start end tipe, end, pos )
-- 
-- unionConstructor :: SParser (A.Located N.Name, [Src.Type])
-- unionConstructor =
--   do  start <- getPosition
--       name <- Var.upper
--       nameEnd <- getPosition
--       namePos <- whitespace
--       (args, end, pos) <- eatArgs [] nameEnd namePos
--       return ( (A.at start nameEnd name, args), end, pos )
-- 
-- 
-- eatArgs :: [Src.Type] -> R.Position -> SPos -> SParser [Src.Type]
-- eatArgs args end pos =
--   oneOf
--     [ do  checkSpace pos
--           arg <- term
--           newEnd <- getPosition
--           newSpace <- whitespace
--           eatArgs (arg:args) newEnd newSpace
--     , return ( reverse args, end, pos )
--     ]
-- 
-- tuple :: R.Position -> Parser Src.Type
-- tuple start =
--   do  Symbol.leftParen
--       P.inContext start E.TypeTuple $
--         oneOf
--           [ do  Symbol.rightParen
--                 end <- getPosition
--                 return (A.at start end Src.TUnit)
--           , do  spaces
--                 (tipe, _, pos) <- expression
--                 checkSpace pos
--                 tupleEnding start tipe []
--           ]
-- 
-- tupleEnding :: R.Position -> Src.Type -> [Src.Type] -> Parser Src.Type
-- tupleEnding start firstType revTypes =
--   oneOf
--     [ do  Symbol.comma
--           spaces
--           (tipe, _, pos) <- expression
--           checkSpace pos
--           tupleEnding start firstType (tipe : revTypes)
--     , do  Symbol.rightParen
--           end <- getPosition
--           case reverse revTypes of
--             [] ->
--               return firstType
-- 
--             secondType : otherTypes ->
--               return $ A.at start end $
--                 Src.TTuple firstType secondType otherTypes
--     ]
-- 
-- record :: R.Position -> Parser Src.Type
-- record start =
--   do  Symbol.leftCurly
--       P.inContext start E.TypeRecord $
--         do  spaces
--             oneOf
--               [ do  Symbol.rightCurly
--                     end <- getPosition
--                     return (A.at start end (Src.TRecord [] Nothing))
--               , do  var <- addLocation Var.lower
--                     spaces
--                     oneOf
--                       [ do  Symbol.pipe
--                             spaces
--                             firstField <- field
--                             fields <- chompFields [firstField]
--                             end <- getPosition
--                             return (A.at start end (Src.TRecord fields (Just var)))
--                       , do  Symbol.hasType
--                             spaces
--                             (tipe, _, nextPos) <- expression
--                             checkSpace nextPos
--                             fields <- chompFields [(var, tipe)]
--                             end <- getPosition
--                             return (A.at start end (Src.TRecord fields Nothing))
--                       ]
--               ]
-- 
-- type Field = ( A.Located N.Name, Src.Type )
-- 
-- chompFields :: [Field] -> Parser [Field]
-- chompFields fields =
--   oneOf
--     [ do  Symbol.comma
--           spaces
--           f <- field
--           chompFields (f : fields)
--     , do  Symbol.rightCurly
--           return (reverse fields)
--     ]
-- 
-- field :: Parser Field
-- field =
--   do  name <- addLocation Var.lower
--       spaces
--       Symbol.hasType
--       spaces
--       (tipe, _, endPos) <- expression
--       checkSpace endPos
--       return (name, tipe)  
 

-- NUMBERS --------------------------------------------------------------------

isDirtyEnd :: ForeignPtr Word8 -> Int -> Int -> Word8 -> Bool
isDirtyEnd fp offset terminal word =
  getInnerWidthHelp fp offset terminal word > 0

isDecimalDigit :: Word8 -> Bool
isDecimalDigit word =
  word <= 0x39 {- 9 -} && word >= 0x30 {- 0 -}

data Number
  = NInt Int
  | NFloat Double

nNumber :: Parser Number
nNumber =
  Parser $ \(State fp offset terminal indent row col ctx) cok cerr _ eerr ->
    if offset >= terminal then
      eerr noError

    else
      let !word = unsafeIndex fp offset in
      if not (isDecimalDigit word) then
        eerr noError

      else
        let
          outcome =
            if word == 0x30 {- 0 -} then
              chompZero fp (offset + 1) terminal
            else
              chompInt fp (offset + 1) terminal (fromIntegral (word - 0x30 {- 0 -}))
        in
          case outcome of
            NErr newOffset problem ->
              cerr (ParseError row (col + (newOffset - offset)) problem)

            OkInt newOffset n ->
              let
                !integer = NInt n
                !newState = State fp newOffset terminal indent row (col + (newOffset - offset)) ctx
              in
              cok integer newState noError

            OkFloat newOffset ->
              let
                !length = newOffset - offset
                !float = Float $ read $ Char8.unpack $ B.PS fp offset length
                !newState = State fp newOffset terminal indent row (col + length) ctx
              in
              cok float newState noError

data Outcome
  = NErr { _offset :: Int, _problem :: Problem }
  | OkInt { _offset :: Int, _value :: Int }
  | OkFloat { _offset :: Int }

chompInt :: ForeignPtr Word8 -> Int -> Int -> Int -> Outcome
chompInt fp offset terminal n =
  if offset >= terminal then

    OkInt offset n

  else

    let
      !word = unsafeIndex fp offset
    in
      if isDecimalDigit word then
        chompInt fp (offset + 1) terminal (10 * n + fromIntegral (word - 0x30 {- 0 -}))

      else if word == 0x2E {- . -} then
        chompFraction fp (offset + 1) terminal n

      else if word == 0x65 {- e -} || word == 0x45 {- E -} then
        chompExponent fp (offset + 1) terminal

      else if isDirtyEnd fp offset terminal word then
        NErr offset BadNumberEnd

      else
        OkInt offset n

chompFraction :: ForeignPtr Word8 -> Int -> Int -> Int -> Outcome
chompFraction fp offset terminal n =
  if offset >= terminal then
    NErr offset (BadNumberDot n)

  else if isDecimalDigit (unsafeIndex fp offset) then
    chompFractionHelp fp (offset + 1) terminal

  else
    NErr offset (BadNumberDot n)

chompFractionHelp :: ForeignPtr Word8 -> Int -> Int -> Outcome
chompFractionHelp fp offset terminal =
  if offset >= terminal then
    OkFloat offset

  else
    let !word = unsafeIndex fp offset in
    if isDecimalDigit word then
      chompFractionHelp fp (offset + 1) terminal

    else if word == 0x65 {- e -} || word == 0x45 {- E -} then
      chompExponent fp (offset + 1) terminal

    else if isDirtyEnd fp offset terminal word then
      NErr offset BadNumberEnd

    else
      OkFloat offset

chompExponent :: ForeignPtr Word8 -> Int -> Int -> Outcome
chompExponent fp offset terminal =
  if offset >= terminal then
    NErr offset BadNumberExp

  else
    let !word = unsafeIndex fp offset in
    if isDecimalDigit word then
      chompExponentHelp fp (offset + 1) terminal

    else if word == 0x2B {- + -} || word == 0x2D {- - -} then

      let !offset1 = offset + 1 in
      if offset1 < terminal && isDecimalDigit (unsafeIndex fp offset1) then
        chompExponentHelp fp (offset + 2) terminal
      else
        NErr offset BadNumberExp

    else
      NErr offset BadNumberExp


chompExponentHelp :: ForeignPtr Word8 -> Int -> Int -> Outcome
chompExponentHelp fp offset terminal =
  if offset >= terminal then
    OkFloat offset

  else if isDecimalDigit (unsafeIndex fp offset) then
    chompExponentHelp fp (offset + 1) terminal

  else
    OkFloat offset

chompZero :: ForeignPtr Word8 -> Int -> Int -> Outcome
chompZero fp offset terminal =
  if offset >= terminal then
    OkInt offset 0

  else
    let !word = unsafeIndex fp offset in
    if word == 0x78 {- x -} then
      chompHexInt fp (offset + 1) terminal

    else if word == 0x2E {- . -} then
      chompFraction fp (offset + 1) terminal 0

    else if isDecimalDigit word then
      NErr offset BadNumberZero

    else if isDirtyEnd fp offset terminal word then
      NErr offset BadNumberEnd

    else
      OkInt offset 0

chompHexInt :: ForeignPtr Word8 -> Int -> Int -> Outcome
chompHexInt fp offset terminal =
  let (# newOffset, answer #) = chompHex fp offset terminal in
  if answer < 0 then
    NErr newOffset BadNumberHex
  else
    OkInt newOffset answer

-- Return -1 if it has NO digits
-- Return -2 if it has BAD digits

chompHex :: ForeignPtr Word8 -> Int -> Int -> (# Int, Int #)
chompHex fp offset terminal =
  chompHexHelp fp offset terminal (-1) 0

chompHexHelp :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> (# Int, Int #)
chompHexHelp fp offset terminal answer accumulator =
  if offset >= terminal then
    (# offset, answer #)
  else
    let
      !newAnswer =
        stepHex fp offset terminal (unsafeIndex fp offset) accumulator
    in
    if newAnswer < 0 then
      (# offset, if newAnswer == -1 then answer else -2 #)
    else
      chompHexHelp fp (offset + 1) terminal newAnswer newAnswer

stepHex :: ForeignPtr Word8 -> Int -> Int -> Word8 -> Int -> Int
stepHex fp offset terminal word acc
  | 0x30 {- 0 -} <= word && word <= 0x39 {- 9 -} = 16 * acc + fromIntegral (word - 0x30 {- 0 -})
  | 0x61 {- a -} <= word && word <= 0x66 {- f -} = 16 * acc + 10 + fromIntegral (word - 0x61 {- a -})
  | 0x41 {- A -} <= word && word <= 0x46 {- F -} = 16 * acc + 10 + fromIntegral (word - 0x41 {- A -})
  | isDirtyEnd fp offset terminal word           = -2
  | True                                         = -1

-- precedence :: Parser Binop.Precedence
-- precedence =
--   Parser $ \(State fp offset terminal indent row col ctx) cok _ _ eerr ->
--     if offset >= terminal then
--       eerr noError
-- 
--     else
--       let !word = unsafeIndex fp offset in
--       if isDecimalDigit word then
--         cok
--           (Binop.Precedence (fromIntegral (word - 0x30 {- 0 -})))
--           (State fp (offset + 1) terminal indent row (col + 1) ctx)
--           noError
--       else
--         eerr noError


-- UTF8 -----------------------------------------------------------------------

utf8Character :: Parser Text.Text
utf8Character =
  Parser $ \(State fp offset terminal indent row col ctx) cok cerr _ eerr ->
    if offset >= terminal || unsafeIndex fp offset /= 0x27 {- ' -} then
      eerr noError

    else
      case chompChar fp (offset + 1) terminal (col + 1) 0 "" of
        Utf8Bad newCol problem ->
          cerr (ParseError row newCol problem)

        Utf8Good newOffset newCol numChars mostRecent ->
          if numChars /= 1 then
            cerr (ParseError row col (BadChar newCol))
          else
            let !newState = State fp newOffset terminal indent row newCol ctx in
            cok mostRecent newState noError

data CharResult
  = Utf8Bad Int Problem
  | Utf8Good Int Int Int Text.Text

chompChar :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> Text.Text -> CharResult
chompChar fp offset terminal col numChars mostRecent =
  if offset >= terminal then
    Utf8Bad col EndOfFile_Char

  else
    let
      !word = unsafeIndex fp offset
    in
      if word == 0x27 {- ' -} then
        Utf8Good (offset + 1) (col + 1) numChars mostRecent

      else if word == 0x0A {- \n -} then
        Utf8Bad col NewLineInChar

      else if word == 0x22 {- " -} then
        chompChar fp (offset + 1) terminal (col + 1) (numChars + 1) "\\\""

      else if word == 0x5C {- \ -} then
        case eatEscape fp (offset + 1) terminal of
          EscapeNormal ->
            chompChar fp (offset + 2) terminal (col + 2) (numChars + 1) (toText fp offset 2)

          EscapeUnicode delta bits ->
            chompChar fp (offset + delta) terminal (col + delta) (numChars + 1) (Text.pack bits)

          EscapeProblem newOffset problem ->
            Utf8Bad col (BadEscape (newOffset - offset) problem)

          EscapeEndOfFile ->
            Utf8Bad col EndOfFile_Char

      else
        let !width = getCharWidth fp offset terminal word in
        chompChar fp (offset + width) terminal (col + 1) (numChars + 1) (toText fp offset width)

utf8ToText :: ForeignPtr Word8 -> Int -> Int -> Text.Text
utf8ToText fp offset length =
  Text.decodeUtf8 (B.PS fp offset length)

utf8String :: Parser Text.Text
utf8String =
  Parser $ \(State fp offset terminal indent row col ctx) cok cerr _ eerr ->
    if isDoubleQuote fp offset terminal then

      let
        !offset1 = offset + 1
        result =
          if isDoubleQuote fp offset1 terminal && isDoubleQuote fp (offset + 2) terminal then
            multiString fp (offset + 3) terminal row (col + 3) (offset + 3) mempty
          else
            singleString fp offset1 terminal row (col + 1) offset1 mempty
      in
        case result of
          Utf8Err err ->
            cerr err

          Utf8Ok newOffset newRow newCol builder ->
            let
              !newState = State fp newOffset terminal indent newRow newCol ctx
              !content = LText.toStrict (Text.toLazyText builder)
            in
              cok content newState noError

    else
      eerr noError

isDoubleQuote :: ForeignPtr Word8 -> Int -> Int -> Bool
isDoubleQuote fp offset terminal =
  offset < terminal && unsafeIndex fp offset == 0x22 {- " -}

data StringResult
  = Utf8Err ParseError
  | Utf8Ok !Int !Int !Int Text.Builder

finalize :: ForeignPtr Word8 -> Int -> Int -> Text.Builder -> Text.Builder
finalize fp start end builder =
  if start == end then
    builder
  else
    builder <> Text.fromText (Text.decodeUtf8 (B.PS fp start (end - start)))

addBits :: Text.Builder -> ForeignPtr Word8 -> Int -> Int -> Text.Builder -> Text.Builder
addBits bits fp start end builder =
  if start == end then
    builder <> bits
  else
    builder <> Text.fromText (Text.decodeUtf8 (B.PS fp start (end - start))) <> bits

singleString :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> Int -> Text.Builder -> StringResult
singleString fp offset terminal row col initialOffset builder =
  if offset >= terminal then
    Utf8Err (ParseError row col EndOfFile_String)

  else
    let
      !word = unsafeIndex fp offset
    in
      if word == 0x22 {- " -} then
        Utf8Ok (offset + 1) row (col + 1) $
          finalize fp initialOffset offset builder

      else if word == 0x0A {- \n -} then
        Utf8Err (ParseError row col NewLineInString)

      else if word == 0x27 {- ' -} then
        let !newOffset = offset + 1 in
        singleString fp newOffset terminal row (col + 1) newOffset $
          addBits singleQuoteBits fp initialOffset offset builder

      else if word == 0x5C {- \ -} then
        case eatEscape fp (offset + 1) terminal of
          EscapeNormal ->
            singleString fp (offset + 2) terminal row (col + 2) initialOffset builder

          EscapeUnicode delta bits ->
            let !newOffset = offset + delta in
            singleString fp newOffset terminal row (col + delta) newOffset $
              addBits (Text.fromText (Text.pack bits)) fp initialOffset offset builder

          EscapeProblem newOffset problem ->
            Utf8Err (ParseError row col (BadEscape (newOffset - offset) problem))

          EscapeEndOfFile ->
            Utf8Err (ParseError row (col + 1) EndOfFile_String)

      else
        let !newOffset = offset + getCharWidth fp offset terminal word in
        singleString fp newOffset terminal row (col + 1) initialOffset builder

multiString :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> Int -> Text.Builder -> StringResult
multiString fp offset terminal row col initialOffset builder =
  if offset >= terminal then
    Utf8Err (ParseError row col EndOfFile_MultiString)

  else
    let !word = unsafeIndex fp offset in
    if word == 0x22 {- " -} && isDoubleQuote fp (offset + 1) terminal && isDoubleQuote fp (offset + 2) terminal then
      Utf8Ok (offset + 3) row (col + 3) $
        finalize fp initialOffset offset builder

    else if word == 0x27 {- ' -} then
      let !offset1 = offset + 1 in
      multiString fp offset1 terminal row (col + 1) offset1 $
        addBits singleQuoteBits fp initialOffset offset builder

    else if word == 0x0A {- \n -} then
      let !offset1 = offset + 1 in
      multiString fp offset1 terminal (row + 1) 1 offset1 $
        addBits newlineBits fp initialOffset offset builder

    else if word == 0x0D {- \r -} then
      let !offset1 = offset + 1 in
      multiString fp offset1 terminal (row + 1) 1 offset1 $
        addBits carriageReturnBits fp initialOffset offset builder

    else if word == 0x5C {- \ -} then
      case eatEscape fp (offset + 1) terminal of
        EscapeNormal ->
          multiString fp (offset + 2) terminal row (col + 2) initialOffset builder

        EscapeUnicode delta bits ->
          let !newOffset = offset + delta in
          multiString fp newOffset terminal row (col + delta) newOffset $
            addBits (Text.fromText (Text.pack bits)) fp initialOffset offset builder

        EscapeProblem newOffset problem ->
          Utf8Err (ParseError row col (BadEscape (newOffset - offset) problem))

        EscapeEndOfFile ->
          Utf8Err (ParseError row (col + 1) EndOfFile_MultiString)

    else
      let !newOffset = offset + getCharWidth fp offset terminal word in
      multiString fp newOffset terminal row (col + 1) initialOffset builder

data Escape
  = EscapeNormal
  | EscapeUnicode !Int [Char]
  | EscapeProblem !Int EscapeProblem
  | EscapeEndOfFile

eatEscape :: ForeignPtr Word8 -> Int -> Int -> Escape
eatEscape fp offset terminal =
  if offset >= terminal then
    EscapeEndOfFile

  else
    case unsafeIndex fp offset of
      0x6E {- n -} -> EscapeNormal
      0x72 {- r -} -> EscapeNormal
      0x74 {- t -} -> EscapeNormal
      0x22 {- " -} -> EscapeNormal
      0x27 {- ' -} -> EscapeNormal
      0x5C {- \ -} -> EscapeNormal
      0x75 {- u -} -> eatUnicode fp (offset + 1) terminal
      _            -> EscapeProblem offset UnknownEscape

eatUnicode :: ForeignPtr Word8 -> Int -> Int -> Escape
eatUnicode fp offset terminal =
  if offset >= terminal || unsafeIndex fp offset /= 0x7B {- { -} then
    EscapeProblem offset UnicodeSyntax
  else
    let
      !digitOffset = offset + 1
      (# newOffset, code #) = chompHex fp digitOffset terminal
      !numDigits = newOffset - digitOffset
    in
    if newOffset >= terminal || unsafeIndex fp newOffset /= 0x7D {- } -} then
      EscapeProblem newOffset UnicodeSyntax

    else if code < 0 || 0x10FFFF < code then
      EscapeProblem (newOffset + 1) UnicodeRange

    else if numDigits < 4 || 6 < numDigits then
      EscapeProblem (newOffset + 1) $
        UnicodeLength numDigits (utf8ToString (B.PS fp digitOffset numDigits))

    else
      EscapeUnicode (numDigits + 4) (codePointToBits code)

codePointToBits :: Int -> [Char]
codePointToBits code =
  if code < 0xFFFF then
    wordToBits code

  else
    let
      (hi,lo) = divMod (code - 0x10000) 0x400
    in
    wordToBits (hi + 0xD800) ++ wordToBits (lo + 0xDC00)

wordToBits :: Int -> [Char]
wordToBits code =
  [ '\\' -- 0x5C -- \
  , 'u'  -- 0x75 -- u
  , toBits code 12
  , toBits code 8
  , toBits code 4
  , toBits code 0
  ]

toBits :: Int -> Int -> Char
toBits code offset =
  let !n = fromIntegral (shiftR code offset .&. 0x000F) in
  Char.chr $ if n < 10 then 0x30 + n else 0x61 + (n - 10)

singleQuoteBits :: Text.Builder
singleQuoteBits =
  "\\\'"

newlineBits :: Text.Builder
newlineBits =
  "\\n"

carriageReturnBits :: Text.Builder
carriageReturnBits =
  "\\r"

  
-- KERNEL ---------------------------------------------------------------------

data Special
  = SpecialEnum Word8 Name
  | SpecialProd
  | SpecialDebug
  | SpecialImport Name
  | SpecialJsField Name
  | SpecialElmField Name

chunk :: Parser (B.ByteString, Maybe Special)
chunk =
  Parser $ \(State fp offset terminal indent row col ctx) cok _ _ _ ->
    let
      (# maybeSpecial, jsOffset, newOffset, newRow, newCol #) =
        chompChunk fp offset terminal row col

      javascript = B.PS fp offset (jsOffset - offset)
      newState = State fp newOffset terminal indent newRow newCol ctx
    in
      cok (javascript, maybeSpecial) newState noError

chompChunk :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> (# Maybe Special, Int, Int, Int, Int #)
chompChunk fp offset terminal row col =
  if offset >= terminal then
    (# Nothing, offset, offset, row, col #)

  else
    let word = unsafeIndex fp offset in
    if word == 0x5F {- _ -} then

      let
        offset1 = offset + 1
        offset3 = offset + 3
      in
      if offset3 <= terminal && unsafeIndex fp offset1 == 0x5F {- _ -} then
        chompSpecial fp offset3 terminal row (col + 3) offset
      else
        chompChunk fp offset1 terminal row (col + 1)

    else if word == 0x0A {- \n -} then
      chompChunk fp (offset + 1) terminal (row + 1) 1

    else
      let newOffset = offset + getCharWidth fp offset terminal word in
      chompChunk fp newOffset terminal row (col + 1)

chompSpecial :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> Int -> (# Maybe Special, Int, Int, Int, Int #)
chompSpecial fp offset terminal row col jsOffset =
  let
    (# newOffset, newCol #) =
      chompInnerChars fp offset terminal col

    tagOffset = offset - 1
    word = unsafeIndex fp tagOffset

    special =
      if word == 0x24 {- $ -} then
        SpecialElmField (fromForeignPtr fp offset (newOffset - offset))

      else
        let name = fromForeignPtr fp tagOffset (newOffset - tagOffset) in
        if 0x30 <= word && word <= 0x39 then
          SpecialEnum (fromIntegral (word - 0x30)) name

        else if 0x61 {- a -} <= word && word <= 0x7A {- z -} then
          SpecialJsField name

        -- else if name == "DEBUG" then
        --   Debug

        -- else if name == "PROD" then
        --   Prod

        else
          SpecialImport name
  in
    (# Just special, jsOffset, newOffset, row, newCol #)
  

-- KEYWORDS -------------------------------------------------------------------

kTerm :: Parser Pattern
kTerm =
  hint Pattern $
    do  start <- getPosition
        oneOf
          [ kRecord start
          , kTuple start
          , kList start
          , kTermHelp start
          ]

kTermHelp :: Position -> Parser Pattern
kTermHelp start =
  oneOf
    [
      do  gunderscore
          end <- getPosition
          return (at start end PAnything)
    ,
      do  name <- lower
          end <- getPosition
          return (at start end (PVar name))
    ,
      do  upper <- foreignUpper
          end <- getPosition
          let region = Region start end
          return $ at start end $
            case upper of
              Unqualified name ->
                PCtor region name []

              Qualified home name ->
                PCtorQual region home name []
    ,
      do  number <- number
          end <- getPosition
          case number of
            Int int ->
              return (at start end (PInt int))

            Float _ ->
              noFloatsAllowedInPatterns
    ,
      do  str <- utf8String
          end <- getPosition
          return (at start end (PStr str))
    ,
      do  chr <- utf8Character
          end <- getPosition
          return (at start end (PChr chr))
    ]

kRecord :: Position -> Parser Pattern
kRecord start =
  do  gleftCurly
      inContext start ExprRecord $
        do  spaces
            oneOf
              [ do  var <- addLocation lower
                    spaces
                    kRecordHelp start [var]
              , do  grightCurly
                    end <- getPosition
                    return (at start end (PRecord []))
              ]

kRecordHelp :: Position -> [Located Name] -> Parser Pattern
kRecordHelp start vars =
  oneOf
    [ do  gcomma
          spaces
          var <- addLocation lower
          spaces
          kRecordHelp start (var:vars)
    , do  grightCurly
          end <- getPosition
          return (at start end (PRecord vars))
    ]

kTuple :: Position -> Parser Pattern
kTuple start =
  do  gleftParen
      inContext start ExprTuple $
        do  spaces
            oneOf
              [ do  (pattern, sPos) <- kExpression
                    checkSpace sPos
                    kTupleHelp start pattern []
              , do  grightParen
                    end <- getPosition
                    return (at start end PUnit)
              ]


kTupleHelp :: Position -> Pattern -> [Pattern] -> Parser Pattern
kTupleHelp start firstPattern revPatterns =
  oneOf
    [ do  gcomma
          spaces
          (pattern, sPos) <- kExpression
          checkSpace sPos
          kTupleHelp start firstPattern (pattern : revPatterns)
    , do  grightParen
          case reverse revPatterns of
            [] ->
              return firstPattern

            secondPattern : otherPatterns ->
              do  end <- getPosition
                  return (at start end (PTuple firstPattern secondPattern otherPatterns))
    ]

kList :: Position -> Parser Pattern
kList start =
  do  gleftSquare
      inContext start PatternList $
        do  spaces
            oneOf
              [ do  (pattern, sPos) <- kExpression
                    checkSpace sPos
                    kListHelp start [pattern]
              , do  grightSquare
                    end <- getPosition
                    return (at start end (PList []))
              ]


kListHelp :: Position -> [Pattern] -> Parser Pattern
kListHelp start patterns =
  oneOf
    [ do  gcomma
          spaces
          (pattern, sPos) <- kExpression
          checkSpace sPos
          kListHelp start (pattern:patterns)
    , do  grightSquare
          end <- getPosition
          return (at start end (PList (reverse patterns)))
    ]

kExpression :: Parser (Pattern, SPos)
kExpression =
  hint Pattern $
    do  start <- getPosition
        cTerm <- kExprTerm
        kExprHelp start [] cTerm

kExprHelp :: Position -> [Pattern] -> (Pattern, Position, SPos) -> Parser (Pattern, SPos)
kExprHelp start patterns (pattern, _end, sPos) =
  oneOf
    [ do  checkSpace sPos
          gcons
          spaces
          cTerm <- kExprTerm
          kExprHelp start (pattern:patterns) cTerm
    , do  checkSpace sPos
          as_
          spaces
          nameStart <- getPosition
          name <- lower
          newEnd <- getPosition
          newSpace <- whitespace
          let alias = at nameStart newEnd name
          return
            ( at start newEnd (PAlias (foldl' cons pattern patterns) alias)
            , newSpace
            )
    , return
        ( foldl' cons pattern patterns
        , sPos
        )
    ]

-- cons :: Pattern -> Pattern -> Pattern
-- cons tl@(At (Region _ end) _) hd@(At (Region start _) _) =
--   at start end (PCons hd tl)

kExprTerm :: Parser Pattern
kExprTerm =
  oneOf
    [
      do  start <- getPosition
          upper <- foreignUpper
          end <- getPosition
          kExprTermHelp (Region start end) upper start []
    ,
      do  t@(At (Region _ end) _) <- kTerm
          pos <- whitespace
          return (t, end, pos)
    ]

kExprTermHelp :: Region -> Upper -> Position -> [Pattern] -> Parser Pattern
kExprTermHelp region upper start revArgs =
  do  end <- getPosition
      sPos <- whitespace
      oneOf
        [ do  checkSpace sPos
              arg <- kTerm
              kExprTermHelp region upper start (arg:revArgs)
        , return
            ( at start end $
                case upper of
                  Unqualified name ->
                    PCtor region name (reverse revArgs)

                  Qualified home name ->
                    PCtorQual region home name (reverse revArgs)
            , end
            , sPos
            )
        ]


-- KEYWORDS -------------------------------------------------------------------

keyword :: B.ByteString -> Parser ()
keyword kwd@(B.PS kwdFp kwdOffset kwdLength) =
  let
    theory =
      assert
        (isNonNewlineAscii kwdFp kwdOffset (kwdOffset + kwdLength))
        (Keyword (Char8.unpack kwd))
  in
  Parser $ \(State fp offset terminal indent row col ctx) cok _ _ eerr ->
    if isSubstring kwdFp kwdOffset kwdLength fp offset terminal
      && getInnerWidth fp (offset + kwdLength) terminal == 0
    then
      let
        newState =
          State fp (offset + kwdLength) terminal indent row (col + kwdLength) ctx
      in
        cok () newState noError

    else
      eerr (expect row col ctx theory)


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



-- EXPRESSIONS ----------------------------------------------------------------

type Expr = Located Expr_

data Expr_
  = Chr Text.Text
  | Str Text.Text
  | Int Int
  | Float Double
  | Var VarType Name
  | VarQual VarType Name Name
  | List [Expr]
  | Op Name
  | Negate Expr
  | Binops [(Expr, Located Name)] Expr
  | Lambda [Pattern] Expr
  | Call Expr [Expr]
  | If [(Expr, Expr)] Expr
  | Let [Located Def] Expr
  | Case Expr [(Pattern, Expr)]
  | Accessor Name
  | Access Expr (Located Name)
  | Update (Located Name) [(Located Name, Expr)]
  | Record [(Located Name, Expr)]
  | Unit
  | Tuple Expr Expr [Expr]
  -- | Shader Text Text Shader.Shader

data VarType = Value | Ctor

data Def
  = Annotate Name Type
  | Define (Located Name) [Pattern] Expr
  | Destruct Pattern Expr

type Pattern = Located Pattern_

data Pattern_
  = PAnything
  | PVar Name
  | PRecord [Located Name]
  | PAlias Pattern (Located Name)
  | PUnit
  | PTuple Pattern Pattern [Pattern]
  | PCtor Region Name [Pattern]
  | PCtorQual Region Name Name [Pattern]
  | PList [Pattern]
  | PCons Pattern Pattern
  | PChr Text.Text
  | PStr Text.Text
  | PInt Int

type Type =
    Located Type_

data Type_
  = TLambda Type Type
  | TVar Name
  | TType Region Name [Type]
  | TTypeQual Region Name Name [Type]
  | TRecord [(Located Name, Type)] (Maybe (Located Name))
  | TUnit
  | TTuple Type Type [Type]

type Decl = Located Decl_

data Decl_
  = Union (Located Name) [Located Name] [(Located Name, [Type])]
  | Alias (Located Name) [Located Name] Type
  -- | Binop Name Binop.Associativity Binop.Precedence Name
  | Port (Located Name) Type
  | Docs Text.Text
  | Annotation (Located Name) Type
  | Definition (Located Name) [Pattern] Expr

data Module decls =
  Module (Maybe Header) [Import] decls

data Header
  = Header
      { _name :: Name
      , _effects :: Effects
      , _exports :: Located Exposing
      , _docs :: Docs
      }

data Import =
  Import
    { _import :: Located Name
    , _alias :: Maybe Name
    , _exposing :: Exposing
    }

data Docs
  = NoDocs Region
  | YesDocs Region B.ByteString

data Effects
  = NoEffects
  | Ports Region
  | Manager Region Manager

data Manager
  = Cmd (Located Name)
  | Sub (Located Name)
  | Fx (Located Name) (Located Name)

data Exposing
  = Open
  | Explicit [Located Exposed]

data Exposed
  = Lower Name
  | Upper Name Privacy
  | Operator Name

data Privacy
  = Public
  | Private

term :: Parser Expr
term =
  hint Expr $
  do  start <- getPosition
      oneOf
        [ variable start >>= accessible start
        , string start
        , number start
        , list start
        , record start >>= accessible start
        , tuple start >>= accessible start
        , accessor start
        , character start
        ]


string :: Position -> Parser Expr
string start =
  do  str <- utf8String
      end <- getPosition
      return (at start end (Str str))


character :: Position -> Parser Expr
character start =
  do  chr <- utf8Character
      end <- getPosition
      return (at start end (Chr chr))


number :: Position -> Parser Expr
number start =
  do  nmbr <- nNumber
      end <- getPosition
      return $ at start end $
        case nmbr of
          NInt int ->
            Int int

          NFloat float ->
            Float float


accessor :: Position -> Parser Expr
accessor start =
  do  dot
      field <- lower
      end <- getPosition
      return (at start end (Accessor field))


variable :: Position -> Parser Expr
variable start =
  do  var <- foreignAlpha
      end <- getPosition
      return (at start end var)


accessible :: Position -> Expr -> Parser Expr
accessible start expr =
  oneOf
    [ do  dot
          pos <- getPosition
          field <- lower
          end <- getPosition
          let newExpr = at start end (Access expr (at pos end field))
          accessible start newExpr
    , return expr
    ]

list :: Position -> Parser Expr
list start =
  do  leftSquare
      inContext start ExprList $
        do  spaces
            oneOf
              [ do  (entry, _, pos) <- expression
                    checkSpace pos
                    listHelp start [entry]
              , do  rightSquare
                    end <- getPosition
                    return (at start end (List []))
              ]

listHelp :: Position -> [Expr] -> Parser Expr
listHelp start entries =
  oneOf
    [ do  comma
          spaces
          (entry, _, pos) <- expression
          checkSpace pos
          listHelp start (entry:entries)
    , do  rightSquare
          end <- getPosition
          return (at start end (List (reverse entries)))
    ]

tuple :: Position -> Parser Expr
tuple start =
  do  leftParen
      pos <- getPosition
      spos <- whitespace
      inContext start ExprTuple $ oneOf $
        [ do  noSpace pos spos
              try (minus >> rightParen)
              end <- getPosition
              return $ at start end (Op "-")

        , do  checkSpace spos
              (entry, _, spos2) <- expression
              checkSpace spos2
              tupleHelp start entry []

        , do  noSpace pos spos
              op <- binop
              rightParen
              end <- getPosition
              return $ at start end (Op op)

        , do  noSpace pos spos
              rightParen
              end <- getPosition
              return (at start end Unit)

        ]

tupleHelp :: Position -> Expr -> [Expr] -> Parser Expr
tupleHelp start firstExpr revExprs =
  oneOf
    [ do  comma
          spaces
          (entry, _, pos) <- expression
          checkSpace pos
          tupleHelp start firstExpr (entry : revExprs)
    , do  rightParen
          end <- getPosition
          case reverse revExprs of
            [] ->
              return firstExpr

            secondExpr : others ->
              return (at start end (Tuple firstExpr secondExpr others))
    ]

record :: Position -> Parser Expr
record start =
  do  leftCurly
      inContext start ExprRecord $
        do  spaces
            oneOf
              [ do  rightCurly
                    end <- getPosition
                    return (at start end (Record []))
              , do  starter <- addLocation lower
                    spaces
                    oneOf
                      [ do  pipe
                            spaces
                            firstField <- chompField
                            fields <- chompFields [firstField]
                            end <- getPosition
                            return (at start end (Update starter fields))
                      , do  equals
                            spaces
                            (value, _, nextPos) <- expression
                            checkSpace nextPos
                            fields <- chompFields [(starter, value)]
                            end <- getPosition
                            return (at start end (Record fields))
                      ]
              ]

type Field = ( Located Name, Expr )

chompFields :: [Field] -> Parser [Field]
chompFields fields =
  oneOf
    [ do  comma
          spaces
          f <- chompField
          chompFields (f : fields)
    , do  rightCurly
          return (reverse fields)
    ]

chompField :: Parser Field
chompField =
  do  key <- addLocation lower
      spaces
      equals
      spaces
      (value, _, pos) <- expression
      checkSpace pos
      return (key, value)

type ExprParser =
  Parser Expr

expression :: ExprParser
expression =
  hint Expr $
  do  start <- getPosition
      oneOf
        [ let_ start
        , if_ start
        , case_ start
        , function start
        , do  expr <- possiblyNegativeTerm start
              end <- getPosition
              space <- whitespace
              exprHelp start (ExprState [] expr [] end space)
        ]

data ExprState =
  ExprState
    { _ops  :: ![(Expr, Located Name)]
    , _expr :: !Expr
    , _args :: ![Expr]
    , _end  :: !Position
    , _pos  :: !SPos
    }

exprHelp :: Position -> ExprState -> ExprParser
exprHelp start (ExprState ops expr args end pos) =
  oneOf
    [ -- argument
      hint Arg $
      do  checkSpace pos
          arg <- term
          newEnd <- getPosition
          newPos <- whitespace
          exprHelp start (ExprState ops expr (arg:args) newEnd newPos)

    , -- infix operator
      do  checkSpace pos
          opStart <- getPosition
          opName <- binop
          opEnd <- getPosition
          let op = at opStart opEnd opName
          spos <- whitespace
          hint (AfterOpExpr opName) (checkSpace spos)
          newStart <- getPosition
          hint (AfterOpExpr opName) $
            if "-" == opName && end /= opStart && opEnd == newStart
            then
              -- negative terms
              do  rawTerm <- term
                  newEnd <- getPosition
                  newPos <- whitespace
                  let arg = at opStart newEnd (Negate rawTerm)
                  exprHelp start (ExprState ops expr (arg:args) newEnd newPos)
            else
              oneOf
                [ -- term
                  do  newExpr <- possiblyNegativeTerm newStart
                      newEnd <- getPosition
                      newPos <- whitespace
                      let newOps = (toCall expr args, op) : ops
                      exprHelp start (ExprState newOps newExpr [] newEnd newPos)

                , -- final term
                  do  (newLast, newEnd, newPos) <-
                        oneOf
                          [ let_ newStart
                          , case_ newStart
                          , if_ newStart
                          , function newStart
                          ]
                      let newOps = (toCall expr args, op) : ops
                      let finalExpr = at start newEnd (Binops (reverse newOps) newLast)
                      return ( finalExpr, newEnd, newPos )
                ]

    , -- done
      let finalExpr = toCall expr args in
      case ops of
        [] ->
          return ( finalExpr, end, pos )

        _ ->
          return ( at start end (Binops (reverse ops) finalExpr), end, pos )
    ]

possiblyNegativeTerm :: Position -> Parser Expr
possiblyNegativeTerm start =
  oneOf
    [ do  minus
          expr <- term
          end <- getPosition
          return (at start end (Negate expr))
    , term
    ]

toCall :: Expr -> [Expr] -> Expr
toCall func revArgs =
  case revArgs of
    [] ->
      func

    lastArg : _ ->
      merge func lastArg (Call func (reverse revArgs))

if_ :: Position -> ExprParser
if_ start =
  do  if_
      inContext start ExprIf $ ifHelp start []

ifHelp :: Position -> [(Expr, Expr)] -> ExprParser
ifHelp start branches =
  do  spaces
      (condition, _, condPos) <- expression
      checkSpace condPos
      then_
      spaces
      (thenBranch, _, thenPos) <- expression
      hint ElseBranch $ checkSpace thenPos
      hint ElseBranch $ else_
      spaces
      let newBranches = (condition, thenBranch) : branches
      oneOf
        [ do  if_
              ifHelp start newBranches
        , do  (elseBranch, elseEnd, elseSpace) <- expression
              let ifExpr = at start elseEnd (If (reverse newBranches) elseBranch)
              return ( ifExpr, elseEnd, elseSpace )
        ]

function :: Position -> ExprParser
function start =
  do  lambda
      inContext start ExprFunc $
        do  spaces
            arg <- term
            spaces
            revArgs <- gatherArgs [arg]
            spaces
            (body, end, space) <- expression
            let func = at start end (Lambda (reverse revArgs) body)
            return ( func, end, space )

gatherArgs :: [Pattern] -> Parser [Pattern]
gatherArgs args =
  oneOf
    [ do  arg <- term
          spaces
          gatherArgs (arg:args)
    , do  rightArrow
          return args
    ]

case_ :: Position -> ExprParser
case_ start =
  do  case_
      inContext start ExprCase $
        do  spaces
            (switcher, _, switcherPos) <- expression
            checkSpace switcherPos
            of_
            spaces
            oldIndent <- getIndent
            newIndent <- getCol
            setIndent newIndent
            (firstBranch, firstEnd, firstPos) <- branchHelp
            (branches, end, pos) <- caseHelp [firstBranch] firstEnd firstPos
            setIndent oldIndent
            return
              ( at start end (Case switcher branches)
              , end
              , pos
              )

branchHelp :: Parser (Pattern, Expr)
branchHelp =
  do  (pattern, patternPos) <- expression
      checkSpace patternPos
      rightArrow
      spaces
      (branchExpr, end, pos) <- expression
      return ( (pattern, branchExpr), end, pos )

caseHelp :: [(Pattern, Expr)] -> Position -> SPos -> Parser [(Pattern, Expr)]
caseHelp branches end pos =
  oneOf
    [ do  checkAligned pos
          (branch, newEnd, newPos) <- branchHelp
          caseHelp (branch:branches) newEnd newPos
    , return ( reverse branches, end, pos )
    ]

let_ :: Position -> ExprParser
let_ start =
  do  oldIndent <- getIndent
      letIndent <- getCol
      let_
      pushContext start ExprLet
      setIndent letIndent
      spaces
      defIndent <- getCol
      setIndent defIndent
      (def, end, space) <- letDef
      letHelp start oldIndent [def] end space

letHelp :: Position -> Int -> [Located Def] -> Position -> SPos -> ExprParser
letHelp start oldIndent revDefs end pos =
  oneOf
    [ do  checkAligned pos
          (def, newEnd, newPos) <- letDef
          letHelp start oldIndent (def:revDefs) newEnd newPos

    , do  setIndent oldIndent
          checkSpace pos
          in_
          popContext ()
          spaces
          (body, newEnd, newPos) <- expression
          let letExpr = at start end (Let (reverse revDefs) body)
          return ( letExpr, newEnd, newPos )
    ]

letDef :: Parser (Located Def)
letDef =
  oneOf
    [ definition
    , destructure
    ]

definition :: Parser (Located Def)
definition =
  do  start <- getPosition
      name <- lower
      nameEnd <- getPosition
      spaces
      oneOf
        -- [ do  hasType
        --       inContext start (Annotation name) $
        --         do  spaces
        --             (tipe, end, space) <- Type.expression
        --             return ( at start end (Annotate name tipe), end, space )
        [ inContext start (Definition name) $
            definitionHelp start (at start nameEnd name) []
        ]

definitionHelp :: Position -> Located Name -> [Pattern] -> Parser (Located Def)
definitionHelp start name revArgs =
  oneOf
    [ do  arg <- hint Arg term
          spaces
          definitionHelp start name (arg : revArgs)
    , do  equals
          spaces
          (body, end, space) <- expression
          let def = at start end (Define name (reverse revArgs) body)
          return ( def, end, space )
    ]

destructure :: Parser (Located Def)
destructure =
  do  start <- getPosition
      pattern <- term
      spaces
      equals
      spaces
      (expr, end, space) <- expression
      return ( at start end (Destruct pattern expr), end, space )


