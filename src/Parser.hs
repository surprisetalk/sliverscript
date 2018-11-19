
{-# LANGUAGE OverloadedStrings, UnboxedTuples, DuplicateRecordFields, Rank2Types #-}


-- MODULE ---------------------------------------------------------------------

module Parser where


-- IMPORTS --------------------------------------------------------------------

import Control.Exception ( assert )

import qualified Data.Set as Set

import Data.Binary (Binary, get, put)

import Data.Bits ((.&.), (.|.), shiftL)

import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Char8 as Char8

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
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



-- AST SOURCE -----------------------------------------------------------------

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
  do  str <- Utf8.string
      end <- getPosition
      return (at start end (Str str))


character :: Position -> Parser Expr
character start =
  do  chr <- Utf8.character
      end <- getPosition
      return (at start end (Chr chr))


number :: Position -> Parser Expr
number start =
  do  nmbr <- Number.number
      end <- getPosition
      return $ at start end $
        case nmbr of
          Number.Int int ->
            Int int

          Number.Float float ->
            Float float


accessor :: Position -> Parser Expr
accessor start =
  do  Symbol.dot
      field <- Var.lower
      end <- getPosition
      return (at start end (Accessor field))


variable :: Position -> Parser Expr
variable start =
  do  var <- Var.foreignAlpha
      end <- getPosition
      return (at start end var)


accessible :: Position -> Expr -> Parser Expr
accessible start expr =
  oneOf
    [ do  Symbol.dot
          pos <- getPosition
          field <- Var.lower
          end <- getPosition
          let newExpr = at start end (Access expr (at pos end field))
          accessible start newExpr
    , return expr
    ]

list :: Position -> Parser Expr
list start =
  do  Symbol.leftSquare
      inContext start ExprList $
        do  spaces
            oneOf
              [ do  (entry, _, pos) <- expression
                    checkSpace pos
                    listHelp start [entry]
              , do  Symbol.rightSquare
                    end <- getPosition
                    return (at start end (List []))
              ]

listHelp :: Position -> [Expr] -> Parser Expr
listHelp start entries =
  oneOf
    [ do  Symbol.comma
          spaces
          (entry, _, pos) <- expression
          checkSpace pos
          listHelp start (entry:entries)
    , do  Symbol.rightSquare
          end <- getPosition
          return (at start end (List (reverse entries)))
    ]

tuple :: Position -> Parser Expr
tuple start =
  do  Symbol.leftParen
      pos <- getPosition
      spos <- W.whitespace
      inContext start ExprTuple $ oneOf $
        [ do  noSpace pos spos
              try (Symbol.minus >> Symbol.rightParen)
              end <- getPosition
              return $ at start end (Op "-")

        , do  checkSpace spos
              (entry, _, spos2) <- expression
              checkSpace spos2
              tupleHelp start entry []

        , do  noSpace pos spos
              op <- Symbol.binop
              Symbol.rightParen
              end <- getPosition
              return $ at start end (Op op)

        , do  noSpace pos spos
              Symbol.rightParen
              end <- getPosition
              return (at start end Unit)

        ]

tupleHelp :: Position -> Expr -> [Expr] -> Parser Expr
tupleHelp start firstExpr revExprs =
  oneOf
    [ do  Symbol.comma
          spaces
          (entry, _, pos) <- expression
          checkSpace pos
          tupleHelp start firstExpr (entry : revExprs)
    , do  Symbol.rightParen
          end <- getPosition
          case reverse revExprs of
            [] ->
              return firstExpr

            secondExpr : others ->
              return (at start end (Tuple firstExpr secondExpr others))
    ]

record :: Position -> Parser Expr
record start =
  do  Symbol.leftCurly
      inContext start ExprRecord $
        do  spaces
            oneOf
              [ do  Symbol.rightCurly
                    end <- getPosition
                    return (at start end (Record []))
              , do  starter <- addLocation Var.lower
                    spaces
                    oneOf
                      [ do  Symbol.pipe
                            spaces
                            firstField <- chompField
                            fields <- chompFields [firstField]
                            end <- getPosition
                            return (at start end (Update starter fields))
                      , do  Symbol.equals
                            spaces
                            (value, _, nextPos) <- expression
                            checkSpace nextPos
                            fields <- chompFields [(starter, value)]
                            end <- getPosition
                            return (at start end (Record fields))
                      ]
              ]

type Field = ( Located N.Name, Expr )

chompFields :: [Field] -> Parser [Field]
chompFields fields =
  oneOf
    [ do  Symbol.comma
          spaces
          f <- chompField
          chompFields (f : fields)
    , do  Symbol.rightCurly
          return (reverse fields)
    ]

chompField :: Parser Field
chompField =
  do  key <- addLocation Var.lower
      spaces
      Symbol.equals
      spaces
      (value, _, pos) <- expression
      checkSpace pos
      return (key, value)

type ExprParser =
  SParser Expr

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
              space <- W.whitespace
              exprHelp start (ExprState [] expr [] end space)
        ]

data ExprState =
  ExprState
    { _ops  :: ![(Expr, Located N.Name)]
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
          newPos <- W.whitespace
          exprHelp start (ExprState ops expr (arg:args) newEnd newPos)

    , -- infix operator
      do  checkSpace pos
          opStart <- getPosition
          opName <- Symbol.binop
          opEnd <- getPosition
          let op = at opStart opEnd opName
          spos <- W.whitespace
          hint (AfterOpExpr opName) (checkSpace spos)
          newStart <- getPosition
          hint (AfterOpExpr opName) $
            if "-" == opName && end /= opStart && opEnd == newStart
            then
              -- negative terms
              do  rawTerm <- term
                  newEnd <- getPosition
                  newPos <- W.whitespace
                  let arg = at opStart newEnd (Negate rawTerm)
                  exprHelp start (ExprState ops expr (arg:args) newEnd newPos)
            else
              oneOf
                [ -- term
                  do  newExpr <- possiblyNegativeTerm newStart
                      newEnd <- getPosition
                      newPos <- W.whitespace
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
    [ do  Symbol.minus
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
  do  Keyword.if_
      inContext start ExprIf $ ifHelp start []

ifHelp :: Position -> [(Expr, Expr)] -> ExprParser
ifHelp start branches =
  do  spaces
      (condition, _, condPos) <- expression
      checkSpace condPos
      Keyword.then_
      spaces
      (thenBranch, _, thenPos) <- expression
      hint ElseBranch $ checkSpace thenPos
      hint ElseBranch $ Keyword.else_
      spaces
      let newBranches = (condition, thenBranch) : branches
      oneOf
        [ do  Keyword.if_
              ifHelp start newBranches
        , do  (elseBranch, elseEnd, elseSpace) <- expression
              let ifExpr = at start elseEnd (If (reverse newBranches) elseBranch)
              return ( ifExpr, elseEnd, elseSpace )
        ]

function :: Position -> ExprParser
function start =
  do  Symbol.lambda
      inContext start ExprFunc $
        do  spaces
            arg <- Pattern.term
            spaces
            revArgs <- gatherArgs [arg]
            spaces
            (body, end, space) <- expression
            let func = at start end (Lambda (reverse revArgs) body)
            return ( func, end, space )

gatherArgs :: [Pattern] -> Parser [Pattern]
gatherArgs args =
  oneOf
    [ do  arg <- Pattern.term
          spaces
          gatherArgs (arg:args)
    , do  Symbol.rightArrow
          return args
    ]

case_ :: Position -> ExprParser
case_ start =
  do  Keyword.case_
      inContext start ExprCase $
        do  spaces
            (switcher, _, switcherPos) <- expression
            checkSpace switcherPos
            Keyword.of_
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

branchHelp :: SParser (Pattern, Expr)
branchHelp =
  do  (pattern, patternPos) <- Pattern.expression
      checkSpace patternPos
      Symbol.rightArrow
      spaces
      (branchExpr, end, pos) <- expression
      return ( (pattern, branchExpr), end, pos )

caseHelp :: [(Pattern, Expr)] -> Position -> SPos -> SParser [(Pattern, Expr)]
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
      Keyword.let_
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
          Keyword.in_
          popContext ()
          spaces
          (body, newEnd, newPos) <- expression
          let letExpr = at start end (Let (reverse revDefs) body)
          return ( letExpr, newEnd, newPos )
    ]

letDef :: SParser (Located Def)
letDef =
  oneOf
    [ definition
    , destructure
    ]

definition :: SParser (Located Def)
definition =
  do  start <- getPosition
      name <- Var.lower
      nameEnd <- getPosition
      spaces
      oneOf
        [ do  Symbol.hasType
              inContext start (Annotation name) $
                do  spaces
                    (tipe, end, space) <- Type.expression
                    return ( at start end (Annotate name tipe), end, space )
        , inContext start (Definition name) $
            definitionHelp start (at start nameEnd name) []
        ]

definitionHelp :: Position -> Located N.Name -> [Pattern] -> SParser (Located Def)
definitionHelp start name revArgs =
  oneOf
    [ do  arg <- hint Arg Pattern.term
          spaces
          definitionHelp start name (arg : revArgs)
    , do  Symbol.equals
          spaces
          (body, end, space) <- expression
          let def = at start end (Define name (reverse revArgs) body)
          return ( def, end, space )
    ]

destructure :: SParser (Located Def)
destructure =
  do  start <- getPosition
      pattern <- Pattern.term
      spaces
      Symbol.equals
      spaces
      (expr, end, space) <- expression
      return ( at start end (Destruct pattern expr), end, space )


-------------------------------------------------------------------------------

slv
  = getCol

