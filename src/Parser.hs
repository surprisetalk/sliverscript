
{-# LANGUAGE DuplicateRecordFields, Rank2Types #-}


-- MODULE ---------------------------------------------------------------------

module Parser where


-- IMPORTS --------------------------------------------------------------------

import qualified Data.ByteString.Internal as B

import qualified Data.Text as Text

import Foreign.ForeignPtr ( ForeignPtr, withForeignPtr ) 
import GHC.Word (Word8)


-- RESULT ---------------------------------------------------------------------

data Result a
  = Ok a State ParseError
  | Err ParseError


-- NAME -----------------------------------------------------------------------

type Name
  = Text.Text


-- ERRORS ---------------------------------------------------------------------

data Error
  = CommentOnNothing Region
  | UnexpectedPort Region Name
  | TypeWithBadDefinition Region Name Name
  | TypeWithoutDefinition Region Name
  | Parse Region (Maybe Region) Problem

data ParseError
  = ParseError Int Int Problem
  -- = ParseError
  --   { row     :: Int
  --   , col     :: Int
  --   , problem :: Problem
  --   }

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


data BadOp = HasType | Equals | Arrow | Pipe | Dot


data Theory
  = Expecting Next
  | Keyword String
  | Symbol String
  | LowVar
  | CapVar
  | InfixOp
  | Digit
  | BadSpace
  deriving (Eq, Ord)


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
  deriving (Eq, Ord)
 
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
  | Definition Name
  | Annotation Name
  ----------------
  | TypeTuple
  | TypeRecord
  ----------------
  | PatternList
  | PatternTuple
  | PatternRecord
  ----------------
  | Module
  | Import
  | TypeUnion
  | TypeAlias
  | Infix
  | Port
  deriving
    ( Eq
    , Ord
    )

type ContextStack
  = [ ( Context, Position ) ]


-- REGIONS --------------------------------------------------------------------

data Region
  = Region
    { start :: Position
    , end   :: Position
    }
  deriving
    ( Eq
    , Ord
    )

data Position
  = Position
    { line   :: Int
    , column :: Int
    }
  deriving
    ( Eq
    , Ord
    )


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
runAt startRow startColumn (Parser parser) (B.PS fp offset length)
  = case parser (State fp offset (offset + length) 0 startRow startColumn []) Ok Err Ok Err of

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

            -- E.BadChar endCol ->
            --   mkError (Region pos (R.Position row endCol)) Nothing

            -- E.BadEscape width _ ->
            --   mkError (Region pos (R.Position row (col + width))) Nothing

            -- E.BadUnderscore badCol ->
            --   mkError (Region pos (R.Position row badCol)) Nothing

            -- E.BadOp _ ((_, start) : _) ->
            --   mkError (Region start pos) (Just (Region pos pos))

            -- E.Theories ((_, start) : _) _ ->
            --   mkError (Region start pos) (Just (Region pos pos))

            _ ->
              mkError (Region pos pos) Nothing


-- STATE ----------------------------------------------------------------------

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
      eok () (state { context = (ctx, pos) : context }) noError


popContext :: a -> Parser a
popContext value
  = Parser
  $ \state@(State _ _ _ _ _ _ context) _ _ eok _ ->
      eok value (state { context = tail context }) noError


setIndent :: Int -> Parser ()
setIndent indent
  = Parser
  $ \state _ _ eok _ ->
      eok () (state { indent = indent }) noError


-- STATE ----------------------------------------------------------------------

data State
  = State
    { source   :: ForeignPtr Word8
    , offset   :: Int
    , terminal :: Int
    , indent   :: Int
    , row      :: Int
    , col      :: Int
    , context  :: ContextStack
    }
