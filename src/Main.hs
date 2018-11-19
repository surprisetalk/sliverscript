
{-# LANGUAGE DeriveDataTypeable #-}


-- MODULE ---------------------------------------------------------------------

module Main where


-- IMPORTS --------------------------------------------------------------------

import qualified Data.ByteString.Internal as B

import qualified Parser as P

import System.Console.CmdArgs as CA
  ( (&=)
  )

import qualified System.Console.CmdArgs as CA
  ( Data
  , Typeable
  , def
  , cmdArgs
  , cmdArgsRun
  , cmdArgsMode
  , modes
  , name
  , def
  , auto
  , args
  , enum
  , typ
  , help
  , opt
  , program
  , summary
  )


-- AST ------------------------------------------------------------------------

data Ast
  = AstTodo
  -- TODO: Implement Show
  deriving
    ( Show
    )


-- PARSER ---------------------------------------------------------------------

parseAst :: P.Parser Ast
parseAst
  = P.Parser
  $ \state cok cerr eok eerr ->
      AstTodo

  
-- CLI ------------------------------------------------------------------------

data CLI
    = Eval
      { input :: B.ByteString
      }
    -- | Test
    --   { threads :: Int
    --   , extra :: [String]
    --   }
    -- | Build
    --   { threads :: Int
    --   , method :: Method
    --   , files :: [FilePath]
    --   }
    deriving
      ( CA.Data
      , CA.Typeable
      , Show
      , Eq
      )

eval :: CLI
eval
  = Eval
    { input
      = CA.def
        &= CA.typ "SLIVER"
        &= CA.args
    }
    &= CA.help "Evaluate a sliver."
    &= CA.auto

cli :: IO CLI
cli
  = CA.cmdArgsRun
  $ CA.cmdArgsMode
  $ CA.modes
    [ eval
    -- , check
    -- , config
    -- , binList
    -- , binAdd
    -- , binRemove
    -- , publish
    -- , compile
    ]
    &= CA.help "TODO"
    &= CA.program "sliverscript"
    &= CA.summary "TODO"


-- MAIN -----------------------------------------------------------------------

main :: IO ()
main
  = do
  c <- cli
  putStrLn "TODO"
  -- case c of
  --   Eval src -> P.run parseAst src
