
{-# LANGUAGE DeriveDataTypeable #-}


-- MODULE ---------------------------------------------------------------------

module Main where


-- IMPORTS --------------------------------------------------------------------

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


-- HELPERS --------------------------------------------------------------------

data Result x e
  = Ok  x
  | Err e


-- AST ------------------------------------------------------------------------

data Ast
  = AstTodo


-- PARSER ---------------------------------------------------------------------

data ParseError
  = PETodo

parseSliver :: String -> Result Sliver ParseError
parseSliver
  = do
  putStrLn "TODO"

runParser :: String -> IO ()
runParser s
  = do
  putStrLn "TODO"
  putStrLn s

  

-- CLI ------------------------------------------------------------------------

-- data Method
--   = Debug
--   | Release
--   | Profile
--   deriving
--     ( CA.Data
--     , CA.Typeable
--     , Show
--     , Eq
--     )

data CLI
    = Eval
      { input :: String
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

-- threadsMsg x
--   = x
--     &= CA.help "Number of threads to use"
--     &= CA.name "j"
--     &= CA.typ "NUM"

eval :: CLI
eval
  = Eval
    { input
      = CA.def
        &= CA.typ "SLIVER"
        &= CA.args
    }
    &= CA.help "Evaluate a sliver."

-- test_ :: CLI
-- test_
--   = Test
--     { threads = threadsMsg CA.def
--     , extra   = CA.def &= CA.typ "ANY" &= CA.args
--     }
--     &= CA.help "Run the test suite"
-- 
-- build :: CLI
-- build
--   = Build
--     { threads
--       = threadsMsg CA.def
--     , method
--       = CA.enum
--         [ Release &= CA.help "Release build"
--         , Debug &= CA.help "Debug build"
--         , Profile &= CA.help "Profile build"
--         ]
--     , files
--       = CA.def &= CA.args
--     }
--     &= CA.help "Build the project"
--     &= CA.auto

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
  case c of
    Eval str -> parseSliver str
