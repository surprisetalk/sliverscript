
{-# LANGUAGE DeriveDataTypeable #-}


-- MODULE ---------------------------------------------------------------------

module Main where


-- IMPORTS --------------------------------------------------------------------

import qualified Data.ByteString as B

import qualified Parser as P

import qualified Options.Applicative as OP
import Data.Semigroup ((<>))



-- AST ------------------------------------------------------------------------

data Ast
  = AstTodo
  -- TODO: Implement Show
  deriving
    ( Show
    )


-- PARSER ---------------------------------------------------------------------

-- parseAst :: P.Parser Ast
parseAst
  = P.getCol


-- EVAL -----------------------------------------------------------------------

eval :: IO B.ByteString -> IO ()
eval io
  = do
  input <- io
  case P.run parseAst input of

      Left _ ->
        putStrLn "TODO: Error"

      Right _ ->
        putStrLn "TODO: AST"


  
-- CLI ------------------------------------------------------------------------

--     -- [ eval
--     -- , check
--     -- , config
--     -- , binList
--     -- , binAdd
--     -- , binRemove
--     -- , publish
--     -- , compile
--     ]

fileInput :: OP.Parser (IO B.ByteString)
fileInput
  = B.readFile
    <$> OP.argument OP.str (OP.metavar "FILENAME")


stdInput :: OP.Parser (IO B.ByteString)
stdInput
  = B.getContents
    <$ OP.flag' ()
       (  OP.long "stdin"
       <> OP.help "Read from stdin"
       )

-- input :: OP.Parser (IO B.ByteString)
-- input
--   = fileInput
--   <> stdInput

cli :: OP.Parser (IO ())
cli
  = OP.subparser
  ( OP.command "eval"
    ( OP.info ( eval <$> fileInput )
      ( OP.progDesc "Evaluate a sliver." )
    )
 -- <> OP.command "commit"
 --    ( OP.info commitOptions
 --      ( OP.progDesc "Record changes to the repository"
 --      )
 --    )
  )

-- digestCLIInput :: (? -> IO ()) -> Input -> IO ()
-- digestCLIInput
--   = 

-- data Sample
--   = Sample
--     { hello      :: String
--     , quiet      :: Bool
--     , enthusiasm :: Int
--     }

-- sample :: OP.Parser Sample
-- sample = Sample
--       <$> OP.strOption
--           ( OP.long "hello"
--          <> OP.metavar "TARGET"
--          <> OP.help "Target for the greeting" )
--       <*> OP.switch
--           ( OP.long "quiet"
--          <> OP.short 'q'
--          <> OP.help "Whether to be quiet" )
--       <*> OP.option OP.auto
--           ( OP.long "enthusiasm"
--          <> OP.help "How enthusiastically to greet"
--          <> OP.showDefault
--          <> OP.value 1
--          <> OP.metavar "INT" )

execCli :: IO ()
execCli = id =<< OP.execParser opts
  where
    opts
      = OP.info (cli OP.<**> OP.helper)
        ( OP.fullDesc
          <> OP.progDesc "Print a greeting for TARGET"
          <> OP.header "hello - a test for optparse-applicative"
        )

-- MAIN -----------------------------------------------------------------------

main :: IO ()
main = execCli

