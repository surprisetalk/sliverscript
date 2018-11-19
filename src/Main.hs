
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
  , help
  , opt
  , summary
  )

  
-- CLI ------------------------------------------------------------------------

data Sample =
  Sample
  { hello :: String
  } deriving
  ( Show
  , CA.Data
  , CA.Typeable
  )

sample =
  Sample
  { hello = CA.def &= CA.help "World argument" &= CA.opt "world"
  } &= CA.summary "Sample v1"


-- MAIN -----------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "TEST"
  print =<< CA.cmdArgs sample
