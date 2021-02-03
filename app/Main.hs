module Main where

import Server (run)
import System.Console.GetOpt
import System.Environment (getArgs)

main :: IO ()
main = do
  (opts, _) <- getArgs >>= parseOpts
  case optMode opts of
    ModeHelp -> putStrLn $ usageInfo usage options
    ModeLSP -> do
      _ <- run False 
      return ()
    ModeDev -> do 
      _ <- run True 
      return ()

--------------------------------------------------------------------------------

-- | Command-line arguments
data Mode = ModeLSP | ModeHelp | ModeDev

newtype Options = Options
  { optMode :: Mode
  }

defaultOptions :: Options
defaultOptions = Options {optMode = ModeLSP}

options :: [OptDescr (Options -> Options)]
options =
  [ Option
      ['h']
      ["help"]
      (NoArg (\opts -> opts {optMode = ModeHelp}))
      "print this help message",
    Option
      ['d']
      ["dev"]
      (NoArg (\opts -> opts {optMode = ModeDev}))
      "for development"
  ]

usage :: String
usage = "Agda Language Server v0.0.1 \nUsage: als [Options...]\n"

parseOpts :: [String] -> IO (Options, [String])
parseOpts argv = case getOpt Permute options argv of
  (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
  (_, _, errs) -> ioError $ userError $ concat errs ++ usageInfo usage options
