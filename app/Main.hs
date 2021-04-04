module Main where

import Server (run)
import System.Console.GetOpt
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
  (opts, _) <- getArgs >>= parseOpts
  if optHelp opts 
    then putStrLn $ usageInfo usage options
    else do 
      _ <- run (optViaTCP opts)
      return ()

--------------------------------------------------------------------------------

-- | Command-line arguments
data Options = Options
  { optViaTCP :: Maybe Int,
    optHelp :: Bool
  }

defaultOptions :: Options
defaultOptions = Options {optViaTCP = Nothing, optHelp = False}

options :: [OptDescr (Options -> Options)]
options =
  [ Option
      ['h']
      ["help"]
      (NoArg (\opts -> opts {optHelp = True}))
      "print this help message",
   Option 
      ['p']
      ["port"]
      (OptArg (\port opts -> case port of 
                  Just n  -> opts {optViaTCP = readMaybe n}
                  Nothing -> opts {optViaTCP = Just 4096}) "PORT")
      "talk with the editor via TCP port (4096 as default)"
  ]

usage :: String
usage = "Agda Language Server v0.0.3.0 \nUsage: als [Options...]\n"

parseOpts :: [String] -> IO (Options, [String])
parseOpts argv = case getOpt Permute options argv of
  (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
  (_, _, errs) -> ioError $ userError $ concat errs ++ usageInfo usage options
