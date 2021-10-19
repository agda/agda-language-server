module Main where

import           Server                         ( run )
import           System.Console.GetOpt
import           System.Environment             ( getArgs )
import           Text.Read                      ( readMaybe )

main :: IO ()
main = do
  -- extract options for Agda from ARGV 
  (argvForALS         , argvForAgda) <- extractAgdaOpts <$> getArgs
  -- parse options for ALS 
  (optsWithoutAgdaOpts, _          ) <- parseOpts argvForALS
  -- save options for Agda back
  let opts = optsWithoutAgdaOpts { optAgdaOptions = argvForAgda }
  if optHelp opts
    then do 
      let usageInfoAboutAgdaOptions = "\n\
      \  +AGDA [Options for Agda ...] -AGDA\n\
      \    To pass command line options to Agda, put them in between '+AGDA' and '-AGDA'\n\
      \    For example:\n\
      \      als -p=3000 +AGDA --cubical -AGDA\n\
      \"
      putStrLn $ usageInfo usage options ++ usageInfoAboutAgdaOptions
    else do
      _ <- run (optViaTCP opts)
      return ()

--------------------------------------------------------------------------------

-- | Command-line arguments
data Options = Options
  { optViaTCP      :: Maybe Int
  , optAgdaOptions :: [String]
  , optHelp        :: Bool
  }

defaultOptions :: Options
defaultOptions =
  Options { optViaTCP = Nothing, optAgdaOptions = [], optHelp = False }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['h']
           ["help"]
           (NoArg (\opts -> opts { optHelp = True }))
           "print this help message"
  , Option
    ['p']
    ["port"]
    (OptArg
      (\port opts -> case port of
        Just n  -> opts { optViaTCP = readMaybe n }
        Nothing -> opts { optViaTCP = Just 4096 }
      )
      "PORT"
    )
    "talk with the editor via TCP port (4096 as default)"
  ]

usage :: String
usage = "Agda Language Server v0.0.3.0 \nUsage: als [Options...]\n"

parseOpts :: [String] -> IO (Options, [String])
parseOpts argv = case getOpt Permute options argv of
  (o, n, []  ) -> return (foldl (flip id) defaultOptions o, n)
  (_, _, errs) -> ioError $ userError $ concat errs ++ usageInfo usage options


-- | Removes RTS options from a list of options (stolen from Agda)
stripRTS :: [String] -> [String]
stripRTS []               = []
stripRTS ("--RTS" : argv) = argv
stripRTS (arg : argv)
  | is "+RTS" arg = stripRTS $ drop 1 $ dropWhile (not . is "-RTS") argv
  | otherwise     = arg : stripRTS argv
  where is x arg = [x] == take 1 (words arg)

-- | Extract Agda options (+AGDA ... -AGDA) from a list of options
extractAgdaOpts :: [String] -> ([String], [String])
extractAgdaOpts argv =
  let (before , argv') = break (== "+AGDA") argv
      (forAgda, after) = break (== "-AGDA") argv'
      forALS           = before ++ dropWhile (== "-AGDA") after
      forAgda'         = dropWhile (== "+AGDA") forAgda
  in  (forALS, forAgda')
