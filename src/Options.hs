{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}

module Options
  ( Options (..),
    defaultOptions,
    getOptionsFromArgv,
    versionNumber,
    versionString,
    usageMessage,
    Config (..),
    initConfig,
  )
where

import Data.Aeson.Types hiding
  ( Options,
    defaultOptions,
  )
import GHC.Generics (Generic)
import System.Console.GetOpt
import System.Environment (getArgs)
import Text.Read (readMaybe)

getOptionsFromArgv :: IO Options
getOptionsFromArgv = do
  -- extract options for Agda from ARGV
  (argvForALS, argvForAgda) <- extractAgdaOpts <$> getArgs
  -- parse options for ALS
  (opts, _) <- parseOpts argvForALS
  -- save options for Agda back
  return $ opts {optRawAgdaOptions = argvForAgda}

usageMessage :: String
usageMessage = usageInfo usage options ++ usageAboutAgdaOptions

--------------------------------------------------------------------------------

-- | Command-line arguments
data Options = Options
  { optViaTCP :: Maybe Int,
    optRawAgdaOptions :: [String],
    optRawResponses :: Bool,
    optSetup :: Bool,
    optHelp :: Bool,
    optVersion :: Bool,
    optStdin :: Bool
  }

defaultOptions :: Options
defaultOptions =
  Options {optViaTCP = Nothing, optRawAgdaOptions = [], optRawResponses = False, optSetup = False, optHelp = False, optVersion = False, optStdin = False}

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
      ( OptArg
          ( \port opts -> case port of
              Just n -> opts {optViaTCP = readMaybe n}
              Nothing -> opts {optViaTCP = Just 4096}
          )
          "PORT"
      )
      "talk with the editor via TCP port (4096 as default)",
    Option
      []
      ["raw"]
      (NoArg (\opts -> opts {optRawResponses = True}))
      "return all responses in raw JSON format",
#if MIN_VERSION_Agda(2,8,0)
    Option
      []
      ["setup"]
      (NoArg (\opts -> opts {optSetup = True}))
      "run Agda setup and exit",
#endif
    Option
      ['V']
      ["version"]
      (NoArg (\opts -> opts {optVersion = True}))
      "print version information and exit",
    Option
      []
      ["stdio"]
      (NoArg (\opts -> opts {optStdin = True}))
      "connect via stdio"
  ]

versionNumber :: Int
versionNumber = 6

versionString :: String
versionString =
#if MIN_VERSION_Agda(2,8,0)
  "Agda v2.8.0 Language Server v" <> show versionNumber <> suffix
#elif MIN_VERSION_Agda(2,7,0)
  "Agda v2.7.0.1 Language Server v" <> show versionNumber <> suffix
#elif MIN_VERSION_Agda(2,6,4)
  "Agda v2.6.4.3 Language Server v" <> show versionNumber <> suffix
#else
  error "Unsupported Agda version"
#endif
  where
#ifdef wasm32_HOST_ARCH
    suffix = " (WebAssembly build)"
#else
    suffix = ""
#endif

usage :: String
usage = versionString <> "\nUsage: als [Options...]\n"

usageAboutAgdaOptions :: String
usageAboutAgdaOptions = "\n  +AGDA [Options for Agda ...] -AGDA\n    To pass command line options to Agda, put them in between '+AGDA' and '-AGDA'\n    For example:\n      als -p=3000 +AGDA --cubical -AGDA\n    If you are using agda-mode on VS Code, put them in the Settings at:\n      agdaMode.connection.commandLineOptions\n"

parseOpts :: [String] -> IO (Options, [String])
parseOpts argv = case getOpt Permute options argv of
  (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
  (_, _, errs) -> ioError $ userError $ concat errs ++ usageInfo usage options

-- | Extract Agda options (+AGDA ... -AGDA) from a list of options
extractAgdaOpts :: [String] -> ([String], [String])
extractAgdaOpts argv =
  let (before, argv') = break (== "+AGDA") argv
      (forAgda, after) = break (== "-AGDA") argv'
      forALS = before ++ dropWhile (== "-AGDA") after
      forAgda' = dropWhile (== "+AGDA") forAgda
   in (forALS, forAgda')

--------------------------------------------------------------------------------

newtype Config = Config {configRawAgdaOptions :: [String]}
  deriving (Eq, Show, Generic)

instance FromJSON Config where
  parseJSON (Object v) = Config <$> v .: "commandLineOptions"
  -- We do not expect a non-Object value here.
  -- We could use empty to fail, but typeMismatch
  -- gives a much more informative error message.
  parseJSON invalid =
    prependFailure "parsing Config failed, " (typeMismatch "Object" invalid)

initConfig :: Config
initConfig = Config []
