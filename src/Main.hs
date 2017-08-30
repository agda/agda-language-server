{-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where

import System.Exit
import System.Process
import System.Info (os)
import System.Console.CmdArgs

--------------------------------------------------------------------------------
-- | Command line argument stuffs

data Args = Args {
        search :: String    -- the name of Agda executable to search with
    ,   path :: FilePath    -- the path of Agda executable
    }   deriving (Show, Data, Typeable)

defaultArgs :: Args
defaultArgs = Args {
        search = "agda"
            &= typ "STRING"
            &= help "the name of Agda executable to search with (default: \"agda\")"
    ,   path = def
            &= typFile
            &= help "the path of Agda executable"
    }
    &= program "agda-language-server"
    &= summary "Agda Language Server v0.1.0"

main :: IO ()
main = do
    args <- cmdArgs defaultArgs
    print args
    return ()


which :: FilePath -> IO (Maybe String)
which path = do
    (code, stdout, stderr) <- case os of
        "mingw32" -> readProcessWithExitCode "where.exe" [path] "" -- not tested on Windows yet
        _         -> readProcessWithExitCode "which"     [path] ""

    case code of
        ExitSuccess   -> return (Just $ sanitize stdout)
        ExitFailure _ -> return Nothing

    where
        sanitize = filter (/= '\n')
