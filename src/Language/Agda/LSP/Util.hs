{-# LANGUAGE DeriveDataTypeable #-}

module Language.Agda.LSP.Util
    (   Args(..)
    ,   getArgs

    ,   spawnAgda
    ,   which

    ,   startLogging
    ,   lg
    ,   stopLogging
    )   where

import System.Console.CmdArgs.Implicit hiding (args)
import System.Exit
import System.Info (os)
import System.IO
import System.Process


import System.Log.Logger
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler.Simple (fileHandler)
import System.Log.Handler (setFormatter)

--------------------------------------------------------------------------------
-- | Command line argument stuffs

data Args = Args {
        search :: String    -- the name of Agda executable to search with
    ,   path :: Maybe FilePath    -- the path of Agda executable
    }   deriving (Show, Data, Typeable)

defaultArgs :: Args
defaultArgs = Args {
        search = "agda"
            &= typ "STRING"
            &= help "the name of Agda executable to search with (default: \"agda\")"
    ,   path = Nothing
            &= typFile
            &= help "the path of Agda executable"
    }
    &= program "agda-language-server"
    &= summary "Agda Language Server v0.1.0"

getArgs :: IO Args
getArgs = cmdArgs defaultArgs

--------------------------------------------------------------------------------
-- | Child process spawning stuffs


spawnAgda :: FilePath -> (String -> IO String) -> IO ()
spawnAgda filepath handler = do
    (Just hin, _, _, _) <- createProcess $ (proc filepath ["--interaction"])
        {   std_in = CreatePipe
        }
    hSetBuffering stdout NoBuffering
    hSetBuffering hin LineBuffering
    loop hin
    where   loop hin = do
                inputFromEditor <- getLine
                outputToAgda <- handler inputFromEditor
                hPutStrLn hin outputToAgda
                loop hin

which :: String -> IO (Maybe String)
which prog = do
    (code, sout, _) <- case os of
        "mingw32" -> readProcessWithExitCode "where.exe" [prog] "" -- not tested on Windows yet
        _         -> readProcessWithExitCode "which"     [prog] ""

    case code of
        ExitSuccess   -> return (Just $ sanitize sout)
        ExitFailure _ -> return Nothing

    where
        sanitize = filter (/= '\n')

--------------------------------------------------------------------------------
-- | Logger

startLogging :: IO ()
startLogging = do
    h <- fileHandler "/tmp/agda-language-server.log" DEBUG >>= \lh -> return $
            -- setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
            setFormatter lh (simpleLogFormatter "$msg")
    updateGlobalLogger "LSP" (setLevel DEBUG)
    updateGlobalLogger "LSP" (addHandler h)

lg :: String -> IO ()
lg = infoM "LSP"

stopLogging :: IO ()
stopLogging = removeAllHandlers
