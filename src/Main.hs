{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase          #-}

module Main (main) where

-- import LSP

import System.Exit
import System.Process
import System.IO
import System.Info (os)
import System.Console.CmdArgs.Implicit hiding (args)

import System.Log.Logger
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter


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

main :: IO ()
main = do

    h <- fileHandler "/tmp/agda-language-server.log" DEBUG >>= \lh -> return $
            setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
    updateGlobalLogger "LSP.Main" (addHandler h)

    debugM "LSP.Main" "This buggy component is buggy"
    warningM "LSP.Main" "Still Buggy"

    removeAllHandlers


    -- args <- cmdArgs defaultArgs
    -- p <- case path args of
    --     Nothing -> which (search args)  -- no path specified
    --     Just p -> return (Just p)
    -- case p of
    --     Nothing -> putStrLn "unable to find Agda, please supply a different path to Agda"
    --     Just filepath -> spawnAgda filepath
    --         -- putStr ">> "
    --         -- line <- getLine
    --         -- print $ " " ++ line
    return ()

spawnAgda :: FilePath -> IO ()
spawnAgda filepath = do
    (Just hin, _, _, _) <- createProcess $ (proc filepath ["--interaction"])
        {   std_in = CreatePipe
        }
    hSetBuffering stdout NoBuffering
    hSetBuffering hin LineBuffering

    -- hPutStrLn hin (load path0)

    -- run (return ()) >>= \case
    --     0 -> exitSuccess
    --     c -> exitWith . ExitFailure $ c
    loop hin
    --
    where   loop hin = do
                s <- getLine

                hPutStrLn hin s
                loop hin
    --
    --         path0 = "\"/Users/banacorn/agda/test/A.agda\""
    --         load path =
    --             "IOTCM " ++ path0 ++ " NonInteractive Indirect ( Cmd_load " ++ path0 ++ " [])"

test :: IO ()
test = spawnAgda "/Users/banacorn/.local/bin/bgda"

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
