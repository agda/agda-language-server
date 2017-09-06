{-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where

import System.Exit
import System.Process
import System.IO
import System.Info (os)
import System.Console.CmdArgs

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
    args <- cmdArgs defaultArgs
    p <- case path args of
        Nothing -> which (search args)  -- no path specified
        Just p -> return (Just p)
    case p of
        Nothing -> putStrLn "unable to find Agda, please supply a different path to Agda"
        Just p -> spawnAgda p
            -- putStr ">> "
            -- line <- getLine
            -- print $ " " ++ line
    return ()

spawnAgda :: FilePath -> IO ()
spawnAgda path = do
    (Just hin, _, _, procHandle) <- createProcess $ (proc path ["--interaction"])
        {   std_in = CreatePipe
        }
    hSetBuffering stdout NoBuffering
    hSetBuffering hin LineBuffering

    hPutStrLn hin (load path0)

    loop hin
    exitCode <- getProcessExitCode procHandle
    print exitCode

    where   loop hin = do
                getLine >>= hPutStrLn hin
                loop hin

            path0 = "\"/Users/banacorn/agda/test/A.agda\""
            load path =
                "IOTCM " ++ path0 ++ " NonInteractive Indirect ( Cmd_load " ++ path0 ++ " [])"

test = spawnAgda "/Users/banacorn/.local/bin/bgda"

which :: String -> IO (Maybe String)
which name = do
    (code, stdout, stderr) <- case os of
        "mingw32" -> readProcessWithExitCode "where.exe" [name] "" -- not tested on Windows yet
        _         -> readProcessWithExitCode "which"     [name] ""

    case code of
        ExitSuccess   -> return (Just $ sanitize stdout)
        ExitFailure _ -> return Nothing

    where
        sanitize = filter (/= '\n')
