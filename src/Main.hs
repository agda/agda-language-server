{-# LANGUAGE OverloadedStrings   #-}
-- {-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Language.Agda.LSP.Util
-- import Language.Agda.LSP.Core
import Language.Agda.LSP.Core (runner)

import qualified Control.Exception as E
import System.IO
import System.Process
-- import System.Exit

main :: IO ()
main = flip E.catches [] $ do
    flip E.finally finalProc $ do
        startLogging

        args <- getArgs
        logs "hi"
        p <- case path args of
            Nothing -> which (search args)  -- no path specified
            Just p -> return (Just p)
        case p of
            Nothing -> putStrLn "unable to find Agda, please supply a different path to Agda"
            Just filepath -> spawnAgda filepath

        where
            finalProc = do
                logs "k bye"
                stopLogging
-- test :: IO ()
-- test = spawnAgda "/Users/banacorn/.local/bin/bgda"

spawnAgda :: FilePath -> IO ()
spawnAgda filepath = do
    (_, _, _, _) <- createProcess $ (proc filepath ["--interaction"])
        -- {   std_in = CreatePipe
        -- ,   std_out = CreatePipe
        -- }
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering

    runner
