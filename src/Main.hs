
{-# LANGUAGE OverloadedStrings   #-}
-- {-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Language.Agda.LSP.Util
-- import Language.Agda.LSP.Core
-- import Language.Agda.LSP.Core (runner)

import           Control.Concurrent
import qualified Control.Exception as E
import System.IO
import System.Process
-- import System.Exit

main :: IO ()
main = flip E.catches [] $ do
    flip E.finally finalProc $ do
        -- startLogging

        args <- getArgs
        p <- case path args of
            Nothing -> which (search args)  -- no path specified
            Just p -> return (Just p)
        case p of
            Nothing -> putStrLn "unable to find Agda, please supply a different path to Agda"
            Just filepath ->
                putStrLn filepath
              -- runner

                -- spawnAgda filepath

        where
            finalProc = do
                return ()
                -- logs "k bye"
                -- stopLogging
-- test :: IO ()
-- test = spawnAgda "/Users/banacorn/.local/bin/bgda"

spawnAgda' :: FilePath -> IO ()
spawnAgda' filepath = do
    (_, _, _, _) <- createProcess $ (proc filepath ["--interaction"])
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering

spawnAgda :: FilePath -> IO ()
spawnAgda filepath = do
    (Just toAgda, Just fromAgda, _, _) <- createProcess $ (proc filepath ["--interaction"])
        {   std_in = CreatePipe
        ,   std_out = CreatePipe
        }
    hSetBuffering toAgda NoBuffering
    hSetBuffering fromAgda NoBuffering

    putStrLn "spawnAgda"
    -- runner
    --
    -- loop hin
    -- where   loop hin = do
    --             -- request from the editor
    --             reqFromEditor <- getLine
    --             reqToAgda <- reqHandler reqFromEditor
    --             hPutStrLn hin reqToAgda
    --
    --             loop hin
    --         reqHandler s = do
    --             logs $ ">>> " ++ s
    --             return s
