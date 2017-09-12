{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase          #-}

module Main (main) where

import Language.Agda.LSP.Util
-- import Language.Agda.LSP.Core

main :: IO ()
main = do

    startLogging


    args <- getArgs
    p <- case path args of
        Nothing -> which (search args)  -- no path specified
        Just p -> return (Just p)
    case p of
        Nothing -> putStrLn "unable to find Agda, please supply a different path to Agda"
        Just filepath -> spawnAgda filepath $ \s -> do
            lg (">>>" ++ s)
            return s


    stopLogging



    return ()

-- test :: IO ()
-- test = spawnAgda "/Users/banacorn/.local/bin/bgda"
