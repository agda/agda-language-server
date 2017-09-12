{-# LANGUAGE OverloadedStrings   #-}
-- {-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Language.Agda.LSP.Util
-- import Language.Agda.LSP.Core

import qualified Control.Exception as E

main :: IO ()
main = flip E.catches [] $ do
    flip E.finally finalProc $ do
        startLogging

        args <- getArgs
        lg "hi"
        p <- case path args of
            Nothing -> which (search args)  -- no path specified
            Just p -> return (Just p)
        case p of
            Nothing -> putStrLn "unable to find Agda, please supply a different path to Agda"
            Just filepath -> spawnAgda filepath $ \s -> do
                lg (">>> " ++ s)
                return s

        where
            -- handlers = [ E.Handler ioExcept
            --            , E.Handler someExcept
            --            ]
            finalProc = do
                lg "k bye"
                stopLogging

            -- ioExcept   (e :: E.IOException)       = print e
            -- someExcept (e :: E.SomeException)     = print e

-- test :: IO ()
-- test = spawnAgda "/Users/banacorn/.local/bin/bgda"
