{-# OPTIONS_GHC -Wno-missing-methods #-}

module Agda where

import           Agda.Compiler.Backend          ( parseBackendOptions )
import           Agda.Compiler.Builtin          ( builtinBackends )
import           Agda.Convert                   ( fromResponse )
import           Agda.Interaction.Base          ( Command
                                                , Command'(Command, Done, Error)
                                                , CommandM
                                                , CommandState(optionsOnReload)
                                                , IOTCM
                                                , initCommandState
                                                )
--import qualified Agda.Interaction.Imports as Imp
import           Agda.Interaction.InteractionTop
                                                ( initialiseCommandQueue
                                                , maybeAbort
                                                , runInteraction
                                                )
import           Agda.Interaction.Options       ( CommandLineOptions
                                                  ( optAbsoluteIncludePaths
                                                  )
                                                , defaultOptions
                                                , runOptM
                                                )
import           Agda.TypeChecking.Errors       ( getAllWarningsOfTCErr
                                                , prettyError
                                                , prettyTCWarnings'
                                                )
import           Agda.TypeChecking.Monad        ( TCErr
                                                , commandLineOptions
                                                , runTCMTop'
                                                )
import           Agda.TypeChecking.Monad.Base   ( TCM )
import qualified Agda.TypeChecking.Monad.Benchmark
                                               as Bench
import           Agda.TypeChecking.Monad.State  ( setInteractionOutputCallback )
import           Agda.Utils.FileName            ( absolute )
import           Agda.Utils.Impossible          ( CatchImpossible
                                                  ( catchImpossible
                                                  )
                                                , Impossible
                                                )
import           Agda.VersionCommit             ( versionWithCommitInfo )
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Maybe                     ( listToMaybe )
import           Data.Text                      ( pack )
import           Monad
import           Options                        ( Options(optAgdaOptions) )

getAgdaVersion :: String
getAgdaVersion = versionWithCommitInfo

interact :: ServerM IO ()
interact = do
  env <- ask

  writeLog "[Agda] interaction start"

  result <- mapReaderT runTCMPrettyErrors $ do
    -- decides how to output Response
    lift $ setInteractionOutputCallback $ \response -> do
      reaction <- fromResponse response
      sendResponse env reaction

    -- keep reading command
    commands <- liftIO $ initialiseCommandQueue (readCommand env)

    -- get command line options 
    options  <- do
      result <- liftIO
        $ parseCommandLineOptions (optAgdaOptions (envOptions env))
      case result of
        -- something bad happened, use the default options instead 
        Left  _    -> commandLineOptions
        Right opts -> return opts

    -- start the loop
    let commandState = (initCommandState commands)
          { optionsOnReload = options { optAbsoluteIncludePaths = [] }
          }

    _ <- mapReaderT (`runStateT` commandState) (loop env)

    return ()
  -- TODO: we should examine the result
  case result of
    Left  _err -> return ()
    Right _val -> return ()
 where
  loop :: Env -> ServerM CommandM ()
  loop env = do
    Bench.reset
    done <- Bench.billTo [] $ do
      r <- lift $ maybeAbort runInteraction
      case r of
        Done    -> return True -- Done.
        Error s -> do
          writeLog ("Error " <> pack s)
          return False
        Command _ -> do
          writeLog "[Response] Finished sending, waiting for them to complete"
          waitUntilResponsesSent
          signalCommandFinish
          return False

    lift Bench.print
    unless done (loop env)

  -- Reads the next command from the Channel
  readCommand :: Env -> IO Command
  readCommand env = Command <$> consumeCommand env

parseCommandLineOptions :: [String] -> IO (Either String CommandLineOptions)
parseCommandLineOptions argv = runExceptT $ do
  (bs, opts) <- ExceptT $ runOptM $ parseBackendOptions builtinBackends
                                                        argv
                                                        defaultOptions
  return opts

parseIOTCM :: String -> Either String IOTCM
parseIOTCM raw = case listToMaybe $ reads raw of
  Just (x, ""     ) -> Right x
  Just (_, remnent) -> Left $ "not consumed: " ++ remnent
  _                 -> Left $ "cannot read: " ++ raw

-- TODO: handle the caught errors

-- | Run a TCM action in IO and throw away all of the errors
runTCMPrettyErrors :: TCM a -> IO (Either String a)
runTCMPrettyErrors p =
  runTCMTop'
      ((Right <$> p) `catchError` handleTCErr `catchImpossible` handleImpossible
      )
    `catch` catchException
 where
  handleTCErr :: TCErr -> TCM (Either String a)
  handleTCErr err = do
    s2s <- prettyTCWarnings' =<< getAllWarningsOfTCErr err
    s1  <- prettyError err
    let ss       = filter (not . null) $ s2s ++ [s1]
    let errorMsg = unlines ss
    return (Left errorMsg)

  handleImpossible :: Impossible -> TCM (Either String a)
  handleImpossible = return . Left . show

  catchException :: SomeException -> IO (Either String a)
  catchException e = return $ Left $ show e
