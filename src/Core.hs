-- {-# LANGUAGE OverloadedStrings #-}

module Core where


import Agda.Interaction.Base (Interaction (..), IOTCM, CommandState (optionsOnReload), initCommandState, Command'(Command))
import qualified Agda.Interaction.Imports as Imp
import Agda.Interaction.Response (Response (..))
import Agda.TypeChecking.Errors (prettyError, prettyTCWarnings')
import Agda.TypeChecking.Monad
    ( TCErr, runTCMTop', commandLineOptions )
import Agda.TypeChecking.Monad.Base (TCM)
import Agda.Utils.Impossible (CatchImpossible (catchImpossible), Impossible)
import Agda.VersionCommit (versionWithCommitInfo)
import Control.Monad.Except (catchError)
import Data.Maybe (listToMaybe)
import Agda.Interaction.Options (CommandLineOptions(optAbsoluteIncludePaths))
import Agda.Interaction.InteractionTop (initialiseCommandQueue, runInteraction, handleCommand_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import Agda.TypeChecking.Monad.State (setInteractionOutputCallback)
import Control.Concurrent
import Control.Exception
import Lispify (lispifyResponse)
import Agda.Utils.Pretty (pretty, render)

import Agda.Interaction.EmacsCommand ()
-- import Control.Exception (catchError)

getAgdaVersion :: String
getAgdaVersion = versionWithCommitInfo



run :: String -> IO String
run raw = do
  let result = parseIOTCM raw
  case result of
    Left err -> return err
    Right command -> do
      result <- runTCMPrettyErrors $ do

        responseMVar <- liftIO newEmptyMVar 


        setInteractionOutputCallback $ \response -> do 
          liftIO $ putMVar responseMVar response
        
        commands <- liftIO $ initialiseCommandQueue (return $ Command command)

        handleCommand_ (lift (return ())) `evalStateT` initCommandState commands

        opts <- commandLineOptions
        let commandState = (initCommandState commands) { optionsOnReload = opts { optAbsoluteIncludePaths = [] } }
        runStateT (runInteraction command) commandState

        response <- liftIO $ readMVar responseMVar
        lispified <- lispifyResponse response
        return $ render $ pretty lispified

      case result of
        Left err -> return err
        Right val -> return val



  -- result <- runTCMPrettyErrors $ do 


  --   case listToMaybe $ reads raw of
  --     Just (x, "")  -> return $ x
  --     Just (_, rem) -> return $ "not consumed: " ++ rem
  --     _             -> return $ "cannot read: " ++ raw

  -- case result of 
  --   Left err -> return err 
  --   Right val -> return val

parseIOTCM :: String -> Either String IOTCM
parseIOTCM raw =
    case listToMaybe $ reads raw of
      Just (x, "")  -> Right x
      Just (_, rem) -> Left $ "not consumed: " ++ rem
      _             -> Left $ "cannot read: " ++ raw



-- TODO: handle the caught errors
-- | Run a TCM action in IO and throw away all of the errors
runTCMPrettyErrors :: TCM String -> IO (Either String String)
runTCMPrettyErrors program = runTCMTop' ((Right <$> program) `catchError` handleTCErr `catchImpossible` handleImpossible) `catch` catchException
  where
    handleTCErr :: TCErr -> TCM (Either String String)
    handleTCErr err = do
      s2s <- prettyTCWarnings' =<< Imp.getAllWarningsOfTCErr err
      s1 <- prettyError err
      let ss = filter (not . null) $ s2s ++ [s1]
      let errorMsg = unlines ss
      return (Left errorMsg)

    handleImpossible :: Impossible -> TCM (Either String String)
    handleImpossible = return . Left . show

    catchException :: SomeException -> IO (Either String String)
    catchException e = return $ Left $ show e