{-# OPTIONS_GHC -Wno-missing-methods #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}

module Agda
  ( start
  , runAgda
  , sendCommand
  , getCommandLineOptions
  , CommandReq(..)
  , CommandRes(..)
  ) where

import           Prelude                        hiding ( null )

import           Agda.Compiler.Backend          ( parseBackendOptions )
import           Agda.Compiler.Builtin          ( builtinBackends )
import           Agda.Convert                   ( fromResponse )
import           Agda.Interaction.Base          ( Command
                                                , Command'(Command, Done, Error)
#if MIN_VERSION_Agda(2,7,0)
#else
                                                , CommandM
#endif
                                                , CommandState(optionsOnReload)
                                                , IOTCM
                                                , initCommandState
                                                , parseIOTCM
                                                )
#if MIN_VERSION_Agda(2,6,4)
import           Agda.Syntax.Common.Pretty      ( render, vcat )
#endif
import           Agda.Interaction.InteractionTop
                                                ( initialiseCommandQueue
                                                , maybeAbort
                                                , runInteraction
#if MIN_VERSION_Agda(2,7,0)
                                                , CommandM
#else
#endif
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
import           Agda.TypeChecking.Monad        ( HasOptions
                                                , TCErr
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
import           Agda.Utils.Null                ( null )
import           Agda.VersionCommit             ( versionWithCommitInfo )
import           Control.Exception              ( SomeException
                                                , catch
                                                )
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Aeson                     ( FromJSON
                                                , ToJSON(toJSON)
                                                , Value
                                                , fromJSON
                                                )
import qualified Data.Aeson                    as JSON
import           Data.Maybe                     ( listToMaybe )
import           Data.Text                      ( pack )
import           GHC.Generics                   ( Generic )
import           Language.LSP.Server            ( getConfig )
import           Monad
import           Options                        ( Config(configRawAgdaOptions)
                                                , Options(optRawAgdaOptions)
                                                , versionNumber
                                                )

getAgdaVersion :: String
getAgdaVersion = versionWithCommitInfo

start :: ServerM IO ()
start = do
  env <- ask

  writeLog "[Agda] interaction start"

  result <- runAgda $ do
    -- decides how to output Response
    lift $ setInteractionOutputCallback $ \response -> do
      reaction <- fromResponse response
      sendResponse env reaction

    -- keep reading command
    commands <- liftIO $ initialiseCommandQueue (readCommand env)

    -- get command line options
    options  <- getCommandLineOptions

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

--------------------------------------------------------------------------------

-- | Convert "CommandReq" to "CommandRes"

sendCommand :: MonadIO m => Value -> ServerM m Value
sendCommand value = do
    -- JSON Value => Request => Response
  case fromJSON value of
    JSON.Error msg ->
      return
        $  toJSON
        $  CmdRes
        $  Just
        $  CmdErrCannotDecodeJSON
        $  show msg
        ++ "\n"
        ++ show value
    JSON.Success request -> toJSON <$> handleCommandReq request


handleCommandReq :: MonadIO m => CommandReq -> ServerM m CommandRes
handleCommandReq CmdReqSYN    = return $ CmdResACK Agda.getAgdaVersion versionNumber
handleCommandReq (CmdReq cmd) = do
  case parseIOTCM cmd of
    Left err -> do
      writeLog $ "[Error] CmdErrCannotParseCommand:\n" <> pack err
      return $ CmdRes (Just (CmdErrCannotParseCommand err))
    Right iotcm -> do
      writeLog $ "[Request] " <> pack (show cmd)
      provideCommand iotcm
      return $ CmdRes Nothing

--------------------------------------------------------------------------------

getCommandLineOptions
  :: (HasOptions m, MonadIO m) => ServerM m CommandLineOptions
getCommandLineOptions = do
  -- command line options from ARGV
  argv   <- asks (optRawAgdaOptions . envOptions)
  -- command line options from agda-mode
  config <- asks (configRawAgdaOptions . envConfig)
  -- concatenate both
  let merged = argv <> config

  result <- runExceptT $ do
    let p = parseBackendOptions builtinBackends merged defaultOptions
    let (r, _warns) = runOptM p
    (bs, opts) <- ExceptT $ pure r
    return opts
  case result of
    -- something bad happened, use the default options instead
    Left  _    -> commandLineOptions
    Right opts -> return opts

-- | Run a TCM action in IO and throw away all of the errors
-- TODO: handle the caught errors
runAgda :: MonadIO m => ServerM TCM a -> ServerM m (Either String a)
runAgda p = do
  env <- ask
  let p' = runServerM env p
  liftIO
    $       runTCMTop'
              (                 (Right <$> p')
              `catchError`      handleTCErr
              `catchImpossible` handleImpossible
              )
    `catch` catchException
 where
  handleTCErr :: TCErr -> TCM (Either String a)
  handleTCErr err = do
    s2s <- prettyTCWarnings' =<< getAllWarningsOfTCErr err
    s1  <- prettyError err
    let ss       = filter (not . null) $ s2s ++ [s1]
#if MIN_VERSION_Agda(2,6,4)
    let errorMsg = render $ vcat ss
#else
    let errorMsg = unlines ss
#endif
    return (Left errorMsg)

  handleImpossible :: Impossible -> TCM (Either String a)
  handleImpossible = return . Left . show

  catchException :: SomeException -> IO (Either String a)
  catchException e = return $ Left $ show e

--------------------------------------------------------------------------------

data CommandReq
  = CmdReqSYN -- ^ For client to initiate a 2-way handshake
  | CmdReq String
  deriving (Generic)

instance ToJSON CommandReq
instance FromJSON CommandReq

data CommandRes
  = CmdResACK -- ^ For server to complete a 2-way handshake
      String   -- ^ Version number of Agda
      Int -- ^ Version number of the language server
  | CmdRes -- ^ Response for 'CmdReq'
      (Maybe CommandErr) -- ^ 'Nothing' to indicate success
  deriving (Generic)

instance ToJSON CommandRes

data CommandErr
  = CmdErrCannotDecodeJSON String
  | CmdErrCannotParseCommand String
  deriving (Generic)

instance ToJSON CommandErr
