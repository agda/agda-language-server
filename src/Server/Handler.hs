module Server.Handler where

import Agda (runTCMPrettyErrors)
import Agda.Interaction.Base (CommandM, CommandQueue (..), CommandState (optionsOnReload), Rewrite (AsIs), initCommandState)
import Agda.Interaction.BasicOps (atTopLevel, typeInCurrent)
import qualified Agda.Interaction.Imports as Imp
import Agda.Interaction.InteractionTop (cmd_load', localStateCommandM)
import Agda.Interaction.Options (CommandLineOptions (optAbsoluteIncludePaths))
import qualified Agda.Parser as Parser
import Agda.Position (makeOffsetTable, toPositionWithoutFile)
import Agda.Syntax.Abstract.Pretty (prettyATop)
import Agda.Syntax.Parser (exprParser, parse)
import Agda.Syntax.Translation.ConcreteToAbstract (concreteToAbstract_)
import Agda.TypeChecking.Monad (HasOptions (commandLineOptions), setInteractionOutputCallback)
import Agda.TypeChecking.Warnings (runPM)
import Agda.Utils.Pretty (render)
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.State
import Data.Text (Text, pack, unpack)
import Language.LSP.Server (LspM)
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.VFS as VFS
import Monad (ServerM)

initialiseCommandQueue :: IO CommandQueue
initialiseCommandQueue = CommandQueue <$> newTChanIO <*> newTVarIO Nothing

runCommandM :: CommandM a -> IO (Either String a)
runCommandM program = runTCMPrettyErrors $ do
  -- we need to set InteractionOutputCallback else it would panic
  setInteractionOutputCallback $ \_response -> return ()

  -- setup the command state
  commandQueue <- liftIO initialiseCommandQueue
  opts <- commandLineOptions
  let commandState = (initCommandState commandQueue) {optionsOnReload = opts {optAbsoluteIncludePaths = []}}

  evalStateT program commandState

inferTypeOfText :: FilePath -> Text -> ServerM (LspM ()) (Either String String)
inferTypeOfText filepath text = liftIO $
  runCommandM $ do
    -- load first
    cmd_load' filepath [] True Imp.TypeCheck $ \_ -> return ()
    -- infer later
    let norm = AsIs
    -- localStateCommandM: restore TC state afterwards, do we need this here?
    typ <- localStateCommandM $ do
      e <- lift $ runPM $ parse exprParser (unpack text)
      lift $
        atTopLevel $ do
          concreteToAbstract_ e >>= typeInCurrent norm

    render <$> prettyATop typ

onHover :: LSP.Uri -> LSP.Position -> ServerM (LspM ()) (Maybe LSP.Hover)
onHover uri pos = do
  result <- LSP.getVirtualFile (LSP.toNormalizedUri uri)
  case result of
    Nothing -> return Nothing
    Just file -> do
      let source = VFS.virtualFileText file
      let offsetTable = makeOffsetTable source
      let agdaPos = toPositionWithoutFile offsetTable pos
      lookupResult <-
        Parser.tokenAt
          uri
          source
          agdaPos
      case lookupResult of
        Nothing -> return Nothing
        Just (_token, text) -> do
          case LSP.uriToFilePath uri of
            Nothing -> return Nothing
            Just filepath -> do
              let range = LSP.Range pos pos

              inferResult <- inferTypeOfText filepath text
              case inferResult of
                Left err -> do
                  let content = LSP.HoverContents $ LSP.markedUpContent "agda-language-server" ("Error: " <> pack err)
                  return $ Just $ LSP.Hover content (Just range)
                Right typeString -> do
                  let content = LSP.HoverContents $ LSP.markedUpContent "agda-language-server" (pack typeString)
                  return $ Just $ LSP.Hover content (Just range)

onHighlight :: LSP.Uri -> LSP.Position -> ServerM (LspM ()) (Maybe LSP.Hover)
onHighlight uri pos = do
  result <- LSP.getVirtualFile (LSP.toNormalizedUri uri)
  case result of
    Nothing -> return Nothing
    Just file -> do
      let source = VFS.virtualFileText file
      let offsetTable = makeOffsetTable source
      let agdaPos = toPositionWithoutFile offsetTable pos
      lookupResult <-
        Parser.tokenAt
          uri
          source
          agdaPos
      case lookupResult of
        Nothing -> return Nothing
        Just (_token, text) -> do
          case LSP.uriToFilePath uri of
            Nothing -> return Nothing
            Just filepath -> do
              let range = LSP.Range pos pos

              inferResult <- inferTypeOfText filepath text
              case inferResult of
                Left err -> do
                  let content = LSP.HoverContents $ LSP.markedUpContent "agda-language-server" ("Error: " <> pack err)
                  return $ Just $ LSP.Hover content (Just range)
                Right typeString -> do
                  let content = LSP.HoverContents $ LSP.markedUpContent "agda-language-server" (pack typeString)
                  return $ Just $ LSP.Hover content (Just range)
