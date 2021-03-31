{-# LANGUAGE OverloadedStrings #-}

module Agda.Misc where

-- import Agda.Syntax.Position (Range' (Range))

import Agda (runTCMPrettyErrors)
import Agda.Interaction.Base (CommandM, CommandState (optionsOnReload), Rewrite (AsIs), initCommandState, CommandQueue(..))
import Agda.Interaction.BasicOps (parseExpr, typeInCurrent, atTopLevel)
import Agda.Interaction.InteractionTop (parseAndDoAtToplevel, localStateCommandM, cmd_load')
import Agda.Interaction.Options (CommandLineOptions (optAbsoluteIncludePaths))
import qualified Agda.Parser as Parser
import Agda.Position (makeOffsetTable, toPositionWithoutFile)
import qualified Agda.Syntax.Abstract as A
import Agda.Syntax.Abstract.Pretty (prettyATop)
import Agda.Syntax.Parser (parse, exprParser)
import Agda.Syntax.Position (Range, getRange)
import Agda.Syntax.Translation.ConcreteToAbstract (concreteToAbstract_)
import Agda.TypeChecking.Monad (TCM, setInputFile, HasOptions (commandLineOptions), setInteractionOutputCallback)
import Agda.TypeChecking.Warnings (runPM)
import Agda.Utils.Pretty (render)
import Common (ServerM, ServerM')
import Control.Monad.Reader
import Control.Monad.State
import Data.Text (Text, pack, unpack)
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.VFS as VFS

import Control.Concurrent.STM
import qualified Agda.Interaction.Imports as Imp

initialiseCommandQueue :: IO CommandQueue
initialiseCommandQueue = CommandQueue <$> newTChanIO <*> newTVarIO Nothing

runCommandM :: ServerM' CommandM a -> LSP.LspT () ServerM (Either String a)
runCommandM program = lift $ runTCMPrettyErrors $ do

    -- we need to set InteractionOutputCallback else it would panic
    lift $ setInteractionOutputCallback $ \response -> return ()

    -- setup the command state  
    commandQueue <- liftIO initialiseCommandQueue
    opts <- commandLineOptions
    let commandState = (initCommandState commandQueue) {optionsOnReload = opts {optAbsoluteIncludePaths = []}}
    -- run the program in TCM 
    mapReaderT (`evalStateT` commandState) program


inferTypeOfText :: Range -> FilePath -> Text -> LSP.LspT () ServerM (Either String String)
inferTypeOfText range filepath text = runCommandM $ lift $ do 
    -- load first 
    cmd_load' filepath [] True Imp.TypeCheck $ \_ -> return ()
    -- infer later
    let norm = AsIs 
    -- localStateCommandM: restore TC state afterwards, do we need this here?
    typ <- localStateCommandM $ do
      e <- lift $ runPM $ parse exprParser (unpack text)
      lift $ atTopLevel $ do
        concreteToAbstract_ e >>= typeInCurrent norm

    render <$> prettyATop typ

onHover :: LSP.Uri -> LSP.Position -> LSP.LspT () ServerM (Maybe LSP.Hover)
onHover uri pos = do
  result <- LSP.getVirtualFile (LSP.toNormalizedUri uri)
  case result of
    Nothing -> return Nothing
    Just file -> do
      let source = VFS.virtualFileText file
      let offsetTable = makeOffsetTable source
      let agdaPos = toPositionWithoutFile offsetTable pos
      lookupResult <- Parser.tokenAt uri source agdaPos
      case lookupResult of
        Nothing -> return Nothing
        Just (token, text) -> do
          case LSP.uriToFilePath uri of 
            Nothing -> return Nothing
            Just filepath -> do 
              let range = LSP.Range pos pos

              inferResult <- inferTypeOfText (getRange token) filepath text
              case inferResult of
                Left error -> do
                  let content = LSP.HoverContents $ LSP.markedUpContent "agda-language-server" ("Error: " <> pack error)
                  return $ Just $ LSP.Hover content (Just range)
                Right typeString -> do
                  let content = LSP.HoverContents $ LSP.markedUpContent "agda-language-server" (pack typeString)
                  return $ Just $ LSP.Hover content (Just range)
