{-# LANGUAGE CPP #-}

module Server.Handler where

import           Agda                           ( getCommandLineOptions
                                                , runAgda
                                                )
import qualified Agda.IR                       as IR

import           Agda.Interaction.Base          ( CommandQueue(..)
#if MIN_VERSION_Agda(2,7,0)
#else
                                                , CommandM
#endif
                                                , CommandState(optionsOnReload)
                                                , Rewrite(AsIs)
                                                , initCommandState
                                                )
import           Agda.Interaction.BasicOps      ( atTopLevel
                                                , typeInCurrent
                                                )
import           Agda.Interaction.Highlighting.Precise
                                                ( HighlightingInfo )
import qualified Agda.Interaction.Imports      as Imp
import           Agda.Interaction.InteractionTop
                                                ( cmd_load'
                                                , localStateCommandM
#if MIN_VERSION_Agda(2,7,0)
                                                , CommandM
#else
#endif
                                                )
import           Agda.Interaction.Options       ( CommandLineOptions
                                                  ( optAbsoluteIncludePaths
                                                  )
                                                )
import qualified Agda.Parser                   as Parser
import           Agda.Position                  ( makeToOffset
                                                , toAgdaPositionWithoutFile
                                                )
import           Agda.Syntax.Abstract.Pretty    ( prettyATop )
import           Agda.Syntax.Parser             ( exprParser
                                                , parse
                                                )
import           Agda.Syntax.Translation.ConcreteToAbstract
                                                ( concreteToAbstract_ )
import           Agda.TypeChecking.Monad        ( HasOptions(commandLineOptions)
                                                , setInteractionOutputCallback
                                                )
import           Agda.TypeChecking.Warnings     ( runPM )
#if MIN_VERSION_Agda(2,6,4)
import           Agda.Syntax.Common.Pretty      ( render )
#else
import           Agda.Utils.Pretty              ( render )
#endif
import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import qualified Data.Text                     as Text
import           Language.LSP.Server            ( LspM )
import qualified Language.LSP.Server           as LSP
import qualified Language.LSP.Protocol.Types   as LSP
import qualified Language.LSP.VFS              as VFS
import           Monad
import           Options                        ( Config
                                                , Options(optRawAgdaOptions)
                                                )

initialiseCommandQueue :: IO CommandQueue
initialiseCommandQueue = CommandQueue <$> newTChanIO <*> newTVarIO Nothing

runCommandM :: CommandM a -> ServerM (LspM Config) (Either String a)
runCommandM program = do
  env <- ask
  runAgda $ do
    -- get command line options
    options <- getCommandLineOptions

    -- we need to set InteractionOutputCallback else it would panic
    lift $ setInteractionOutputCallback $ \_response -> return ()

    -- setup the command state
    commandQueue <- liftIO initialiseCommandQueue
    let commandState = (initCommandState commandQueue)
          { optionsOnReload = options { optAbsoluteIncludePaths = [] }
          }

    lift $ evalStateT program commandState

inferTypeOfText
  :: FilePath -> Text -> ServerM (LspM Config) (Either String String)
inferTypeOfText filepath text = runCommandM $ do
    -- load first
  cmd_load' filepath [] True Imp.TypeCheck $ \_ -> return ()
  -- infer later
  let norm = AsIs
  -- localStateCommandM: restore TC state afterwards, do we need this here?
  typ <- localStateCommandM $ do
    (e, _attrs) <- lift $ runPM $ parse exprParser (unpack text)
    lift $ atTopLevel $ do
      concreteToAbstract_ e >>= typeInCurrent norm

  render <$> prettyATop typ

onHover :: LSP.Uri -> LSP.Position -> ServerM (LspM Config) (LSP.Hover LSP.|? LSP.Null)
onHover uri pos = do
  result <- LSP.getVirtualFile (LSP.toNormalizedUri uri)
  case result of
    Nothing   -> return $ LSP.InR LSP.Null
    Just file -> do
      let source      = VFS.virtualFileText file
      let offsetTable = makeToOffset source
      let agdaPos     = toAgdaPositionWithoutFile offsetTable pos
      lookupResult <- Parser.tokenAt uri source agdaPos
      case lookupResult of
        Nothing             -> return $ LSP.InR LSP.Null
        Just (_token, text) -> do
          case LSP.uriToFilePath uri of
            Nothing       -> return $ LSP.InR LSP.Null
            Just filepath -> do
              let range = LSP.Range pos pos
              inferResult <- inferTypeOfText filepath text
              case inferResult of
                Left err -> do
                  let content = hoverContent $ "Error: " <> pack err
                  return $ LSP.InL $ LSP.Hover content (Just range)
                Right typeString -> do
                  let content = hoverContent $ pack typeString
                  return $ LSP.InL $ LSP.Hover content (Just range)
  where
      hoverContent =
        LSP.InL . LSP.mkMarkdownCodeBlock "agda-language-server"
--------------------------------------------------------------------------------
-- Helper functions for converting stuff to SemanticTokenAbsolute


fromHighlightingInfo :: IR.HighlightingInfo -> LSP.SemanticTokenAbsolute
fromHighlightingInfo (IR.HighlightingInfo start end aspects isTokenBased note defSrc)
  = LSP.SemanticTokenAbsolute 1 1 3 kw []
  where
    kw = LSP.SemanticTokenTypes_Keyword

-- HighlightingInfo
--       Int -- starting offset
--       Int -- ending offset
--       [String] -- list of names of aspects
--       Bool -- is token based?
--       String -- note
--       (Maybe (FilePath, Int)) -- the defining module of the token and its position in that module

-- toToken
--   :: Ranged a
--   => J.SemanticTokenTypes
--   -> [J.SemanticTokenModifiers]
--   -> a
--   -> [J.SemanticTokenAbsolute]
-- toToken types modifiers x =
--   let range = rangeOf x
--   in  [ J.SemanticTokenAbsolute (posLine (rangeStart range) - 1)
--                                 (posCol (rangeStart range) - 1)
--                                 (rangeSpan range)
--                                 types
--                                 modifiers
--       ]
