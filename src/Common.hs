module Common where 

import Control.Concurrent
import Data.Text (Text)
import Control.Monad.Reader
import Language.LSP.Server (LanguageContextEnv, LspT, runLspT)
    
--------------------------------------------------------------------------------

data Env = Env
  { envChan :: Chan Text
  , envDevMode :: Bool
  }

type ServerM = ReaderT Env IO

runServerM :: Env -> LanguageContextEnv () -> LspT () ServerM a -> IO a
runServerM env ctxEnv program = runReaderT (runLspT ctxEnv program) env

writeLog :: Text -> LspT () ServerM ()
writeLog msg = do
  chan <- lift $ asks envChan
  liftIO $ writeChan chan msg
