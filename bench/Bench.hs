-- | A by-hand benchmark of the hover. CI does not run it.
--
-- It times the same hovers on the als that stack builds from this tree,
-- with the hover cache off (--no-hover-cache) and on, and prints the
-- median of each and the ratio on / off:
--
-- > stack bench
--
-- Each case writes its own Agda files to a temporary directory, so the
-- result depends only on this tree.
module Main (main) where

import Control.Monad (forM, forM_, replicateM, when)
import Control.Monad.IO.Class (liftIO)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import GHC.Clock (getMonotonicTimeNSec)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Test
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getTemporaryDirectory, removeDirectoryRecursive)
import System.Environment (getArgs, getEnvironment)
import System.Exit (die)
import System.FilePath (takeDirectory, (</>))
import System.Process (CreateProcess (cwd, env))
import Text.Printf (printf)

-- | Generated Agda files, and where to hover
data Case = Case
  { caseName :: String,
    caseFiles :: [(FilePath, Text)],
    -- | The file that the hover is in
    caseMain :: FilePath,
    caseHover :: Position,
    -- | The file that the edit adds a definition to, and then removes it
    caseEdited :: FilePath
  }

-- | Main imports M(n-1), which imports M(n-2), and so on down to M0. The
-- edit is to M(n-2), near the top, so the modules below it do not change
-- and a reload can reuse them.
modules :: Int -> Case
modules n = (chain n 0 0) {caseName = "modules " ++ show n}

-- | 'modules 50', with this many more include dirs, all empty. Each module
-- lookup looks in every include dir.
includeDirs :: Int -> Case
includeDirs dirs = (chain 50 dirs 0) {caseName = "include dirs " ++ show dirs}

-- | 'modules 50', with this many more lines in Main. A reload checks Main
-- again.
largeFile :: Int -> Case
largeFile lines' = (chain 50 0 lines') {caseName = "Main " ++ show lines' ++ " lines"}

-- | A chain of n modules, with this many more include dirs and this many
-- more lines in Main
chain :: Int -> Int -> Int -> Case
chain n dirs lines' =
  Case
    { caseName = "",
      caseFiles = ("Main.agda", mainText) : [(file k, moduleText k) | k <- [0 .. n - 1]] ++ library,
      caseMain = "Main.agda",
      -- 'f' in 'g = f'
      caseHover = Position 5 4,
      caseEdited = file (n - 2)
    }
  where
    name k = "M" <> Text.pack (show k)
    file k = Text.unpack (name k) ++ ".agda"
    mainText =
      Text.unlines $
        ["module Main where", "", "open import " <> name (n - 1), "", "g : T → T", "g = f"]
          ++ concat [["", "h" <> Text.pack (show k) <> " : T", "h" <> Text.pack (show k) <> " = f t"] | k <- [1 .. lines' `div` 3]]
    dir k = "i" ++ show k
    -- an empty file in each include dir, so that the dir exists
    library
      | dirs == 0 = []
      | otherwise =
          ("bench.agda-lib", "name: bench\ninclude: . " <> Text.pack (unwords (map dir [1 .. dirs])) <> "\n")
            : [(dir k </> "empty", "") | k <- [1 .. dirs]]
    moduleText 0 = Text.unlines ["module M0 where", "", "data T : Set where", "  t : T", "", "f : T → T", "f x = x"]
    moduleText k = Text.unlines ["module " <> name k <> " where", "", "open import " <> name (k - 1) <> " public", "", "d" <> name k <> " : T", "d" <> name k <> " = f t"]

-- | What every hover in every case shows
expected :: Text
expected = "\n```agda-language-server\nT → T\n```\n"

cases :: [Case]
cases =
  [modules 50, modules 100, modules 200]
    ++ [includeDirs 10, includeDirs 20, includeDirs 40]
    ++ [largeFile 1000, largeFile 2000, largeFile 4000]

-- | Hovers with no change, and hovers after an edit
noChangeSamples, editCycles :: Int
noChangeSamples = 10
editCycles = 3

main :: IO ()
main = do
  args <- getArgs
  -- 'stack bench' puts the als it builds on the PATH
  als <- case args of
    [] -> return "als"
    ["--als", path] -> return path
    _ -> die "usage: stack bench [--ba '--als <path to als>']"
  putStrLn "Median time of a hover, in ms, with the hover cache off and on. Ratio is on / off.\n"
  putStrLn "                 | no change                   | after an edit"
  putStrLn "case             |      off       on     ratio |      off       on     ratio"
  forM_ cases $ \c -> do
    (offNoChange, offEdit) <- measure "off" (als ++ " --no-hover-cache") c
    (onNoChange, onEdit) <- measure "on" als c
    printf
      "%-16s | %8.1f %8.1f %9.2f | %8.1f %8.1f %9.2f\n"
      (caseName c)
      offNoChange
      onNoChange
      (onNoChange / offNoChange)
      offEdit
      onEdit
      (onEdit / offEdit)

-- | The medians of the hovers with no change and of the hovers after an
-- edit, on fresh copies of the files. The first hover loads the files and
-- is not timed. The command is the als executable and its arguments.
measure :: String -> FilePath -> Case -> IO (Double, Double)
measure label als c = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "als-bench" </> map (\x -> if x == ' ' then '-' else x) (caseName c) </> label
  exists <- doesDirectoryExist dir
  when exists $ removeDirectoryRecursive dir
  createDirectoryIfMissing True dir
  forM_ (caseFiles c) $ \(file, text) -> do
    createDirectoryIfMissing True (takeDirectory (dir </> file))
    TextIO.writeFile (dir </> file) text
  let edited = dir </> caseEdited c
  original <- TextIO.readFile edited
  inSession als dir $ do
    doc <- openDoc (caseMain c) "agda"
    _ <- timedHover doc
    noChange <- replicateM noChangeSamples (timedHover doc)
    afterEdit <- fmap concat $ forM [1 .. editCycles] $ \_ -> do
      liftIO $ TextIO.writeFile edited (original <> "\nextra : T\nextra = t\n")
      added <- timedHover doc
      liftIO $ TextIO.writeFile edited original
      removed <- timedHover doc
      return [added, removed]
    return (median noChange, median afterEdit)
  where
    -- the time of a hover in ms; stops the benchmark if the hover is wrong
    timedHover doc = do
      start <- liftIO getMonotonicTimeNSec
      TResponseMessage _ _ rsp <- request SMethod_TextDocumentHover (HoverParams doc (caseHover c) Nothing)
      end <- liftIO getMonotonicTimeNSec
      case rsp of
        Right (InL (Hover (InL (MarkupContent _ content)) _)) | content == expected -> return ()
        _ -> liftIO $ die (label ++ ", " ++ caseName c ++ ": unexpected hover: " ++ show rsp)
      return (fromIntegral (end - start) / 1e6)

-- | Runs a session with als in the directory, with its own AGDA_DIR, so
-- that the Agda libraries of the user do not change the result
inSession :: FilePath -> FilePath -> Session a -> IO a
inSession als dir session = do
  let agdaDir = dir </> ".agda"
  createDirectoryIfMissing True agdaDir
  environment <- filter ((/= "AGDA_DIR") . fst) <$> getEnvironment
  let inDir process = process {cwd = Just dir, env = Just (("AGDA_DIR", agdaDir) : environment)}
  runSessionWithConfigCustomProcess inDir defaultConfig als fullLatestClientCaps dir session

median :: [Double] -> Double
median xs
  | even n = (sorted !! (half - 1) + sorted !! half) / 2
  | otherwise = sorted !! half
  where
    sorted = sort xs
    n = length xs
    half = n `div` 2
