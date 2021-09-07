import qualified Test.SrcLoc                   as SrcLoc
import           Test.Tasty                     ( TestTree
                                                , defaultMain
                                                , testGroup
                                                )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [SrcLoc.tests]
