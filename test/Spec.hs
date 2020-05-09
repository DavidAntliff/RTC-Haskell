import Test.Tasty
import qualified Lib.Tests (unitTests)
import qualified Math.Tests (unitTests)
import qualified Quadruple.Tests (unitTests)

main :: IO()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests" [ Lib.Tests.unitTests
                              , Math.Tests.unitTests
                              , Quadruple.Tests.unitTests
                              ]
