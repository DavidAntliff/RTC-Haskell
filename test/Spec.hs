import Test.Tasty
import qualified Lib.Tests (unitTests)
import qualified Math.Tests (unitTests)
import qualified Quadruple.Tests (unitTests)
import qualified Color.Tests (unitTests)
import qualified Canvas.Tests (unitTests)
import qualified Internal.Canvas.Tests (unitTests)
import qualified Matrix.Tests (unitTests)
import qualified Transformations.Tests (unitTests)
import qualified RaySphere.Tests (unitTests)

main :: IO()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests" [ Lib.Tests.unitTests
                              , Math.Tests.unitTests
                              , Quadruple.Tests.unitTests
                              , Color.Tests.unitTests
                              , Internal.Canvas.Tests.unitTests
                              , Canvas.Tests.unitTests
                              , Matrix.Tests.unitTests
                              , Transformations.Tests.unitTests
                              , RaySphere.Tests.unitTests
                              ]
