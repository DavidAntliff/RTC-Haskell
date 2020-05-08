import Test.Tasty
import qualified TestFoo (unitTests)
import qualified TestLib (unitTests)

main :: IO()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests" [TestFoo.unitTests, TestLib.unitTests]
