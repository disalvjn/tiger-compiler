import Test.HUnit
import qualified Typecheck as Typecheck
import qualified Translation as Translation

main = runTestTT $ test [Typecheck.tests, Translation.tests]
