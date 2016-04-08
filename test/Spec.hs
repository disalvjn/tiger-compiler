import Test.HUnit
import qualified Typecheck as Typecheck
import qualified Translation as Translation
import qualified TestLiveness as Liveness

main = runTestTT $ test [Typecheck.tests, Translation.tests, Liveness.tests]
