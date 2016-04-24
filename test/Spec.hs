import Test.HUnit
import qualified TestSemant as Semant
import qualified TestTranslate as Translate
import qualified TestLiveness as Liveness
import qualified TestCompiler as Compiler

main = runTestTT $ test [{--Semant.tests, Translate.tests, Liveness.tests,--} Compiler.tests]
