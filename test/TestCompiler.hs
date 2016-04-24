module TestCompiler (tests) where
import Compile(compileFile)
import Test.HUnit
import qualified System.Process as SP

--import System.IO (openFile, hClose, IOMode(WriteMode), hPutStr, getContents)

{-- These tests are organized as follows:
In the testcases/compiler directory, each test has two files associated with it:
    1) a .tig, which is the program to compile, and
    2) a .expected, which is the expected output printed to the screen.

A file is compiled and run by using compileFile on it, which produces a .s,
then running "spim -file <prog>.s > <prog>.temp" [SPIM is a MIPS simulator].

SPIM always prefaces the output with 5 lines of copyright stuff.

--}

--run :: String -> IO ()
run name = do
  let base = "test/testcases/compiler/" ++ name
      strip = unlines . drop 5 . lines
  compileFile $ base ++ ".tig"
  expect <- readFile $ base ++ ".expected"
  actualOutput <- fmap strip $ SP.readProcess "spim" ["-file", name ++ ".s"] ""
  assertBool ("Expected: " ++ expect ++ "\n" ++ "But got: " ++ actualOutput) (expect == actualOutput)


tests = test [ "if1.tig" ~: run "if1" ,
               "while1.tig" ~: run "while1"]
