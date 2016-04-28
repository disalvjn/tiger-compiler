module TestCompiler (tests) where
import Compile(compileFile)
import Test.HUnit
import qualified System.Process as SP
import System.Directory(renameFile, createDirectoryIfMissing)

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
run name =
  let base = "test/testcases/compiler/" ++ name
      strip = unlines . drop 5 . lines
      go = do
        compileFile $ base ++ ".tig"
        expect <- readFile $ base ++ ".expected"
        actualOutput <- fmap strip $ SP.readProcess "spim" ["-file", name ++ ".s"] ""
        assertBool ("Expected: " ++ expect ++ "\n" ++ "But got: " ++ actualOutput)
          (expect == actualOutput)
        createDirectoryIfMissing False "compile_test_output"
        renameFile (name ++ ".s") ("compile_test_output/"++name++".s")
  in (name ++ ".tig") ~: go


tests = test [ run "if1" ,
               run "while1",
               run "record1",
               run "record2",
               run "array1",
               run "fn1",
               run "fn2",
               run "fn3",
               run "fn4",
               run "fn5"]
