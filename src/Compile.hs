{-# LANGUAGE TemplateHaskell #-}
module Compile(main, compileFile) where

import FrontEnd.Core(lexAndParse)
import qualified Symbol as S
import qualified Control.Monad.State.Strict as ST
import qualified CodeGen.MipsGen as Gen
import Semant.Core(analyze)
import Semant.Environment(rootEnv)
import qualified Translate.Core as Translate
import qualified Translate.Frame as Fr
import qualified CodeGen.MipsGen as Gen
import qualified Allocation.Core as Allocation
import qualified CodeGen.Assem as A
import qualified Data.Map as M
import qualified Data.Set as Set
import Control.Monad (forM_, when)
import Debug.Trace(trace)
import Util

import qualified Data.FileEmbed as FileEmbed

import System.Environment (getArgs)
import System.IO (openFile, hClose, IOMode(WriteMode), hPutStr, getContents)
import qualified System.FilePath as Posix


runtime = $(FileEmbed.embedStringFile "src/runtime.s")

createConfigs :: ST.State S.SymbolTable (Fr.Registers S.Temp, Translate.TransConfig)
createConfigs = do
  regs <- Fr.createRegs
  let specialRegs = Fr.specialRegs regs
      framePtr = Fr.fp specialRegs
      returnReg = Fr.v0 specialRegs
  malloc <- S.namedLabel "malloc"
  stringEqLabel <- S.namedLabel "stringEq"
  initArrayLabel <- S.namedLabel "initArray"
  let runTime = ["print", "printInt", "size", "substring", "concat", "not", "exit"]
  runTimeSyms <- mapM S.intern runTime
  runTimeLabels <- mapM S.namedLabel runTime
  let runTimeFunctions = M.fromList $ zip runTimeSyms runTimeLabels
      transConfig = Translate.TransConfig framePtr returnReg malloc stringEqLabel
                    initArrayLabel runTimeFunctions
  return (regs, transConfig)



compile :: String -> (String -> IO ()) -> IO ()
compile program printSomewhere =
  let go = do
        ast <- lexAndParse program
        env <- rootEnv
        let typed = analyze env ast
        case typed of
          Right typed' -> do
            (regs, config) <- createConfigs
            (trans, frags) <- Translate.translate config typed'
            lg (show trans)
            assem <- fmap concat $ mapM (Gen.gen regs) trans
            lg (show assem)
            --lg (show assem)
            let reservedRegs = Set.fromList [0, 1]
            colors <- Allocation.allocateRegisters 32 (Fr.colors regs) (Set.fromList [0, 1]) assem
            --lg (show colors)
            return $ Right (assem, colors)

          Left err -> return $ Left err

      (results,symTab)  = ST.runState go S.empty
  in case results of
        Right (assem, colors) -> do
          let tempToStr t = let Just col = M.lookup t colors
                            in "$" ++ (show col)
              labelToStr label = S.labelToString label symTab
          printSomewhere "main:"
          forM_ assem $ printSomewhere . (\i -> A.format i tempToStr labelToStr)
          printSomewhere "li $v0, 10\nsyscall\n" -- exit the program
          printSomewhere runtime
        Left err -> putStrLn $ show err



compileFile :: String -> IO ()
compileFile input = do
  program <- readFile input
  let inputName = Posix.takeBaseName input
  output <- openFile (inputName ++ ".s") WriteMode
  compile program (\s -> hPutStr output s >> hPutStr output "\n")
  hClose output

compileStd :: IO ()
compileStd = getContents >>= \prog -> compile prog putStrLn

main = do
  args <- getArgs
  case args of
    [] -> compileStd
    (file : _) -> compileFile file
