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
import Translate.Canon(canonicize)
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


runtime = $(FileEmbed.embedStringFile "src/runtime.s") {-- building from shell --}
--runtime = $(FileEmbed.embedStringFile "runtime.s")   {-- in emacs --}

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
      transConfig = Translate.TransConfig regs framePtr returnReg malloc stringEqLabel
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
            (mainFrame, trans, frags) <- Translate.translate config typed'
            --lg (show trans)
            main <- process regs trans mainFrame False
            fns <- mapM (processFrag regs) frags
            --lg (show assem)
            --lg (show assem)
            --lg (show colors)
            return $ Right (main, fns)

          Left err -> return $ Left err

      processFrag regs (Fr.ProcFrag body frame) = process regs body frame True

      process regs body frame isFn = do
        let maxOutgoingParams = Fr.findMaxOutgoingParams body
        assem <- treeToAssem regs body isFn
        (startLabel : newAssem, colors, newFrame) <- color regs frame assem
        let finalAssem = startLabel
                         : Fr.prologueEpilogue regs newFrame maxOutgoingParams newAssem isFn
        return (finalAssem, colors)

      treeToAssem regs stm isFn = do
        canon <- canonicize stm
        fmap ((if isFn then Fr.sink regs else id) . concat) . mapM (Gen.gen regs) $ canon

      color regs frame assem = do
        let reservedRegs = Set.fromList [0, 1, 29, 30] -- $zero, $at, $sp, $fp
        Allocation.allocateRegisters (Fr.colors regs) reservedRegs frame regs assem

      emit assem colors symTab = do
        let tempToStr t = let Just col = M.lookup t colors
                          in "$" ++ (show col)
            labelToStr label = S.labelToString label symTab
        forM_ assem $ printSomewhere . (\i -> A.format i tempToStr labelToStr)

      (results,symTab)  = ST.runState go S.empty

  in case results of
        Right ((mainAssem, mainColors), fragsAndColors) -> do
          printSomewhere "main:"
          emit mainAssem mainColors symTab
          printSomewhere "li $v0, 10\nsyscall\n" -- exit the program
          forM_ fragsAndColors $ \(f,c) -> emit f c symTab
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
