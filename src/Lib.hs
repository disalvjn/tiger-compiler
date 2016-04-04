module Lib where
import Parse(parse)
import Lex(tokenize)
import qualified Symbol as S
import qualified Control.Monad.State.Strict as ST
import AST
import qualified Semant as Semant
import qualified Translate as Translate
import qualified Canon as Canon
import qualified Frame as Fr
import qualified Data.Map as M
import qualified MipsGen as Gen
import Debug.Trace(trace)

typecheck str =
  let (symTab, tokens) = tokenize str
      ast = parse tokens
      (env, symTab') = ST.runState Semant.rootEnv symTab
  in (Semant.analyze env ast, symTab')


createConfigs :: ST.State S.SymbolTable (Fr.Registers S.Temp, Translate.TransConfig)
createConfigs = do
  regs <- Fr.createRegs
  let specialRegs = Fr.specialRegs regs
      framePtr = Fr.fp specialRegs
      returnReg = Fr.v0 specialRegs
  malloc <- S.genLabel
  stringEqLabel <- S.genLabel
  initArrayLabel <- S.genLabel
  let runTime = ["print", "flush", "getchar", "ord", "chr",
                 "size", "substring", "concat", "not", "exit"]
  runTimeSyms <- mapM S.intern runTime
  runTimeLabels <- mapM S.namedLabel runTime
  let runTimeFunctions = M.fromList $ zip runTimeSyms runTimeLabels
      transConfig = Translate.TransConfig framePtr returnReg malloc stringEqLabel
                    initArrayLabel runTimeFunctions
  return (regs, transConfig)

translate ast = do
  (regs, config) <- createConfigs
  (transAst, fragments) <- Translate.translate config ast
  canonAst <- Canon.canonicize transAst
  assem <- fmap concat $ mapM (Gen.gen regs) canonAst
  return (assem, canonAst, transAst, fragments)
  --return (transAst, fragments)

translateStr str =
  let (Right ast, table) = typecheck str
  in ST.evalState (translate ast) table

--fmap uniqueIds $ readFile "../test/testcases/testmakeidsunique.tig"
-- fmap translateStr $ readFile "../test/testcases/test3.tig"
-- fmap translateStr $ readFile "../test/testcases/appel's/queens.tig"
