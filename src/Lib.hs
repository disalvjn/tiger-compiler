module Lib where
import Parse(parse)
import Lex(tokenize)
import qualified Symbol as S
import qualified Control.Monad.State.Strict as ST
import AST
import qualified Semant as Semant
import qualified Translate as Translate
import Debug.Trace(trace)

typecheck str =
  let (symTab, tokens) = tokenize str
      ast = parse (trace (show tokens) tokens)
      (env, symTab') = ST.runState Semant.rootEnv symTab
  in (Semant.analyze env ast, symTab')


createTransConfig :: ST.State S.SymbolTable Translate.TransConfig
createTransConfig = do
  framePtr <- S.genTemp
  returnReg <- S.genTemp
  malloc <- S.genLabel
  stringEqLabel <- S.genLabel
  initArrayLabel <- S.genLabel
  return (Translate.TransConfig framePtr returnReg malloc stringEqLabel initArrayLabel)

translate ast = do
  ast' <- Translate.makeIdsUnique ast
  (accessMap, mainFrame) <- Translate.buildAccessMap (Translate.findEscapes ast') ast'
  config <- createTransConfig
  Translate.translate config mainFrame accessMap ast'

translateStr str =
  let (Right ast, table) = typecheck str
  in ST.evalState (translate ast) table

--fmap uniqueIds $ readFile "../test/testcases/testmakeidsunique.tig"
-- fmap translateStr $ readFile "../test/testcases/test3.tig"
