module Lib where
import Parse(parse)
import Lex(tokenize)
import qualified Symbol as S
import qualified Control.Monad.State as ST
import AST
import qualified Semant as Semant
import Debug.Trace(trace)

typecheck str =
  let (symTab, tokens) = tokenize str
      ast = parse (trace (show tokens) tokens)
      (env, symTab') = ST.runState Semant.rootEnv symTab
  in (trace (show symTab') Semant.analyze env ast)
