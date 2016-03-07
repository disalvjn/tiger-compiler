module Lib where
import Parse(parse)
import Lex(tokenize)
import qualified Symbol as S
import qualified Control.Monad.State as ST
import AST
import qualified Semant as Semant
import qualified Translate as Translate
import Debug.Trace(trace)

typecheck str =
  let (symTab, tokens) = tokenize str
      ast = parse (trace (show tokens) tokens)
      (env, symTab') = ST.runState Semant.rootEnv symTab
      typeChecked = Semant.analyze env ast
      escaped = typeChecked >>= Right . Translate.findEscapes
  in escaped

uniqueIds str =
  let (symTab, tokens) = tokenize str
      ast = parse tokens
  in ST.runState (Translate.makeIdsUnique ast) symTab
