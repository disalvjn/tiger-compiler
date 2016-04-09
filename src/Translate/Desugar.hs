module Translate.Desugar(desugar) where
import AST.Core
import AST.Traversal
import qualified Semant.Core as Semant
import qualified Semant.Type as Type
import qualified Symbol as S
import qualified Control.Monad.State.Strict as ST

desugar :: Semant.TypedExp -> ST.State S.SymbolTable Semant.TypedExp
desugar ast@(Exp (datum@(pos, typ), exp)) =
    case exp of
      ForExp var lo hi body -> do
          {--
            for i := lo to hi body -->
            let var i := lo
                var limit := hi
            in if i < limit
               then while 1 do (body; if i < limit then i := i + 1 else break)

            This works when hi = maxInt, unlike a more straightforward translation into a while loop.
          --}
          limitSym <- S.genSym
          let declareVar = Dec (pos, VarDec var Nothing lo)
              declareLimit = Dec (pos, VarDec limitSym Nothing hi)
              decs = [declareVar, declareLimit]
              varAsVar = Var ((pos, Type.IntType), SimpleVar var)
              limitAsVar = Var ((pos, Type.IntType), SimpleVar limitSym)
              varExp v@(Var ((pos, typ), _)) = Exp ((pos, typ), VarExp v)
              -- i < limit
              isVarLessThanLimit = Exp ((pos, Type.IntType),
                                           OpExp (varExp varAsVar) LtOp (varExp limitAsVar))
              -- i := i + 1
              incVar = Exp (datum, AssignExp varAsVar
                                     (Exp ((pos, Type.IntType),
                                           OpExp (varExp varAsVar) PlusOp
                                                     (Exp ((pos, Type.IntType), IntExp 1)))))
              -- if i < limit then i := i + 1 else break
              reenterLoop = Exp (datum, IfExp isVarLessThanLimit incVar
                                          (Just (Exp (datum, BreakExp))))

              -- (body; if i < limit then i := i + 1 else break)
              whileBody = Exp (datum, SeqExp [body, reenterLoop])
              -- while 1 do (body; if i < limit then i := i + 1 else break)
              whileLoop = Exp (datum, WhileExp (Exp ((pos, Type.IntType), IntExp 1)) whileBody)
              -- if i < limit then while 1 do (body; if i < limit then i := i + 1 else break)
              enterLoop = Exp (datum, IfExp isVarLessThanLimit whileLoop Nothing)
              entireExp = Exp (datum, LetExp decs enterLoop)
          desugar entireExp
      _ -> mapMExp desugar ast
