module MipsGen(gen) where
import Util
import qualified Assem as A
import qualified Tree as T
import qualified Symbol as S
import qualified Frame as Fr

import qualified Control.Monad.RWS as RWS
import Control.Monad(zipWithM_)
import qualified Data.Monoid as Monoid
import qualified Control.Monad.State.Strict as ST

type Instr = A.Instr S.Temp S.Label
type CodegenResults = RWS.RWS (Fr.Registers S.Temp) (Monoid.Endo [Instr]) S.SymbolTable

emit :: Instr -> CodegenResults ()
emit instr = RWS.tell $ Monoid.Endo ([instr]++)

genTemp = liftState S.genTemp

gen :: Fr.Registers S.Temp -> T.Stm -> ST.State S.SymbolTable [Instr]
gen regs stm = do
  table <- ST.get
  let (_, table', instrs) = RWS.runRWS (munchStm stm) regs table
  ST.put table'
  return $ Monoid.appEndo instrs []

munchStm :: T.Stm -> CodegenResults ()
munchStm stm =
    let makeBranch branch l r false true = do
          l' <- munchExp l
          r' <- munchExp r
          emit $ A.Oper (branch l' r' true) [l', r'] [] (Just [true, false])

        makeSW offset from to = do
          from' <- munchExp from
          to' <- munchExp to
          emit $ A.Oper (A.SW to' from' offset) [from', to'] [] Nothing

    in case stm of
      T.Label l -> emit $ A.Label l

      T.Seq stm1 stm2 -> munchStm stm1 >> munchStm stm2

      T.Jump (T.Name label) jumpTo -> emit $ A.Oper (A.B label) [] [] (Just jumpTo)
      T.Jump exp jumpTo -> do
               exp' <- munchExp exp
               emit $ A.Oper (A.JR exp') [exp'] [] (Just jumpTo)

      T.CJump op l r f t -> makeBranch (relopToAssem op) l r f t

      T.Move (T.Temp t) r -> do
                  r' <- munchExp r
                  emit $ A.Oper (A.MOVE t r') [t, r'] [t] Nothing

      T.Move (T.Mem (T.Binop T.Plus to (T.Const c))) from -> makeSW c from to
      T.Move (T.Mem (T.Binop T.Plus (T.Const c) to)) from -> makeSW c from to
      T.Move (T.Mem (T.Binop T.Minus to (T.Const c))) from -> makeSW (-c) from to
      T.Move (T.Mem (T.Const c)) from -> do
                  zero <- RWS.asks (Fr.zero . Fr.specialRegs)
                  from' <- munchExp from
                  emit $ A.Oper (A.SW zero from' c) [from'] [] Nothing
      T.Move to@(T.Mem _) from -> makeSW 0 from to
      -- Should the general case assume that something is being moved into Temp or Mem?

      T.ExpStm e -> munchExp e >> return ()

relopToAssem T.Eq = A.BEQ
relopToAssem T.Ne = A.BNE
relopToAssem T.Lt = A.BLT
relopToAssem T.Gt = A.BGT
relopToAssem T.Le = A.BLE
relopToAssem T.Ge = A.BGE

munchExp :: T.Exp -> CodegenResults S.Temp
munchExp exp = do
  result <- genTemp
  let makeADDI exp const = do
        exp' <- munchExp exp
        emit $ A.Oper (A.ADDI result exp' const) [exp'] [result] Nothing
        return result

      makeLW offset fromReg = do
        emit $ A.Oper (A.LW result fromReg offset) [fromReg] [result] Nothing
        return result

  case exp of
    T.Binop T.Plus (T.Const c) r -> makeADDI r c
    T.Binop T.Plus l (T.Const c) -> makeADDI l c
    T.Binop T.Minus l (T.Const c) -> makeADDI l (-c)

    T.Binop op l r -> do
      l' <- munchExp l
      r' <- munchExp r
      let assem = binopToAssem op
      emit $ A.Oper (assem result l' r') [l', r'] [result] Nothing
      return result

    T.Mem (T.Binop T.Plus (T.Const c) r) -> munchExp r >>= makeLW c
    T.Mem (T.Binop T.Plus l (T.Const c)) -> munchExp l >>= makeLW c
    T.Mem (T.Binop T.Minus l (T.Const c)) -> munchExp l >>= makeLW (-c)
    T.Mem (T.Const c) -> RWS.asks (Fr.zero . Fr.specialRegs) >>= makeLW c

    T.Mem at -> do
      at' <- munchExp at
      emit $ A.Oper (A.LW result at' 0) [at'] [result] Nothing
      return result

    T.Const i -> do
      emit $ A.Oper (A.LI result i) [] [result] Nothing
      return result

    T.Temp t -> return t

    -- T.Name label -> shouldn't happen

    -- How would this be handled if variables could be functions?
    T.Call (T.Name label) args -> do
      v0 <- RWS.asks (Fr.v0 . Fr.specialRegs)
      calldefs <- RWS.asks Fr.calldefs
      args' <- munchArgs args
      emit $ A.Oper (A.JAL label) args' calldefs Nothing
      return v0

munchArgs :: [T.Exp] -> CodegenResults [S.Temp]
munchArgs args = do
  argRegs <- RWS.asks Fr.argRegs
  args' <- mapM munchExp args
  -- silently fail on functions with too many parameters
  zipWithM_ (\ arg reg -> emit $ A.Oper (A.MOVE reg arg) [arg] [reg] Nothing) args' argRegs
  return argRegs

binopToAssem T.Plus = A.ADD
binopToAssem T.Minus = A.SUB
binopToAssem T.Mul = A.MUL
binopToAssem T.Div = A.DIV
