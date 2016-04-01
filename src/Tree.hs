module Tree(Stm(..), Exp(..), Ex(..), Binop(..), Relop(..),
           asExp, asStm, asCx, seqStm, Tree.negate) where
import qualified Symbol as S
import qualified Control.Monad.State.Strict as ST

data Stm = Seq Stm Stm
         | Label S.Label
         | Jump Exp [S.Label]
         | CJump Relop Exp Exp S.Label S.Label
         | Move Exp Exp
         | ExpStm Exp
           deriving (Show)

data Exp = Binop Binop Exp Exp
         | Mem Exp
         | Temp S.Temp
         | Eseq Stm Exp
         | Name S.Label
         | Const Int
         | Call Exp [Exp]
           deriving (Show)

type CJumpToLabels = S.Label -> S.Label -> Stm

data Ex = Ex Exp
        | Nx Stm
        | Cx CJumpToLabels

data Binop = Plus | Minus | Mul | Div
             deriving (Show)
data Relop = Eq | Ne | Lt | Gt | Le | Ge
             deriving (Show)

negate :: Relop -> Relop
negate Eq = Ne
negate Ne = Eq
negate Lt = Ge
negate Ge = Lt
negate Le = Gt
negate Gt = Le

-- a > b | c < d ->
-- Cx (\ t f -> Seq (CJump (Gt, a, b, t, z), Seq (Label z, CJump (Lt, c, d, t, f)))
-- for some label z

seqExp :: Exp -> [Stm] -> Exp
seqExp = foldr Eseq

seqStm :: [Stm] -> Stm
seqStm [] = ExpStm $ Const 0
seqStm [x] = x
seqStm (x:xs) = Seq x (seqStm xs)

asExp :: Ex -> ST.State S.SymbolTable Exp
asExp ex =
    case ex of
      Ex exp -> return exp
      Nx stm -> return $ Eseq stm (Const 0)
      Cx genstm -> do
              reg <- S.genTemp
              t <- S.genLabel
              f <- S.genLabel
              let jumpStm = genstm t f
                  jumpSeq = seqExp (Temp reg)
                                   [ Move (Temp reg) (Const 1)
                                   , jumpStm
                                   , Label f
                                   , Move (Temp reg) (Const 0)
                                   , Label t]
              return jumpSeq

asStm :: Ex -> ST.State S.SymbolTable Stm
asStm ex =
    case ex of
      Ex exp -> return $ ExpStm exp
      Nx stm -> return stm
      Cx genstm -> do
              t <- S.genLabel
              f <- S.genLabel
              return $ genstm t f

asCx :: Ex -> CJumpToLabels
asCx ex =
    case ex of
      Ex exp -> CJump Gt exp (Const 0)
      Nx stm -> CJump Gt (Const 0) (Const 1)
      Cx genstm -> genstm

{--
unCx :: Ex -> ST.State S.SymbolTable (S.Label -> S.Label -> Stm)
unCx ex =
    case ex of
      Ex exp ->
      -- Nx stm should never happen...
      Cx genstm -> return genstm
--}
