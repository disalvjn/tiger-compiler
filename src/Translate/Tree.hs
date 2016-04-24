module Translate.Tree(Stm(..), Exp(..), Ex(..), Binop(..), Relop(..), (->-), (->+),
                      asExp, asStm, asCx, seqStm, Translate.Tree.negate) where
import qualified Symbol as S
import qualified Control.Monad.State.Strict as ST

data Stm = Seq Stm Stm
         | Label S.Label -- defines a label
         | Jump Exp [S.Label]
         | CJump Relop Exp Exp S.Label S.Label -- labels are True, False
         | Move Exp Exp
         | ExpStm Exp
           deriving (Show)

data Exp = Binop Binop Exp Exp
         | Mem Exp
         | Temp S.Temp
         | Eseq Stm Exp
         | Name S.Label -- use of a label
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

(->-) = Seq
(->+) = Eseq

negate :: Relop -> Relop
negate Eq = Ne
negate Ne = Eq
negate Lt = Ge
negate Ge = Lt
negate Le = Gt
negate Gt = Le

seqStm :: [Stm] -> Stm
seqStm [] = ExpStm $ Const 0
seqStm [x] = x
seqStm (x:xs) = x ->- seqStm xs

asExp :: Ex -> ST.State S.SymbolTable Exp
asExp ex =
    case ex of
      Ex exp -> return exp
      Nx stm -> return $ stm ->+ (Const 0)
      Cx genstm -> do
              reg <- S.genTemp
              t <- S.genLabel
              f <- S.genLabel
              let jumpStm = genstm t f
                  jumpSeq =  Move (Temp reg) (Const 1)
                             ->- jumpStm
                             ->- Label f
                             ->- Move (Temp reg) (Const 0)
                             ->- Label t
                             ->+ Temp reg
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
      Nx stm -> \t f -> stm ->- Jump (Name f) [f]
      Cx genstm -> genstm
