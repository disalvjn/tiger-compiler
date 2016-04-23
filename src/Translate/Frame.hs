module Translate.Frame(Frame(..), Access(..), Fragment(..), SpecialRegs(..), Registers(..),
                       newFrame, escapesToAccesses, wordSize, externalCall, viewShift
                      , calldefs, createRegs, colors) where
import qualified Control.Monad.State.Strict as ST
import qualified Symbol as S
import qualified Translate.Tree as Tr
import qualified CodeGen.Assem as Assem
import qualified Data.Map as M
import Control.Applicative

wordSize = 4

data Access = InFrame Int | InReg S.Temp
            deriving (Show)

data FrameType = Mips deriving (Show)

data Frame = Frame {frameType :: FrameType,
                    frameFormals :: [Access],
                    frameLocals :: [Access],
                    frameName :: S.Label,
                    parentFrame :: S.Label}
             deriving (Show)

data Fragment = StringFrag S.Label String
              | ProcFrag Tr.Stm Frame
                deriving (Show)

-- https://en.wikipedia.org/wiki/MIPS_instruction_set#Compiler_register_usage
-- http://www.cs.uni.edu/~fienup/cs041s08/lectures/lec20_MIPS.pdf
data Registers a = Registers { specialRegs :: SpecialRegs a
                             , argRegs :: [a] -- a0-a3 or $4-$7
                             , calleeSaveTemps :: [a] -- s0-s7 or $16-$23
                             , callerSaveTemps :: [a]} -- t0-t7 & t8-t9 or $8-$15 & $24-$25

data SpecialRegs a = SpecialRegs { zero :: a -- always zero
                                 , at :: a -- assembler temporary
                                 , v0 :: a -- function results results
                                 , v1 :: a -- function results
                                 , k0 :: a -- reserved for OS kernel
                                 , k1 :: a -- reserved for OS kernel
                                 , gp :: a -- global pointer
                                 , sp :: a -- stack pointer ; callee save
                                 , fp :: a -- frame pointer ; callee save
                                 , ra :: a -- return address
                                 }

colors :: (Ord a) => Registers a -> M.Map a Int
colors (Registers (SpecialRegs zero at v0 v1 k0 k1 gp sp fp ra) args calleeSave callerSave) =
  let arg = M.fromList $ zip args [4..7]
      callee = M.fromList $ zip calleeSave [16..23]
      caller = M.fromList $ zip callerSave ([8..15] ++ [24..25])
      special = M.fromList [(zero, 0), (at, 1), (v0, 2), (v1, 3)
                           ,(k0, 26), (k1, 27), (gp, 28), (sp, 29)
                           ,(fp, 30), (ra, 31)]
  in M.unions [arg, callee, caller, special]

createRegs :: ST.State S.SymbolTable (Registers S.Temp)
createRegs = do
  argRegs <- mapM (\_ -> S.genTemp) [0..3]
  calleeSaveTemps <- mapM (\_ -> S.genTemp) [0..7]
  callerSaveTemps <- mapM (\_ -> S.genTemp) [0..9]
  [zero,at,v0,v1,k0,k1,gp,sp,fp,ra] <- mapM (\_ -> S.genTemp) [0..9]
  let specials = SpecialRegs zero at v0 v1 k0 k1 gp sp fp ra
  return $ Registers specials argRegs calleeSaveTemps callerSaveTemps


-- List of registers that are potentially trashed after calling a subroutine
calldefs :: Registers a -> [a]
calldefs (Registers specials args calleeSaves callerSaves) =
    (v0 specials) : (v1 specials) : (gp specials) : (ra specials) : (args ++ callerSaves)


newFrame :: [Access] -> [Access] -> S.Label -> S.Label -> Frame
newFrame = Frame Mips

-- REMEMBER: the static link is treated as formal that always escapes
escapesToAccesses :: [Bool] -> [Bool] -> ST.State S.SymbolTable ([Access], [Access])
escapesToAccesses formals locals = do
  (formalEscapes, offset) <- go formals 0
  (localEscapes, _) <- go locals offset
  return (formalEscapes, localEscapes)
      where go escapes offset =
                case escapes of
                  [] -> return ([], offset)
                  (True : rest) -> do
                               (accesses, offset') <- go rest (offset + wordSize)
                               return ((InFrame offset) : accesses, offset')
                  (False : rest) -> do
                               (accesses, offset') <- go rest offset
                               regLabel <- S.genTemp
                               return ((InReg regLabel) : accesses, offset')

externalCall :: S.Label -> [Tr.Exp] -> Tr.Ex
externalCall name args = Tr.Ex $ Tr.Call (Tr.Name name) args

-- Appel's procEntryExit 1
viewShift :: Frame -> Tr.Stm -> Tr.Stm
viewShift f = id

-- Appel's procEntryExit 2
sink :: Registers S.Temp -> [Assem.Instr S.Temp label] -> [Assem.Instr S.Temp label]
sink (Registers specials args calleeSave callerSave) fnBodyInstrs =
    fnBodyInstrs ++ [(Assem.Oper Assem.NOOP [] ((v0 specials) : (sp specials) : (fp specials) :
                                               args ++ calleeSave)
                    Nothing)]
