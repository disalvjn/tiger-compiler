module Translate.Frame(Frame(..), Access(..), Fragment(..), SpecialRegs(..), Registers(..),
                       newFrame, escapesToAccesses, wordSize, externalCall, viewShift, sink
                      , calldefs, createRegs, colors, findMaxOutgoingParams, prologueEpilogue
                      , allocateLocals) where
import qualified Control.Monad.State.Strict as ST
import qualified Symbol as S
import qualified Translate.Tree as Tr
import Translate.Tree((->-))
import qualified CodeGen.Assem as Assem
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe(fromMaybe)
import Control.Applicative

wordSize = 4

data Access = InFrame Int | InReg S.Temp
            deriving (Show)

isInFrame (InFrame _) = True
isInFrame (InReg _) = False

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
    (v0 specials) : (v1 specials) : (gp specials)
    : (k0 specials) : (k1 specials) : (ra specials) : (args ++ callerSaves)


newFrame :: [Access] -> [Access] -> S.Label -> S.Label -> Frame
newFrame formals locals name parentName =
  Frame Mips formals (InFrame (-wordSize) : locals) {--dynamic link--} name parentName

-- REMEMBER: the static link is treated as formal that always escapes
escapesToAccesses :: [Bool] -> [Bool] -> ST.State S.SymbolTable ([Access], [Access])
escapesToAccesses formals locals = do
  formalEscapes <- go formals [0*wordSize, 1*wordSize..]
  -- local var slot -1 is used by the dynamic link
  localEscapes <- go locals [-2*wordSize, -3*wordSize..]
  return (formalEscapes, localEscapes)
      where go escapes offsets@(offset : restOffsets) =
                case escapes of
                  [] -> return []
                  (True : rest) -> do
                               accesses <- go rest restOffsets
                               return $ InFrame offset : accesses
                  (False : rest) -> do
                               accesses <- go rest offsets
                               temp <- S.genTemp
                               return $ InReg temp : accesses

allocateLocals :: Frame -> Int -> (Frame, [Int]) -- newframe, list of offsets
allocateLocals frame num =
  let locals = frameLocals frame
      isInFrame offset = case offset of
                           InReg _ -> False
                           _ -> True
      InFrame lastOffset = fromMaybe (InFrame (-wordSize)) . L.find isInFrame $ reverse locals
      nextOffsets = take num . iterate (\x -> x - wordSize) $ lastOffset - wordSize
      newFrame = frame {frameLocals = locals ++ map InFrame nextOffsets}
  in (newFrame, nextOffsets)

externalCall :: S.Label -> [Tr.Exp] -> Tr.Ex
externalCall name args = Tr.Ex $ Tr.Call (Tr.Name name) args

-- Appel's procEntryExit 1
-- Does two things:
-- 1) Moves every incoming argument into the place where it's seen by the procedure
--    (some temp or a memory location).
-- 2) Move every callee save reg into a new temp. This gives the register allocator an opportunity
--    to spill them (since it can't spill precolored regs). If it doesn't, the moves will be coalesced.
viewShift :: Registers S.Temp -> Frame -> Tr.Stm -> ST.State S.SymbolTable Tr.Stm
viewShift (Registers specials args someCalleeSaves _) frame body = do
  let calleeSaves = (fp specials) : (ra specials) : someCalleeSaves
  calleeSavesTo <- mapM (\_ -> S.genTemp) calleeSaves
  let toLocation access =
        case access of
          InReg t -> Tr.Temp t
          InFrame offset -> Tr.Mem (Tr.Binop Tr.Plus framePtr (Tr.Const offset))
      incomingTemps = map InReg $ (v1 specials) : args
      -- mem spots 0..4 are reserved for dynamic link & escaping parameters passed in v1 & $a0-$a3
      incomingMem = map (InFrame . (* wordSize)) [5..]
      framePtr = Tr.Temp (fp specials)
      formalMoves = zipWith (\ to from -> Tr.Move (toLocation to) (toLocation from))
                    (frameFormals frame) (incomingTemps ++ incomingMem)
      calleeSaveMoves = zipWith (\ to from -> Tr.Move (Tr.Temp to) (Tr.Temp from))
                        calleeSavesTo calleeSaves
      calleeSaveMoveBacks = zipWith (\to from -> Tr.Move (Tr.Temp to) (Tr.Temp from))
                            calleeSaves calleeSavesTo
  return $ Tr.seqStm calleeSaveMoves
    ->- Tr.seqStm formalMoves
    ->- body
    ->- Tr.seqStm calleeSaveMoveBacks

-- Appel's procEntryExit 2
-- Indicate to the register allocator that the callee saves are live throughout.
sink :: Registers S.Temp -> [Assem.Instr S.Temp label] -> [Assem.Instr S.Temp label]
sink (Registers specials _ calleeSaves _) body =
  let (retAddr, framePtr, stackPtr) = (ra specials, fp specials, sp specials)
      flag = Assem.Oper Assem.NOOP (retAddr:framePtr:stackPtr:calleeSaves) [] Nothing
  in body ++ [flag]

findMaxOutgoingParams :: Tr.Stm -> Int
findMaxOutgoingParams =
  let inStm stm =
        case stm of
          Tr.Seq s1 s2 -> max (inStm s1) (inStm s2)
          Tr.Jump exp _ -> (inExp exp)
          Tr.CJump _ e1 e2 _ _ -> max (inExp e1) (inExp e2)
          Tr.Move e1 e2 -> max (inExp e1) (inExp e2)
          Tr.ExpStm e1 -> inExp e1
          _ -> 0

      inExp exp =
        case exp of
          Tr.Binop _ e1 e2 -> max (inExp e1) (inExp e2)
          Tr.Mem e -> inExp e
          Tr.Eseq s e -> max (inStm s) (inExp e)
          Tr.Call fun args -> max (length args) $ max (inExp fun) (foldr max 0 (map inExp args))
          _ -> 0
  in inStm


-- Appel's procEntryExit 3
prologueEpilogue :: Registers S.Temp -> Frame -> Int -> [Assem.Instr S.Temp S.Label] -> Bool
                 -> [Assem.Instr S.Temp S.Label]
prologueEpilogue (Registers specials _ calleeSaves _) frame outgoingParams body isFn =
  -- callee's frameFormals are stored in the caller's frame.
  let frameSize = wordSize * (outgoingParams + (length . filter isInFrame . frameLocals $ frame))
      (framePtr, stackPtr, retAddr) = (fp specials, sp specials, ra specials)
      -- Added after register allocation, so no need to specify src and dest
      prologue = [ if isFn
                   then Assem.Oper (Assem.SW stackPtr framePtr (-wordSize)) [] [] Nothing
                   else Assem.Oper Assem.NOOP [] [] Nothing
                 , Assem.Oper (Assem.MOVE framePtr stackPtr) [] []  Nothing
                 , Assem.Oper (Assem.ADDI stackPtr stackPtr (-frameSize)) [] [] Nothing
                 ]
      epilogue = Assem.Oper (Assem.ADDI stackPtr stackPtr frameSize) [] [] Nothing
                 : if isFn
                   then [Assem.Oper (Assem.LW framePtr stackPtr (-wordSize)) [] [] Nothing
                        , Assem.Oper (Assem.JR retAddr) [] [] Nothing]
                   else []
  in prologue ++ body ++ epilogue
