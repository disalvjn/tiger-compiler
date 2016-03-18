module Frame(Frame(..), Access(..), Fragment(..),
             newFrame, escapesToAccesses, wordSize, externalCall, viewShift) where
import qualified Control.Monad.State.Lazy as ST
import qualified Symbol as S
import qualified Tree as Tr
import Control.Applicative

wordSize = 1

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

viewShift :: Frame -> Tr.Stm -> Tr.Stm
viewShift f = id
