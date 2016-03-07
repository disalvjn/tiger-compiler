module Frame(Frame(..), Access(..), newFrame, escapesToAccesses) where
import qualified Control.Monad.State.Lazy as ST
import qualified Symbol as S
import Control.Applicative

data Access = InFrame Int | InReg S.Temp
            deriving (Show)

data FrameType = Mips deriving (Show)

data Frame = Frame {frameType :: FrameType,
                    frameFormals :: [Access],
                    frameLocals :: [Access],
                    frameName :: S.Label}
             deriving (Show)


newFrame :: [Access] -> [Access] -> S.Label -> Frame
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
                               (accesses, offset') <- go rest (offset + 1)
                               return ((InFrame offset) : accesses, offset')
                  (False : rest) -> do
                               (accesses, offset') <- go rest offset
                               regLabel <- S.genTemp
                               return ((InReg regLabel) : accesses, offset')
