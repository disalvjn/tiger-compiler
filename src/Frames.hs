module Frames where
import qualified Control.Monad.State.Lazy as ST
import Control.Applicative

newtype Label = Label Int deriving (Show, Eq)
data TempTable = TempTable {nextLabel :: Label}
               deriving (Show)

getLabel :: ST.State TempTable Label
getLabel = do
  table <- ST.get
  let label@(Label l) = nextLabel table
  ST.put $ table {nextLabel = Label (l+1)}
  return label

data Access = InFrame Int | InReg Label
              deriving (Show)

data Frame = MipsFrame {mipsFormals :: [Access],
                        mipsLocals :: [Access],
                        mipsName :: Label}
             deriving (Show)

newFrame :: Label -> [Bool] -> ST.State TempTable Frame
newFrame name formals = do
  accessList <- escapeListToAccessList formals
  return $ MipsFrame accessList [] name

escapeListToAccessList :: [Bool] -> ST.State TempTable [Access]
escapeListToAccessList escapes =
  go escapes 0
      where go escapes offset =
                case escapes of
                  [] -> return []
                  (True : rest) -> (InFrame offset :) <$> go rest (offset + 1)
                  (False : rest) -> do
                               regLabel <- getLabel
                               others <- go rest offset
                               return $ (InReg regLabel) : others
