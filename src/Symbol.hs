module Symbol(empty, name, intern, genSym, genTemp, genLabel, symbol,
              Symbol, Temp, Label, SymbolTable) where
import qualified Data.Map as M
import Control.Monad.State.Strict

newtype Symbol = Symbol Int deriving (Ord, Eq, Show)
newtype Temp = Temp Int deriving (Ord, Eq, Show)
newtype Label = Label Int deriving (Ord, Eq, Show)

data SymbolTable = SymbolTable {toSym :: M.Map String Symbol,
                                toStr :: M.Map Symbol String,
                                nextId :: Int}
                   deriving (Show)

empty = SymbolTable M.empty M.empty 0

name :: Symbol -> SymbolTable -> Maybe String
name s table = M.lookup s (toStr table)

symbol :: String -> SymbolTable -> Maybe Symbol
symbol s table = M.lookup s (toSym table)

nextInt :: State SymbolTable Int
nextInt = do
  table <- get
  let sym = nextId table
  put $ table {nextId = sym + 1}
  return sym

genSym = fmap Symbol $ nextInt
genTemp = fmap Temp $ nextInt
genLabel = fmap Label $ nextInt

intern :: String -> State SymbolTable Symbol
intern symName = do
  SymbolTable toSym toStr nextId <- get
  case M.lookup symName toSym of
    Just sym -> return sym
    Nothing -> let sym = Symbol nextId
                   toSym' = M.insert symName sym toSym
                   toStr' = M.insert sym symName toStr
               in do put $ SymbolTable toSym' toStr' (nextId + 1)
                     return sym
