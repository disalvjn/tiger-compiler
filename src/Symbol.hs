module Symbol(empty, name, intern, Symbol, SymbolTable) where
import qualified Data.Map as M
import Control.Monad.State.Lazy

newtype Symbol = Symbol {sym :: Int} deriving (Ord, Eq, Show)

data SymbolTable = SymbolTable {toSym :: M.Map String Symbol,
                                toStr :: M.Map Symbol String,
                                lastId :: Int}
                   deriving (Show)

empty = SymbolTable M.empty M.empty 0

name :: Symbol -> SymbolTable -> Maybe String
name s table = M.lookup s (toStr table)

intern :: String -> State SymbolTable Symbol
intern symName = do
  SymbolTable toSym toStr lastId <- get
  case M.lookup symName toSym of
    Just sym -> return sym
    Nothing -> let sym = Symbol lastId
                   toSym' = M.insert symName sym toSym
                   toStr' = M.insert sym symName toStr
               in do put $ SymbolTable toSym' toStr' (lastId + 1)
                     return sym
