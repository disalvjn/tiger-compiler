{-# LANGUAGE TemplateHaskell #-}

module Symbol(empty, name, intern, genSym, genTemp, genLabel, symbol,
              Symbol, Temp, Label(..), SymbolTable, namedLabel, labelToString) where
import qualified Data.Map as M
import Control.Monad.State.Strict
import Control.Lens

newtype Symbol = Symbol Int deriving (Ord, Eq, Show)
newtype Temp = Temp Int deriving (Ord, Eq, Show)
newtype Label = Label Int deriving (Ord, Eq, Show)

data SymbolTable = SymbolTable {_toSym :: M.Map String Symbol,
                                _toStr :: M.Map Symbol String,
                                _labelToStr :: M.Map Label String,
                                _nextId :: Int}
                   deriving (Show)

makeLenses ''SymbolTable

empty = SymbolTable M.empty M.empty M.empty 0

name :: Symbol -> SymbolTable -> Maybe String
name s = M.lookup s . _toStr

symbol :: String -> SymbolTable -> Maybe Symbol
symbol s = M.lookup s . _toSym

nextInt :: State SymbolTable Int
nextInt = do
  sym <- gets _nextId
  nextId .= sym + 1
  return sym

genSym = fmap Symbol $ nextInt
genTemp = fmap Temp $ nextInt
genLabel = fmap Label $ nextInt

namedLabel name = do
  label <- genLabel
  labelToStr %= M.insert label name
  return label

labelToString :: Label -> SymbolTable -> String
labelToString lab@(Label i) st =
  case M.lookup lab (_labelToStr st) of
   Just name -> name
   Nothing -> "L" ++ (show i)

intern :: String -> State SymbolTable Symbol
intern symName = do
  sym <- gets $ symbol symName
  case sym of
    Just sym -> return sym
    Nothing -> do
      sym' <- genSym
      toSym %= M.insert symName sym'
      toStr %= M.insert sym' symName
      return sym'
