module Semant.Type(Type(..), TypeId(..), TypeError(..),
                   isSubtypeOf, isComparableTo, mapType, mapMType, getTypeId, minTypeId) where

import qualified Symbol as S
import FrontEnd.Lex(Pos)

{--   TYPES

   There are four terminal types - int, str, nil, and unit - and three non-terminal
types  - arrays, records, and names. In Tiger, structurually equivalent records/arrays
are not necessarily equal. Each expression like 'type intlist = {head : int, tail : intlist}'
creates a new, unique type. Unique IDs (ints) are used to identify types. Also, NameTypes
(like 'type count = int') store the ID of the type to which they refer.

   Types are equal if their IDs are equal. Nil is a subtype of every record type.

--}

newtype TypeId = TypeId Int deriving (Show, Eq, Ord)

data Type = IntType
          | StrType
          | NilType
          | UnitType
          | NameType S.Symbol TypeId (Maybe TypeId) -- name, this id, refers to
          | RecordType [(S.Symbol, Type)] TypeId
          | ArrayType Type TypeId
            deriving (Show)

instance Eq Type where
    t1 == t2 = getTypeId t1 == getTypeId t2

isSubtypeOf NilType (RecordType _ _) = True
isSubtypeOf t1 t2 = t1 == t2

-- this is NOT transitive
isComparableTo t1 t2 = t1 `isSubtypeOf` t2 || t2 `isSubtypeOf` t1

data TypeError = UndefVar S.Symbol Pos
               | UndefType S.Symbol Pos
               | UndefField S.Symbol Pos
               | CircularType Pos
               | WrongType Type Type Pos -- expected, actual
               | WrongArity S.Symbol Int Int Pos -- expected, actual
               | ExpectedArray Type Pos -- but got
               | ExpectedRecord S.Symbol Type Pos -- with field, but got
               | ExpectedFunction S.Symbol Type Pos  -- s of type t is not a function
               | NotARecord S.Symbol Pos
               | MultipleDeclarations S.Symbol Pos
               | EveryoneKnowsFunctionsArentValues S.Symbol Pos
               | UnconstrainedNil Pos
               | BreakNotInForWhile Pos
                 deriving (Eq,Show)

intTypeId = TypeId 0
strTypeId = TypeId 1
nilTypeId = TypeId 2
unitTypeId = TypeId 3
minTypeId = TypeId 4

getTypeId IntType = intTypeId
getTypeId StrType = strTypeId
getTypeId NilType = nilTypeId
getTypeId UnitType = unitTypeId
getTypeId (NameType _ id _) = id
getTypeId (RecordType _ id) = id
getTypeId (ArrayType _ id) = id


mapType :: (Type -> Type) -> Type -> Type
mapType f t =
    case t of
      RecordType fields id -> RecordType (map (fmap f) fields) id
      ArrayType typ id -> ArrayType (f typ) id
      other -> other

mapMType :: Monad m => (Type -> m Type) -> Type -> m Type
mapMType f t =
    case t of
      RecordType fields id -> do
             newFields <- mapM (\(sym, typ) -> f typ >>= return . (,) sym) fields
             return $ RecordType newFields id
      ArrayType typ id -> do
             newElemType <- f typ
             return $ ArrayType newElemType id
      other -> return other
