{-# LANGUAGE FlexibleContexts #-}
module Semant.Environment(Environment, EnvEntry(..),
                          resolveNameType, lookupVar, lookupType, lookupFirstType,
                          lookupTypeById, bindVar, bindFun, bindType, bindTypeToId,
                          createNameType, createArrayType, createRecordType, rootEnv) where
import Semant.Type
import qualified Symbol as S
import qualified Control.Monad.State.Strict as ST
import qualified Data.Map as M
import qualified Data.Set as Set
import FrontEnd.Lex(Pos)
import Control.Monad.Except as E

{--  ENVIRONMENTS

  An environment consists of variable and type environments. Types are mapped to
from both names (S.Symbol) and ids (TypeId). New Non-terminal types are created inside
of an environment, which assigns them unique IDs.

  Most lookup functions resolve name types.

  A root environment consisting of predefined functions and types can be created with the
aid of a symbol table.
--}


-- Finds in an environment the non-name type to which a name type ultimately refers.
-- Returns a TypeError if the name type cycles, an UndefType error if some name type
-- refers to nothing.
resolveNameType :: (MonadError TypeError m) => (Type, Pos) -> Environment -> m Type
resolveNameType (typ, pos) env =
    go (Set.insert (getTypeId typ) Set.empty) typ
    where go cycles typ =
              case typ of
                NameType inName _ (Just references) ->
                    if Set.member references cycles
                    then throwError $ CircularType pos
                    else case lookupTypeById references env of
                           Nothing -> throwError $ UndefType inName pos
                           Just next -> go (Set.insert references cycles) next
                NameType inName _ Nothing -> throwError $ UndefType inName pos
                someType -> return someType


data Environment = Environment {vEnv :: M.Map S.Symbol EnvEntry,
                                tEnv :: M.Map S.Symbol Type,
                                idEnv :: M.Map TypeId Type,
                                nextTypeId :: TypeId}
                   deriving (Show)

data EnvEntry = VarEntry Type
              | FunEntry [Type] Type
                deriving (Show)

traverseEnvEntry :: (Monad m) => (Type -> m Type) -> EnvEntry -> m EnvEntry
traverseEnvEntry f entry =
    case entry of
      VarEntry t -> f t >>= return . VarEntry
      FunEntry ts t -> do
                 ts' <- mapM f ts
                 t' <- f t
                 return $ FunEntry ts' t'

-- resolve name types
lookupVar :: (MonadError TypeError m) => (S.Symbol, Pos) -> Environment -> m EnvEntry
lookupVar (sym, pos) env = case M.lookup sym $ vEnv env of
                    Nothing -> throwError $ UndefVar sym pos
                    Just envEntry -> traverseEnvEntry
                                     (\typ -> resolveNameType (typ, pos) env)
                                     envEntry

-- resolve name types
lookupType :: (MonadError TypeError m) => (S.Symbol, Pos) -> Environment -> m Type
lookupType (sym, pos) env =
    case M.lookup sym (tEnv env) of
      Nothing -> throwError $ UndefType sym pos
      Just t -> resolveNameType (t, pos) env

-- don't resolve name types
lookupFirstType :: (MonadError TypeError m) => (S.Symbol, Pos) -> Environment -> m Type
lookupFirstType (sym, pos) env =
    case M.lookup sym (tEnv env) of
      Nothing -> throwError $ UndefType sym pos
      Just t -> return t

lookupTypeById :: TypeId -> Environment -> Maybe Type
lookupTypeById id env = M.lookup id (idEnv env)

bindVar :: S.Symbol -> Type ->  ST.State Environment ()
bindVar s t = do
  env <- ST.get
  ST.put $ env {vEnv = M.insert s (VarEntry t) (vEnv env)}

bindFun :: S.Symbol -> [Type] -> Type -> ST.State Environment ()
bindFun s paramTypes retType = do
  env <- ST.get
  ST.put $ env {vEnv = M.insert s (FunEntry paramTypes retType) (vEnv env)}

bindType :: S.Symbol -> Type -> ST.State Environment ()
bindType s t = do
  env <- ST.get
  ST.put $ env {tEnv = M.insert s t (tEnv env)}

bindTypeToId :: TypeId -> Type -> ST.State Environment ()
bindTypeToId id t = do
  env <- ST.get
  ST.put $ env {idEnv = M.insert id t (idEnv env)}

incId :: ST.State Environment ()
incId = do
  env <- ST.get
  let (TypeId ltid) = nextTypeId env
  ST.put $ env {nextTypeId = TypeId $ 1 + ltid}

createNameType :: (Maybe TypeId) -> S.Symbol -> ST.State Environment Type
createNameType references name = do
  state <- ST.get
  let id = nextTypeId state
      nameType = NameType name id references
  incId
  bindTypeToId id nameType
  bindType name nameType
  return $ nameType

createArrayType :: Type -> ST.State Environment Type
createArrayType elemType = do
  state <- ST.get
  let id = nextTypeId state
      arrayType = ArrayType elemType id
  incId
  bindTypeToId id arrayType
  return $ arrayType

createRecordType :: [S.Symbol] -> [Type] -> ST.State Environment Type
createRecordType fieldNames fieldTypes = do
  state <- ST.get
  let id = nextTypeId state
      recType = RecordType (zip fieldNames fieldTypes) id
  incId
  bindTypeToId id recType
  return $ recType

rootEnv :: ST.State S.SymbolTable Environment
rootEnv = do
  let predefinedFuns=  [("print", FunEntry [StrType] UnitType),
                        ("printInt", FunEntry [IntType] UnitType),
                        ("readStr", FunEntry [IntType] StrType),
                        ("readInt", FunEntry [] IntType),
                        --("flush", FunEntry [] UnitType),
                        --("ord", FunEntry [StrType] IntType),
                        --("chr", FunEntry [IntType] StrType),
                        ("size", FunEntry [StrType] IntType),
                        ("substring", FunEntry [StrType, IntType, IntType] StrType),
                        ("concat", FunEntry [StrType, StrType] StrType),
                        ("not", FunEntry [IntType] IntType),
                        ("exit", FunEntry [IntType] UnitType)]
  intSym <- S.intern "int"
  strSym <- S.intern "string"
  predefinedBySyms <- mapM (\(name, entry) ->
                                S.intern name >>= (\n -> return (n, entry)))
                      predefinedFuns
  return Environment { tEnv = M.fromList [(intSym, IntType), (strSym, StrType)]
                     , vEnv = M.fromList predefinedBySyms
                     , idEnv = M.fromList $ map (\t -> (getTypeId t, t))
                              [IntType, StrType, UnitType, NilType]
                     , nextTypeId = minTypeId}
