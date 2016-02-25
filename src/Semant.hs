{-# LANGUAGE FlexibleContexts #-}

module Semant(typecheck, rootEnv) where
import AST
import Lex(Pos)
import qualified Symbol as S
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Control.Monad.State.Lazy as ST
import Debug.Trace(trace)
import Control.Monad.Except as E

{--
  TYPES
--}
newtype TypeId = TypeId {typeId :: Int} deriving (Show, Eq, Ord)

intTypeId = TypeId 0
strTypeId = TypeId 1
nilTypeId = TypeId 2
unitTypeId = TypeId 3
minTypeId = TypeId 4

data Type = IntType
          | StrType
          | NilType
          | UnitType
          | NameType S.Symbol TypeId (Maybe TypeId)
          | RecordType [(S.Symbol, Type)] TypeId
          | ArrayType Type TypeId
            deriving (Show)

getTypeId IntType = intTypeId
getTypeId StrType = strTypeId
getTypeId NilType = nilTypeId
getTypeId UnitType = unitTypeId
getTypeId (NameType _ id _) = id
getTypeId (RecordType _ id) = id
getTypeId (ArrayType _ id) = id

instance Eq Type where
    IntType == IntType = True
    StrType == StrType = True
    NilType == NilType = True
    UnitType == UnitType = True
    RecordType _ id1 == RecordType _ id2 = id1 == id2
    ArrayType _ id1 == ArrayType _ id2 = id1 == id2
    NameType _ _ (Just t1) == NameType _ _ (Just t2) = t1 == t2
    _ == _ = False

resolveNameType :: (MonadError TypeError m) => (Type, S.Symbol, Pos) -> Environment -> m Type
resolveNameType (typ, name, pos) env =
    go (Set.insert (getTypeId typ) Set.empty) typ
    where go cycles typ =
              case typ of
                NameType inName _ (Just references) ->
                    if Set.member references cycles
                    then throwError $ CircularType name pos
                    else case lookupTypeById references env of
                           Nothing -> throwError $ UndefType inName pos
                           Just next -> go (Set.insert references cycles) next
                NameType inName _ Nothing -> throwError $ UndefType inName pos
                someType -> return $ someType


{--
  ENVIRONMENTS
--}

data EnvEntry = VarEntry Type
              | FunEntry [Type] Type
                deriving (Show)

traverseEnvEntry :: (Monad m) => (Type -> m Type) -> EnvEntry -> m EnvEntry
traverseEnvEntry f (VarEntry t) = do
  t' <- f t
  return $ VarEntry t'

traverseEnvEntry f (FunEntry ts t) = do
  ts' <- mapM f ts
  t' <- f t
  return $ FunEntry ts' t'


data Environment = Environment {vEnv :: M.Map S.Symbol EnvEntry,
                                tEnv :: M.Map S.Symbol Type,
                                idEnv :: M.Map TypeId Type,
                                lastTypeId :: TypeId}
                   deriving (Show)

lookupVar :: (MonadError TypeError m) => (S.Symbol, Pos) -> Environment -> m EnvEntry
lookupVar (sym, pos) env = case M.lookup sym $ vEnv env of
                    Nothing -> throwError $ UndefVar sym pos
                    Just envEntry -> traverseEnvEntry
                                     (\typ -> resolveNameType (typ, sym, pos) env)
                                     envEntry

lookupType :: (MonadError TypeError m) => (S.Symbol, Pos) -> Environment -> m Type
lookupType (sym, pos) env =
    case M.lookup sym (tEnv env) of
      Nothing -> throwError $ UndefType sym pos
      Just t -> resolveNameType (t, sym, pos) env

lookupFirstType :: (MonadError TypeError m) => (S.Symbol, Pos) -> Environment -> m Type
lookupFirstType (sym, pos) env =
    case M.lookup sym (tEnv env) of
      Nothing -> throwError $ UndefType sym pos
      Just t -> return $ t

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
  let (TypeId ltid) = lastTypeId env
  ST.put $ env {lastTypeId = TypeId $ 1 + ltid}

rootEnv :: ST.State S.SymbolTable Environment
rootEnv = do
  intSym       <- S.intern "int"
  strSym       <- S.intern "string"
  printSym     <- S.intern "print"
  flushSym     <- S.intern "flush"
  getcharSym   <- S.intern "getchar"
  ordSym       <- S.intern "ord"
  chrSym       <- S.intern "chr"
  sizeSym      <- S.intern "size"
  substringSym <- S.intern "substring"
  concatSym    <- S.intern "concat"
  notSym       <- S.intern "not"
  exitSym      <- S.intern "exit"
  let tEnv = M.fromList [(intSym, IntType), (strSym, StrType)]
      vEnv = M.fromList [(printSym     , FunEntry [StrType] UnitType),
                         (flushSym     , FunEntry [] UnitType),
                         (getcharSym   , FunEntry [] StrType),
                         (ordSym       , FunEntry [StrType] IntType),
                         (chrSym       , FunEntry [IntType] StrType),
                         (sizeSym      , FunEntry [StrType] IntType),
                         (substringSym , FunEntry [StrType, IntType, IntType] StrType),
                         (concatSym    , FunEntry [StrType, StrType] StrType),
                         (notSym       , FunEntry [IntType] IntType),
                         (exitSym      , FunEntry [IntType] UnitType)]
      lastTypeId = minTypeId
      idEnv = M.fromList $ map (\t -> (getTypeId t, t)) [IntType, StrType, UnitType, NilType]
  return $ Environment vEnv tEnv idEnv lastTypeId

{--
  ANNOTATIONS
--}

data TypeError = UndefVar S.Symbol Pos
               | UndefType S.Symbol Pos
               | UndefField S.Symbol Pos
               | CircularType S.Symbol Pos
               | WrongType Type Type Pos -- expected, actual
                 deriving (Show)

anyArrayType = ArrayType NilType $ TypeId (-1)
anyRecordType = RecordType [] $ TypeId (-1)
recordWithField sym = RecordType [(sym, NilType)] $ TypeId (-1)

type TypedExp = Exp (Pos,Type) (Pos,Type) (Pos,Type) (Pos,Type) (Pos,Type)
type TypedVar = Var (Pos,Type) (Pos,Type) (Pos,Type) (Pos,Type) (Pos,Type)
type TypedDec = Dec (Pos,Type) (Pos,Type) (Pos,Type) (Pos,Type) (Pos,Type)
type TypedTy = Ty (Pos,Type)
type TypedFundec = Fundec (Pos,Type) (Pos,Type) (Pos,Type) (Pos,Type) (Pos,Type)

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_:xs) = safeLast xs

mustBe :: (MonadError TypeError m) => (Type, Pos) -> Type -> m Type
mustBe (actual,pos) expected =
    if expected == actual
    then return actual
    else throwError $ WrongType expected actual pos

asMust :: (MonadError TypeError m) => m Type -> (Type, Pos) -> m Type
asMust expected actual =
    expected >>= mustBe actual

typedExp pos typ node = return $ Exp ((pos, typ), node)
typedVar pos typ node = return $ Var ((pos, typ), node)
typedDec pos typ node = return $ Dec ((pos, typ), node)

typecheck :: PosExp -> Environment -> Either TypeError TypedExp
typecheck = annotateExp

annotateExp ::  (MonadError TypeError m) => PosExp -> Environment -> m TypedExp
annotateExp (Exp (pos, exp)) env =
    case exp of
      VarExp v -> do
               annVar@(Var ((_, vType), _)) <- annotateVar v env
               typedExp pos vType (VarExp annVar)

      NilExp -> typedExp pos NilType NilExp

      IntExp i -> typedExp pos IntType $ IntExp i

      StringExp s -> typedExp pos StrType $ StringExp s

      OpExp left oper right -> do
               annL@(Exp ((lp, lType), _)) <- annotateExp left env
               annR@(Exp ((rp, rType), _)) <- annotateExp right env
               if (oper == EqOp) || (oper == NeqOp)
               then do
                 (rType,rp) `mustBe` lType
                 typedExp pos lType $ OpExp annL oper annR
               else do
                 (lType, lp) `mustBe` IntType `asMust` (rType, rp)
                 typedExp pos IntType $ OpExp annL oper annR

      -- RecordExp

      SeqExp exps -> do
               annExps <- mapM (\exp -> annotateExp exp env) exps
               case (safeLast annExps) of
                 Just (Exp ((_, lastType), _)) -> typedExp pos lastType $ SeqExp annExps
                 _ -> typedExp pos UnitType $ SeqExp annExps

      AssignExp lvar rval -> do
               annExp@(Exp ((epos, expType), _)) <- annotateExp rval env
               annVar@(Var ((vpos, varType), _)) <- annotateVar lvar env
               (expType, epos) `mustBe` varType
               typedExp pos UnitType $ AssignExp annVar annExp

      IfExp pred conseq alt -> do
               annPred@(Exp ((ppos, predType), _)) <- annotateExp pred env
               annConseq@(Exp ((cpos, conType), _)) <- annotateExp conseq env
               (predType, ppos) `mustBe` IntType
               case alt of
                 (Just a) -> do
                          annAlt@(Exp ((apos, aType), _)) <- annotateExp a env
                          (conType, cpos) `mustBe` aType
                          typedExp pos aType $ IfExp annPred annConseq (Just annAlt)
                 Nothing -> do
                          (conType, cpos) `mustBe` UnitType
                          typedExp pos UnitType $ IfExp annPred annConseq Nothing

      WhileExp test body -> do
               annTest@(Exp ((tpos, tType), _)) <- annotateExp test env
               annBody@(Exp ((bpos, bType), _)) <- annotateExp body env
               (tType, tpos) `mustBe` IntType
               (bType, bpos) `mustBe` UnitType
               typedExp pos UnitType $ WhileExp annTest annBody

      ForExp sym lo hi body ->
          let (_, env') = ST.runState (bindVar sym IntType) env
          in do
            annLo@(Exp ((lpos, lType), _)) <- annotateExp lo env'
            annHi@(Exp ((hpos, hType), _)) <- annotateExp hi env'
            annBody@(Exp ((bpos, bType), _)) <- annotateExp body env'
            (lType, lpos) `mustBe` IntType `asMust` (hType, hpos)
            (bType, bpos) `mustBe` UnitType
            typedExp pos UnitType $ ForExp sym annLo annHi annBody

      BreakExp  -> typedExp pos UnitType BreakExp

      LetExp decs body -> do
               let (newDecs, newScope) = ST.runState (E.runExceptT (mapM annotateDec decs)) env
               case newDecs of
                 Left err -> throwError err
                 Right annDecs -> do
                              annBody@(Exp ((_, bodyType), _)) <-
                                  annotateExp body (trace (show newScope) newScope)
                              typedExp pos bodyType $ LetExp annDecs annBody

      ArrayExp typ size init -> do
               annSize@(Exp ((spos, sType), _)) <- annotateExp size env
               annInit@(Exp ((ipos, iType), _)) <- annotateExp init env
               arrayType <- lookupType (typ, pos) env
               (sType, spos) `mustBe` IntType --`asMust` (iType, ipos)
               case arrayType of
                 ArrayType elemType _ -> do
                                  realType <- resolveNameType (elemType, typ, pos) env
                                  (iType, ipos) `mustBe` realType
                                  typedExp pos arrayType $ ArrayExp typ annSize annInit
                 other -> throwError $ WrongType anyArrayType arrayType pos


annotateVar :: (MonadError TypeError m) => PosVar -> Environment -> m TypedVar
annotateVar (Var (pos, var)) env =
    case var of
      SimpleVar sym -> do
               varType <- lookupVar (sym, pos) env
               case varType of
                 VarEntry simpleType -> typedVar pos simpleType $ SimpleVar sym
                 FunEntry _ retType -> typedVar pos retType $ SimpleVar sym

      FieldVar v sym -> do
               annVar@(Var ((varPos, varType), _)) <- annotateVar v env
               case varType of
                 RecordType fields _ -> maybe
                                        (throwError $ UndefField sym varPos)
                                        (\fieldType -> typedVar pos fieldType $ FieldVar annVar sym)
                                        (lookup sym fields)
                 _ -> throwError $ WrongType (recordWithField sym) varType pos

      SubscriptVar v exp -> do
               annExp@(Exp ((expPos, expType), _)) <- annotateExp exp env
               annVar@(Var ((_, varType), _)) <- annotateVar v env
               (expType, expPos) `mustBe` IntType
               case varType of
                 ArrayType elemType _ -> typedVar pos elemType $ SubscriptVar annVar annExp
                 _ -> throwError $ WrongType anyArrayType varType pos

annotateDec :: PosDec -> (E.ExceptT TypeError (ST.State Environment)) TypedDec
annotateDec (Dec (pos, dec)) =
    case dec of
      --FunDec fundecs ->

      VarDec name typ init ->
          do env <- lift ST.get
             annInit@(Exp ((ipos, iType), _)) <- annotateExp init env
             let makeResult varType = do
                   (iType, ipos) `mustBe` (trace (show varType) varType)
                   lift $ bindVar (trace (show name) name) varType
                   return $ Dec ((pos, varType), VarDec name typ annInit)
             case typ of
               Nothing -> makeResult iType
               Just expectedType -> do
                        et <- lookupType (expectedType, pos) env
                        makeResult (trace (show et) et)

      TypeDec typedecs -> do
          let (names, types) = unzip typedecs
          nameTypes <- lift $ mapM (createNameType Nothing) names
          processed <- mapM annotateTy types
          let rawTypes = map (\(Ty ((_, ty), _)) -> ty) processed
              nameIds = map getTypeId nameTypes
              mapping = M.fromList $ zip nameIds rawTypes
              updatedNames = map (associateNameTypes mapping) nameTypes
              updatedProcessed = map (associateNameTypes mapping) rawTypes
              returnTypeDec = map (\ ((Ty ((pos, _), ty)), realType) -> Ty ((pos, realType), ty)) $
                              zip processed updatedProcessed
          lift $ mapM_ (\typ -> bindTypeToId (getTypeId typ) typ) updatedNames
          lift $ mapM_ (\typ -> bindTypeToId (getTypeId typ) typ) updatedProcessed
          lift $ mapM_ (\(name, typ) -> bindType name typ) $ zip names updatedNames
          return $ Dec ((pos, UnitType), TypeDec $ zip names returnTypeDec)

associateNameTypes :: M.Map TypeId Type -> Type -> Type
associateNameTypes mapping typ =
    case typ of
      NameType sym id Nothing ->
          NameType sym id ((M.lookup id mapping) >>= Just . getTypeId)
      RecordType fields id ->
          RecordType (map (fmap $ associateNameTypes mapping) fields) id
      ArrayType t id -> ArrayType (associateNameTypes mapping t) id
      other -> other

annotateTy :: PosTy -> E.ExceptT TypeError (ST.State Environment) TypedTy
annotateTy (Ty (pos, NameTy sym)) = do
  env <- lift ST.get
  refType <- lookupFirstType (sym, pos) env
  return $ Ty ((pos, refType), NameTy sym)

annotateTy (Ty (pos, ArrayTy sym)) = do
  env <- lift ST.get
  elemType <- lookupFirstType (sym,pos) env
  arrayType <- lift $ createArrayType elemType
  return $ Ty ((pos, arrayType), ArrayTy sym)


createNameType :: (Maybe TypeId) -> S.Symbol -> ST.State Environment Type
createNameType references name = do
  state <- ST.get
  let id = lastTypeId state
      nameType = NameType name id references
  incId
  bindTypeToId id nameType
  bindType name nameType
  return $ nameType

createArrayType :: Type -> ST.State Environment Type
createArrayType elemType = do
  state <- ST.get
  let id = lastTypeId state
      arrayType = ArrayType elemType id
  incId
  bindTypeToId id arrayType
  return $ arrayType
