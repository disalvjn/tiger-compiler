-- This lets me write functions that return either Either TypeError a or ExceptT TypeError m
{-# LANGUAGE FlexibleContexts #-}

module Semant(typecheck, rootEnv, Type(..), TypeError(..)) where
import AST
import Lex(Pos)
import qualified Symbol as S
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Control.Monad.State.Lazy as ST
import Control.Monad.Except as E

{--   TYPES

   There are four terminal types - int, str, nil, and unit - and three non-terminal
types  - arrays, records, and names. In Tiger, structurually equivalent records/arrays
are not necessarily equal. Each expression like 'type intlist = {head : int, tail : intlist}'
creates a new, unique type. Unique IDs (ints) are used to identify types. Also, NameTypes
(like 'type count = int') store the ID of the type to which they refer.

   Types are equal if their IDs are equal, with the exception that any record type
'equals' nil, since Tiger allows any instance of a record type to be nil.

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
    RecordType _ _ == NilType = True
    NilType == RecordType _ _ = True
    t1 == t2 = getTypeId t1 == getTypeId t2

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

{--  ENVIRONMENTS

  An environment consists of variable and type environments. Types are mapped to
from both names (S.Symbol) and ids (TypeId). New Non-terminal types are created inside
of an environment, which assigns them unique IDs.

  Most lookup functions resolve name types.

  A root environment consisting of predefined functions and types can be created with the
aid of a symbol table.
--}


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
                        ("flush", FunEntry [] UnitType),
                        ("getchar", FunEntry [] StrType),
                        ("ord", FunEntry [StrType] IntType),
                        ("chr", FunEntry [IntType] StrType),
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

{-- ANNOTATION

  The core of this module - the type checking. What to say?

--}

-- Exps, Vars, and Tys will have types associated with them.
-- Decs and Fundecs won't, since they have no type.
-- Tys have types because they cause new types to be created.
type TypedExp = Exp (Pos,Type) (Pos,Type) Pos (Pos,Type) Pos
type TypedVar = Var (Pos,Type) (Pos,Type) Pos (Pos,Type) Pos
type TypedDec = Dec (Pos,Type) (Pos,Type) Pos (Pos,Type) Pos
type TypedTy = Ty (Pos,Type)
type TypedFundec = Fundec (Pos,Type) (Pos,Type) Pos (Pos,Type) Pos

safeLast :: [a] -> Maybe a
safeLast list = if null list then Nothing else Just $ last list

mustBe :: (MonadError TypeError m) => (Type, Pos) -> Type -> m Type
mustBe (actual,pos) expected =
    if expected == actual
    then return actual
    else throwError $ WrongType expected actual pos

asMust :: (MonadError TypeError m) => m Type -> (Type, Pos) -> m Type
asMust expected actual = expected >>= mustBe actual

-- a little syntactic sugar
typedExp pos typ node = return $ Exp ((pos, typ), node)
typedVar pos typ node = return $ Var ((pos, typ), node)

typecheck :: Environment -> PosExp -> Either TypeError TypedExp
typecheck = annotateExp

annotateExp ::  (MonadError TypeError m) => Environment -> PosExp -> m TypedExp
annotateExp env (Exp (pos, exp)) =
    case exp of
      VarExp v -> do
               annVar@(Var ((_, vType), _)) <- annotateVar env v
               typedExp pos vType (VarExp annVar)

      NilExp -> typedExp pos NilType NilExp

      IntExp i -> typedExp pos IntType $ IntExp i

      StringExp s -> typedExp pos StrType $ StringExp s

      -- All OpExps evaluate to ints. <> and = are defined for any identical types.
      -- All other binops are defined only for ints.
      OpExp left oper right -> do
               annL@(Exp ((lp, lType), _)) <- annotateExp env left
               annR@(Exp ((rp, rType), _)) <- annotateExp env right
               if (oper == EqOp) || (oper == NeqOp)
               then do
                 -- = and <> work on any identical types
                 (rType,rp) `mustBe` lType
                 typedExp pos IntType $ OpExp annL oper annR
               else do
                 -- all other binops work only on integers
                 (lType, lp) `mustBe` IntType `asMust` (rType, rp)
                 typedExp pos IntType $ OpExp annL oper annR

      -- When calling f(a1, ..., an), where f has formal parameters p1, ..., pk :
      -- check n = k
      -- check that type(ai) = type(pi)
      -- check that type(body of f) = declared return type of f
      -- the type of the expression is f's return type
      CallExp f args -> do
               fType <- lookupVar (f, pos) env
               case fType of
                 VarEntry typ -> throwError $ ExpectedFunction f typ pos
                 FunEntry paramTypes retType -> do
                            let (paramCount, argCount) = (length paramTypes, length args)
                            -- ensure the function is being called with the proper number of args
                            when (paramCount /= argCount)
                                     (throwError $ WrongArity f paramCount argCount pos)
                            annArgs <- mapM (annotateExp env) args
                            -- ensure the arg types match the declared param types
                            zipWithM_ (\ (Exp ((pos, typ), _)) expectedType ->
                                       (typ, pos) `mustBe` expectedType) annArgs paramTypes
                            -- the type of a function call is the fn's return type
                            typedExp pos retType $ CallExp f annArgs

      -- For rectyp {f1 = v1, ..., fn = vn}, where rectyp was declared {f'1 = t1, ..., f'k = tk} :
      -- Check k = n [to do]
      -- Check fi = f'i (names match) [to do]
      -- Check type(vi) = ti
      -- The expression has record's type
      RecordExp fields typ -> do
               let (fieldNames, fieldExps) = unzip fields
               recordType <- lookupType (typ, pos) env
               annFieldVals <- mapM (annotateExp env) fieldExps
               case recordType of
                 RecordType fields _ -> do
                                 let (_, fieldTypes) = unzip fields
                                 -- A record's field types may be references to other types -
                                 -- including the record itself. Get the types to which they refer.
                                 resolvedFieldTypes <- mapM (\t -> resolveNameType (t, pos) env)
                                                       fieldTypes
                                 -- Ensure the field init values' types match the declared types.
                                 zipWithM_ (\ (Exp ((pos, actType), _)) expected ->
                                            (actType,pos) `mustBe` expected)
                                           annFieldVals resolvedFieldTypes
                                 -- Also check the names?
                                 -- Also check arity?
                                 typedExp pos recordType
                                         $ RecordExp (zip fieldNames annFieldVals) typ
                 _ -> throwError $ NotARecord typ pos

      -- An empty SeqExp has type UnitType. A non-empty one has the type of its last exp.
      SeqExp exps -> do
               annExps <- mapM (annotateExp env) exps
               case safeLast annExps of
                 Just (Exp ((_, lastType), _)) -> typedExp pos lastType $ SeqExp annExps
                 _ -> typedExp pos UnitType $ SeqExp annExps

      AssignExp lvar rval -> do
               annExp@(Exp ((epos, expType), _)) <- annotateExp env rval
               annVar@(Var ((vpos, varType), _)) <- annotateVar env lvar
               (expType, epos) `mustBe` varType
               typedExp pos UnitType $ AssignExp annVar annExp

      -- for 'if p then c else a' : type(c) = type(a). For 'if p then c', type(c) = Unit.
      -- type(p) = Int, always.
      IfExp pred conseq alt -> do
               annPred@(Exp ((ppos, predType), _)) <- annotateExp env pred
               annConseq@(Exp ((cpos, conType), _)) <- annotateExp env conseq
               (predType, ppos) `mustBe` IntType
               case alt of
                 Just a -> do
                          annAlt@(Exp ((apos, aType), _)) <- annotateExp env a
                          (conType, cpos) `mustBe` aType
                          typedExp pos aType $ IfExp annPred annConseq (Just annAlt)
                 Nothing -> do
                          (conType, cpos) `mustBe` UnitType
                          typedExp pos UnitType $ IfExp annPred annConseq Nothing

      -- Body has type of unit. Test has type of int. Whole exp has type of unit.
      WhileExp test body -> do
               annTest@(Exp ((tpos, tType), _)) <- annotateExp env test
               annBody@(Exp ((bpos, bType), _)) <- annotateExp env body
               (tType, tpos) `mustBe` IntType
               (bType, bpos) `mustBe` UnitType
               typedExp pos UnitType $ WhileExp annTest annBody

      -- Bind sym to an int. Lo and Hi must be ints. Body must be of unit type.
      ForExp sym lo hi body ->
          let (_, env') = ST.runState (bindVar sym IntType) env
          in do
            annLo@(Exp ((lpos, lType), _)) <- annotateExp env' lo
            annHi@(Exp ((hpos, hType), _)) <- annotateExp env' hi
            annBody@(Exp ((bpos, bType), _)) <- annotateExp env' body
            (lType, lpos) `mustBe` IntType `asMust` (hType, hpos)
            (bType, bpos) `mustBe` UnitType
            typedExp pos UnitType $ ForExp sym annLo annHi annBody

      BreakExp  -> typedExp pos UnitType BreakExp

      -- Extend the environment by processing the decs and annotate the body in the new env.
      -- The body is treated as a SeqExp (always parsed as one).
      LetExp decs body -> do
               let (newDecs, newScope) = ST.runState (E.runExceptT (mapM annotateDec decs)) env
               case newDecs of
                 Left err -> throwError err
                 Right annDecs -> do
                              annBody@(Exp ((_, bodyType), _)) <- annotateExp newScope body
                              typedExp pos bodyType $ LetExp annDecs annBody

      -- type(init) = elemType(typ)
      -- type(size) = int
      -- the whole exp has the array's type.
      ArrayExp typ size init -> do
               annSize@(Exp ((spos, sType), _)) <- annotateExp env size
               annInit@(Exp ((ipos, iType), _)) <- annotateExp env init
               arrayType <- lookupType (typ, pos) env
               (sType, spos) `mustBe` IntType
               case arrayType of
                 ArrayType elemType _ -> do
                                  realType <- resolveNameType (elemType, pos) env
                                  (iType, ipos) `mustBe` realType
                                  typedExp pos arrayType $ ArrayExp typ annSize annInit
                 other -> throwError $ ExpectedArray arrayType pos


annotateVar :: (MonadError TypeError m) => Environment -> PosVar -> m TypedVar
annotateVar env (Var (pos, var)) =
    case var of
      -- A lone variable has the type to which it is bound in the environment.
      -- However, the variable can't be a function, since functions are ironically plebians.
      SimpleVar sym -> do
               varType <- lookupVar (sym, pos) env
               case varType of
                 VarEntry simpleType -> typedVar pos simpleType $ SimpleVar sym
                 FunEntry _ retType -> throwError $ EveryoneKnowsFunctionsArentValues sym pos

      -- For r.x, r must be a record with field x. The var has the type of field x.
      FieldVar v sym -> do
               annVar@(Var ((varPos, varType), _)) <- annotateVar env v
               case varType of
                 RecordType fields _ -> maybe
                                        (throwError $ UndefField sym varPos)
                                        (\fieldType -> typedVar pos fieldType $ FieldVar annVar sym)
                                        (lookup sym fields)
                 _ -> throwError $ ExpectedRecord sym varType pos

      -- for a[i], a must be an array. i must be an int. The var has type of the array's elements.
      SubscriptVar v exp -> do
               annExp@(Exp ((expPos, expType), _)) <- annotateExp env exp
               annVar@(Var ((_, varType), _)) <- annotateVar env v
               (expType, expPos) `mustBe` IntType
               case varType of
                 ArrayType elemType _ -> typedVar pos elemType $ SubscriptVar annVar annExp
                 _ -> throwError $ ExpectedArray varType pos

anyDuplicates :: Ord a => [a] -> Maybe a
anyDuplicates list =
    let go list set =
            case list of
              [] -> Nothing
              (x:xs) -> if Set.member x set
                        then Just x
                        else go xs $ Set.insert x set
    in go list Set.empty

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust x f = maybe (return ()) f x

annotateDec :: PosDec -> (E.ExceptT TypeError (ST.State Environment)) TypedDec
annotateDec (Dec (pos, dec)) =
    case dec of
      -- The strategy is to first get the names, param types, and ret type of
      -- each function and extend the environment with them. Only then do we
      -- annotate the function bodies. This allows functions in the same block
      -- to be mutually recursive.
      FunDec fundecs -> do
               env <- lift ST.get
               funcTypes <- mapM (funcTypeNamePair env) fundecs
               let (fNames, fParamTypes, fRetTypes) = unzip3 funcTypes
               -- a fn can be defined only once per block
               whenJust (anyDuplicates fNames) (\dup -> throwError $ MultipleDeclarations dup pos)
               -- extend the environment
               lift $ mapM_ (\(name, paramTypes, retType) -> bindFun name paramTypes retType)
                    funcTypes
               env <- lift ST.get
               -- Annotate the bodies of the functions
               processedFunDecs <- zipWithM (\ fundec paramTypes ->
                                                 (annotateFunDec fundec paramTypes env))
                                   fundecs fParamTypes
               let (annFundecs, annBodies) = unzip processedFunDecs
               -- Verify that the type of each function's body is equal to the function's
               -- declared return type.
               zipWithM_  (\(_, _, retType) (Exp ((pos, typ), _)) ->
                           (typ, pos) `mustBe` retType) funcTypes annBodies
               return $ Dec (pos, FunDec annFundecs)

      -- For 'var x : t := y', t = type(y), and the environment is extended
      -- with x -> t. For 'var x := y', x -> type(y).
      VarDec name typ init ->
          do env <- lift ST.get
             annInit@(Exp ((ipos, iType), _)) <- annotateExp env init
             let makeResult varType = do
                   (iType, ipos) `mustBe` varType
                   lift $ bindVar name varType
                   return $ Dec (pos, VarDec name typ annInit)
             case typ of
               Nothing -> makeResult iType
               Just expectedType -> lookupType (expectedType, pos) env >>= makeResult

      -- The strategy: for each type being declared, create a dummy name type that
      -- references nothing and extend the environment with each name -> dummyName.
      -- Then evaluate each type decleration (creating new record, array, and name types)
      -- in this env. Unfortunately, if types are mutually recursive, then the dummy name
      -- appears in the new type. Consider:
      -- type intlist = {head : int, tail : intlist}. This initially creates an env with:
      -- {intlist -> NameType intlist 0 Nothing} (its ID is 0).
      -- Then we create the new record type, which is:
      -- RecordType [(head, IntType), (tail, NameType intlist 0 Nothing)] 1
      -- We need to: associate intlist's nametype with the new record type, and associate
      -- the nametype inside of the record type with the record type.
      -- So, we build a map from the dummy name type ids to the newly created types.
      -- In this case, {0 -> RecordType ... }
      -- Update EVERY name type, among both the dummies and the new types, to reference
      -- the correct types using this map. Then, re-extend the environment with these updated
      -- types. Finally, trace every name type to a non-name type (but not recursively --
      -- otherwise you get an infinite loop) to ensure that all type cycles 'go through'
      -- a record or array.
      TypeDec typedecs -> do
               let (names, types) = unzip typedecs
               -- a type can be defined only once in a block
               whenJust (anyDuplicates names) (\dup -> throwError $ MultipleDeclarations dup pos)
               dummyNameTypes <- lift $ mapM (createNameType Nothing) names
               annTys <- mapM annotateTy types
               let rawTypes = map (\(Ty ((_, ty), _)) -> ty) annTys
                   nameIds = map getTypeId dummyNameTypes
                   nameIdsToTypes = M.fromList $ zip nameIds rawTypes
                   updatedNames = map (associateNameTypes nameIdsToTypes) dummyNameTypes
                   updatedRawTypes = map (associateNameTypes nameIdsToTypes) rawTypes
                   fullTypeDecs = zipWith (\ (Ty ((pos, _), ty)) realType ->
                                            Ty ((pos, realType), ty))
                                  annTys updatedRawTypes
               lift $ mapM_ (\typ -> bindTypeToId (getTypeId typ) typ) updatedNames
               env <- lift $ ST.get
               -- ensure every cycle goes through a record or array
               returnTypeDec <- mapM (\((Ty ((pos, typ), t))) -> do
                                          newType <- resolveNamesAtFirstLevel (typ, pos) env
                                          return $ Ty ((pos, newType), t))
                                fullTypeDecs
               lift $ mapM_ (\(Ty ((_, typ), _)) -> bindTypeToId (getTypeId typ) typ) returnTypeDec
               lift $ zipWithM_ (\ name typ -> bindType name typ) names updatedNames
               return $ Dec (pos, TypeDec $ zip names returnTypeDec)

funcTypeNamePair :: MonadError TypeError m =>
     Environment -> PosFundec -> m (S.Symbol, [Type], Type)
funcTypeNamePair env (Fundec (pos, FundecF name params result _)) = do
  paramTypes <- mapM (\(Field fname ftype) -> (lookupType (ftype, pos) env))  params
  retType <- maybe (return UnitType) (\typName -> lookupType (typName, pos) env) result
  return $ (name, paramTypes, retType)

-- Annotate the fundec's body, returning a pair of the ann'd fundec and ann'd body.
annotateFunDec :: MonadError TypeError m =>
                  PosFundec -> [Type] -> Environment -> m (TypedFundec, TypedExp)
annotateFunDec (Fundec (pos, FundecF name params result body)) paramTypes env = do
  let (_, envWithParams) = ST.runState (zipWithM (\name typ -> bindVar name typ)
                                                 (map fieldName params) paramTypes)
                           env
  annBody <- annotateExp envWithParams body
  return $ (Fundec (pos, FundecF name params result annBody), annBody)

associateNameTypes :: M.Map TypeId Type -> Type -> Type
associateNameTypes mapping typ =
    case typ of
      NameType sym id Nothing ->
          NameType sym id $ fmap getTypeId (M.lookup id mapping)
      other -> mapType (associateNameTypes mapping) other

resolveNamesAtFirstLevel (typ, pos) env =
    case typ of
      NameType _ _ _ -> resolveNameType (typ, pos) env
      other -> mapMType (\t -> resolveNamesAtFirstLevel (t, pos) env) other

-- A side effect of annotating a ty is created a new type.
annotateTy :: PosTy -> E.ExceptT TypeError (ST.State Environment) TypedTy
annotateTy (Ty (pos, ty)) =
  case ty of
    NameTy sym -> do
            env <- lift ST.get
            refType <- lookupFirstType (sym, pos) env
            return $ Ty ((pos, refType), NameTy sym)

    ArrayTy sym -> do
            env <- lift ST.get
            elemType <- lookupFirstType (sym,pos) env
            arrayType <- lift $ createArrayType elemType
            return $ Ty ((pos, arrayType), ArrayTy sym)

    RecordTy fields -> do
            let (fieldNames, fieldTypes) = (map fieldName fields, map fieldTyp fields)
            env <- lift ST.get
            actualTypes <- mapM (\t -> lookupFirstType (t, pos) env) fieldTypes
            recType <- lift $ createRecordType fieldNames actualTypes
            return $ (Ty ((pos, recType), RecordTy fields))
