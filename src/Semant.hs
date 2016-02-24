module Semant() where
import AST
import Lex(Pos)
import qualified Symbol as S
import qualified Data.Map as M
import qualified Control.Monad.State.Lazy as ST
import Control.Monad.Except as E
import Control.Monad (foldM)

{--
  TYPES
--}
newtype TypeId = TypeId {typeId :: Int} deriving (Show, Eq)

data Type = IntType
          | StrType
          | NilType
          | UnitType
          | NameType S.Symbol (Maybe Type)
          | RecordType [(S.Symbol, Type)] TypeId
          | ArrayType Type TypeId
            deriving (Show)

instance Eq Type where
    IntType == IntType = True
    StrType == StrType = True
    NilType == NilType = True
    UnitType == UnitType = True
    NameType _ (Just t1) == NameType _ (Just t2) = t1 == t2
    NameType _ (Just t1) == t2 = (t1 == t2)
    t1 == NameType _ (Just t2) = t1 == t2
    RecordType types1 id1 == RecordType types2 id2 = (types1 == types2) && (id1 == id2)
    ArrayType t1 id1 == ArrayType t2 id2 = (t1 == t2) && (id1 == id2)
    _ == _ = False

resolveNameType :: Type -> Type
resolveNameType (NameType sym (Just t)) = resolveNameType t
resolveNameType t = t

{--
  ENVIRONMENTS
--}

data EnvEntry = VarEntry Type
              | FunEntry [Type] Type

data Environment = Environment {vEnv :: M.Map S.Symbol EnvEntry,
                                tEnv :: M.Map S.Symbol Type,
                                lastTypeId :: Int}

lookupVar :: S.Symbol -> Environment -> Maybe EnvEntry
lookupVar s env = M.lookup s $ vEnv env

lookupType :: S.Symbol -> Environment -> Maybe Type
lookupType s env = fmap resolveNameType $ M.lookup s (tEnv env)

bindVar :: S.Symbol -> Type -> Environment -> Environment
bindVar s t (Environment vEnv tEnv lastTypeId) =
    Environment (M.insert s (VarEntry t) vEnv) tEnv lastTypeId

bindFun :: S.Symbol -> [Type] -> Type -> Environment -> Environment
bindFun s paramTypes retType (Environment vEnv tEnv lastTypeId) =
    Environment (M.insert s (FunEntry paramTypes retType) vEnv) tEnv lastTypeId

bindType :: S.Symbol -> Type -> Environment -> Environment
bindType s t (Environment vEnv tEnv lastTypeId) =
    Environment vEnv (M.insert s t tEnv) lastTypeId

{--
  ANNOTATIONS
--}

data TypeError = UndefVar S.Symbol Pos
               | UndefType S.Symbol Pos
               | UndefField S.Symbol Pos
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

type AnnotationResults = (E.ExceptT TypeError (ST.State Environment)) TypedExp
type AR = Either TypeError (ST.State Environment TypedExp)

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_:xs) = safeLast xs

require :: (b -> Bool) -> b -> a -> Either a b
require pred res err = if (pred res) then Right res else Left err

mustBe :: (Type, Pos) -> Type -> Either TypeError Type
mustBe (actual,pos) expected =
    require (== expected) actual $ WrongType expected actual pos

asMust :: Either TypeError Type -> (Type, Pos) -> Either TypeError Type
asMust expected actual =
    expected >>= mustBe actual


-- addType :: PosExp -> Type -> Either a TypedExp
 -- addType (Exp (p, e)) typ = Right $ Exp ((p, typ), e)

typedExp pos typ node = Right $ Exp ((pos, typ), node)
typedVar pos typ node = Right $ Var ((pos, typ), node)
typedDec pos typ node = Right $ Dec ((pos, typ), node)


annotateExp :: PosExp -> Environment -> Either TypeError TypedExp
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
          let env' = bindVar sym IntType env
          in do
            annLo@(Exp ((lpos, lType), _)) <- annotateExp lo env'
            annHi@(Exp ((hpos, hType), _)) <- annotateExp hi env'
            annBody@(Exp ((bpos, bType), _)) <- annotateExp body env'
            (lType, lpos) `mustBe` IntType `asMust` (hType, hpos)
            (bType, bpos) `mustBe` UnitType
            typedExp pos UnitType $ ForExp sym annLo annHi annBody

      BreakExp  -> typedExp pos UnitType BreakExp
    -- LetExp
      ArrayExp typ size init -> do
               annSize@(Exp ((spos, sType), _)) <- annotateExp size env
               annInit@(Exp ((ipos, iType), _)) <- annotateExp init env
               (sType, spos) `mustBe` IntType `asMust` (iType, ipos)
               case lookupType typ env of
                 Nothing -> Left $ UndefType typ pos
                 Just t -> typedExp pos t $ ArrayExp typ annSize annInit


annotateVar :: PosVar -> Environment -> Either TypeError TypedVar
annotateVar (Var (pos, var)) env =
    case var of
      SimpleVar sym ->
          case lookupVar sym env of
            Just (VarEntry typ) -> typedVar pos (resolveNameType typ) $ SimpleVar sym
            Just (FunEntry _ retType) -> typedVar pos (resolveNameType retType) $ SimpleVar sym
            Nothing -> Left $ UndefVar sym pos

      FieldVar v sym -> do
               annVar@(Var ((varPos, varType), _)) <- annotateVar v env
               case resolveNameType varType of
                 RecordType fields _ -> maybe
                                        (Left $ UndefField sym varPos)
                                        (\fieldType -> typedVar pos fieldType $ FieldVar annVar sym)
                                        (lookup sym fields)
                 _ -> Left $ WrongType (recordWithField sym) varType pos

      SubscriptVar v exp -> do
               annExp@(Exp ((expPos, expType), _)) <- annotateExp exp env
               annVar@(Var ((_, varType), _)) <- annotateVar v env
               (expType, expPos) `mustBe` IntType
               case resolveNameType varType of
                 ArrayType elemType _ -> typedVar pos elemType $ SubscriptVar annVar annExp
                 _ -> Left $ WrongType anyArrayType varType pos

annotateDec :: PosDec -> Environment -> Either TypeError (Environment, TypedDec)
annotateDec (Dec (pos, dec)) env =
    case dec of
      --FunDec fundecs ->

      VarDec name typ init ->
          do annInit@(Exp ((ipos, iType), _)) <- annotateExp init env
             let makeResult varType = do
                   (iType, ipos) `mustBe` varType
                   return (bindVar name varType env, Dec ((pos, varType), VarDec name typ annInit))
             case typ of
               Nothing -> makeResult iType
               Just expectedType ->
                   maybe (Left $ UndefType expectedType pos)
                         makeResult
                         (lookupType expectedType env)

      -- TypeDec typedecs ->

--}
