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
            deriving (Show, Eq)


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

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_:xs) = safeLast xs

{--
  ANNOTATIONS
--}

data TypeError = UndefVar S.Symbol Pos
               | UndefType S.Symbol Pos
               | UndefField S.Symbol Pos
               | WrongType Type Type Pos -- expected, actual

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

require :: (b -> Bool) -> b -> a -> Either a b
require pred res err = if (pred res) then Right res else Left err

-- addType :: PosExp -> Type -> Either a TypedExp
 -- addType (Exp (p, e)) typ = Right $ Exp ((p, typ), e)

typedExp pos typ node = Right $ Exp ((pos, typ), node)
typedVar pos typ node = Right $ Var ((pos, typ), node)


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
               require (== IntType) lType $ WrongType IntType lType lp
               require (== IntType) rType $ WrongType IntType rType rp
               typedExp pos IntType (OpExp annL oper annR)
      -- RecordExp
      SeqExp exps -> do annExps <- mapM (\exp -> annotateExp exp env) exps
                        case (safeLast annExps) of
                          Just (Exp ((_, lastType), _)) -> typedExp pos lastType $ SeqExp annExps
                          _ -> typedExp pos UnitType $ SeqExp annExps
    -- AssignExp
    -- IfExp
    -- WhileExp
    -- ForExp
    -- BreakExp
    -- LetExp
    -- ArrayExp

annotateVar :: PosVar -> Environment -> Either TypeError TypedVar
annotateVar (Var (pos, var)) env =
    case var of
      SimpleVar sym -> case lookupVar sym env of
                         Just (VarEntry typ) -> typedVar pos typ $ SimpleVar sym
                         Just (FunEntry _ retType) -> typedVar pos retType $ SimpleVar sym
                         Nothing -> Left $ UndefVar sym pos
      FieldVar v sym -> do
               annVar@(Var ((varPos, varType), _)) <- annotateVar v env
               case varType of
                 RecordType fields _ -> maybe
                                        (Left $ UndefField sym varPos)
                                        (\fieldType -> typedVar pos fieldType $ FieldVar annVar sym)
                                        (lookup sym fields)
                 _ -> Left $ WrongType (recordWithField sym) varType pos

      SubscriptVar v exp -> do
               annExp@(Exp ((expPos, expType), _)) <- annotateExp exp env
               annVar@(Var ((_, varType), _)) <- annotateVar v env
               require (== IntType) expType $ WrongType IntType expType expPos
               case varType of
                 ArrayType elemType _ -> typedVar pos elemType $ SubscriptVar annVar annExp
                 _ -> Left $ WrongType anyArrayType varType pos
