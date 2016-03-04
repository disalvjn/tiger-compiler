-- Transliteration of what's in the book
-- http://lambda-the-ultimate.org/node/4170#comment-63836
module AST(VarF(..), ExpF(..), DecF(..), FundecF(..), TyF(..), Oper(..), Field(..),
           Exp(..), Var(..), Dec(..), Fundec(..), Ty(..),
           PosExp, PosVar, PosTy, PosDec, PosFundec,
           mapExp, mapMExp, transformExp) where
import Lex(Pos)
import Symbol(Symbol)
import qualified Data.Traversable as T

data VarF exp var = SimpleVar Symbol
                  | FieldVar var Symbol
                  | SubscriptVar var exp
                    deriving (Show, Eq)

data Oper = PlusOp  | MinusOp  | TimesOp  | DivideOp | EqOp
          | NeqOp | LtOp | LeOp | GtOp | GeOp | AndOp | OrOp
          deriving (Show, Eq)

data ExpF exp var dec = VarExp var
                       | NilExp
                       | IntExp Int
                       | StringExp String
                       | CallExp {callFunc :: Symbol, callArgs :: [exp]}
                       | OpExp {opLeft :: exp, opOper :: Oper, opRight :: exp}
                       | RecordExp {recFields :: [(Symbol, exp)], recTyp :: Symbol}
                       | SeqExp [exp]
                       | AssignExp {assignVar :: var, assignExp :: exp}
                       | IfExp {ifPred :: exp, ifConseq :: exp, ifAlt :: Maybe exp}
                       | WhileExp {whileTest :: exp, whileBody :: exp}
                       | ForExp {forVar :: Symbol, forLo :: exp, forHi :: exp,
                                 forBody :: exp, forVarEscape :: Bool}
                       | BreakExp
                       | LetExp {letDecs :: [dec], letBody :: exp}
                       | ArrayExp {arrayTyp :: Symbol, arraySize :: exp,
                                   arrayInit :: exp}
                         deriving (Show, Eq)

data DecF exp fundec ty = FunDec [fundec]
                        | VarDec {varName :: Symbol, varTyp :: Maybe Symbol,
                                  varInit :: exp, varEscape :: Bool}
                        | TypeDec [(Symbol, ty)] -- typename and type
                          deriving (Show, Eq)

data TyF = NameTy Symbol
         | RecordTy [Field]
         | ArrayTy Symbol
           deriving (Show, Eq)

data Field = Field {fieldName :: Symbol, fieldTyp :: Symbol, fieldEscape :: Bool}
              deriving (Show, Eq)

data FundecF exp = FundecF {funName :: Symbol, funParams :: [Field],
                            funResult :: Maybe Symbol, funBody :: exp}
                   deriving (Show, Eq)

newtype Exp e v d t f = Exp (e, ExpF (Exp e v d t f) (Var e v d t f) (Dec e v d t f)) deriving (Show, Eq)
newtype Var e v d t f = Var (v, VarF (Exp e v d t f) (Var e v d t f)) deriving (Show, Eq)
newtype Dec e v d t f = Dec (d, DecF (Exp e v d t f) (Fundec e v d t f) (Ty t)) deriving (Show, Eq)
newtype Ty t = Ty (t, TyF) deriving (Show, Eq)
newtype Fundec e v d t f = Fundec (f, FundecF (Exp e v d t f)) deriving (Show, Eq)

transformExpF :: (exp -> (exp', a)) -> (var -> (var', a)) -> (dec -> (dec', a))
                 -> (a -> b -> b) -> b -> ExpF exp var dec -> (ExpF exp' var' dec', b)
transformExpF expf varf decf combine nil exp =
  case exp of
    VarExp var             -> let (var', x) = varf var
                              in (VarExp var', x `combine` nil)
    NilExp                 -> (NilExp, nil)
    IntExp i               -> (IntExp i, nil)
    StringExp s            -> (StringExp s, nil)
    CallExp f args         -> let (args', x) = unpackage $ map expf args
                              in (CallExp f args', x)
    OpExp left op right    -> let (left', l) = expf left
                                  (right', r) = expf right
                              in (OpExp left' op right', l `combine` (r `combine` nil))
    RecordExp fields typ   -> let (fieldNames, fieldExps) = unzip fields
                                  (newExps, x) = unpackage $ map expf fieldExps
                              in (RecordExp (zip fieldNames newExps) typ, x)
    SeqExp exps            -> let (exps', x) = unpackage $ map expf exps
                              in (SeqExp exps', x)
    AssignExp lval rval    -> let (lval', x) = varf lval
                                  (rval', y) = expf rval
                              in (AssignExp lval' rval', x `combine` (y `combine` nil))
    IfExp pred conseq alt  -> let (pred', x) = expf pred
                                  (conseq', y) = expf conseq
                                  (alt', z) = maybe
                                              (Nothing, nil)
                                              (\(a, b) -> (Just a, b `combine` nil))
                                              (fmap expf alt)
                              in (IfExp pred' conseq' alt',
                                  x `combine` (y `combine` z))
    WhileExp test body     -> let (test', x) = expf test
                                  (body', y) = expf body
                              in (WhileExp test' body', x `combine` (y `combine` nil))
    ForExp i lo hi body e  -> let (lo', x) = expf lo
                                  (hi', y) = expf hi
                                  (body', z) = expf body
                              in (ForExp i lo' hi' body' e,
                                  x `combine` (y `combine` (z `combine` nil)))
    BreakExp               -> (BreakExp, nil)
    LetExp decs body       -> let (decs', xs) = unzip $ map decf decs
                                  (body', y) = expf body
                              in (LetExp decs' body', foldr combine (y `combine` nil) xs )
    ArrayExp typ size init -> let (size', x) = expf size
                                  (init', y) = expf init
                              in (ArrayExp typ size' init', x `combine` (y `combine` nil))
    where unpackage expsAndVals =
              let (exps, vals) = unzip expsAndVals
                  joinedVals = foldr combine nil vals
              in (exps, joinedVals)

package f x = (f x, ())
discard _ _ = ()

mapExpF expf varf decf exp =
    fst $ transformExpF (package expf) (package varf) (package decf) discard () exp

transformVarF :: (exp -> (exp', a)) -> (var -> (var', a)) ->
                 (a -> b -> b) -> b -> VarF exp var -> (VarF exp' var', b)
transformVarF expf varf combine nil var =
    case var of
      SimpleVar s      -> (SimpleVar s, nil)
      FieldVar v s     -> let (v', x) = varf v
                          in (FieldVar v' s, x `combine` nil)
      SubscriptVar v e -> let (v', x) = varf v
                              (e', y) = expf e
                          in (SubscriptVar v' e', x `combine` (y `combine` nil))

mapVarF expf varf exp =
    fst $ transformVarF (package expf) (package varf) discard () exp

transformDecF :: (exp -> (exp', a)) -> (fundec -> (fundec', a)) -> (ty -> (ty', a))
              -> (a -> b -> b) -> b -> DecF exp fundec ty -> (DecF exp' fundec' ty', b)
transformDecF expf fundecf tyf combine nil dec =
    case dec of
      FunDec decs            -> let (decs', xs) = unzip $ map fundecf decs
                                in (FunDec decs', foldr combine nil xs)
      VarDec name typ init e -> let (init', x) = expf init
                                in (VarDec name typ init' e, x `combine` nil)
      TypeDec decs           -> let (tyNames, tyDecs) = unzip decs
                                    (tyDecs', xs) = unzip $ map tyf tyDecs
                                in (TypeDec (zip tyNames tyDecs'), foldr combine nil xs)

mapDecF expf fundecf tyf dec =
    fst $ transformDecF (package expf) (package fundecf) (package tyf) discard () dec

instance Functor FundecF where
    fmap expf (FundecF name params result body) = FundecF name params result (expf body)

mapMExpF :: Monad m => (exp -> m exp') -> (var -> m var') -> (dec -> m dec')
         -> ExpF exp var dec -> m (ExpF exp' var' dec')
mapMExpF expf varf decf exp =
    case exp of
      VarExp v -> (varf v) >>= return . VarExp
      NilExp -> return NilExp
      IntExp i -> return $ IntExp i
      StringExp s -> return $ StringExp s
      CallExp f args -> mapM expf args >>= return . (CallExp f)
      OpExp left op right -> do
               left' <- expf left
               right' <- expf right
               return $ OpExp left' op right'
      RecordExp fields typ -> do
               let (names, inits) = unzip fields
               inits' <- mapM expf inits
               return $ RecordExp (zip names inits') typ
      SeqExp exps -> mapM expf exps >>= return . SeqExp
      AssignExp lval rval -> do
               lval' <- varf lval
               rval' <- expf rval
               return $ AssignExp lval' rval'
      IfExp pred conseq alt -> do
               pred' <- expf pred
               conseq' <- expf conseq
               alt' <- T.mapM expf alt
               return $ IfExp pred' conseq' alt'
      WhileExp test body -> do
               test' <- expf test
               body' <- expf body
               return $ WhileExp test' body'
      ForExp i lo hi body esc -> do
               lo' <- expf lo
               hi' <- expf hi
               body' <- expf body
               return $ ForExp i lo' hi' body' esc
      BreakExp -> return BreakExp
      LetExp decs body -> do
               decs' <- mapM decf decs
               body' <- expf body
               return $ LetExp decs' body'
      ArrayExp typ size init -> do
               size' <- expf size
               init' <- expf init
               return $ ArrayExp typ size' init'

mapMVarF :: (Monad m) => (exp -> m exp') -> (var -> m var') -> VarF exp var -> m (VarF exp' var')
mapMVarF expf varf var =
    case var of
      SimpleVar s -> return $ SimpleVar s
      FieldVar v s -> do
               v' <- varf v
               return $ FieldVar v' s
      SubscriptVar v e -> do
               v' <- varf v
               e' <- expf e
               return $ SubscriptVar v' e'

mapMDecF :: (Monad m) => (exp -> m exp') -> (fundec -> m fundec') -> (ty -> m ty')
         -> DecF exp fundec ty -> m (DecF exp' fundec' ty')
mapMDecF expf fundecf tyf dec =
    case dec of
      FunDec decs -> mapM fundecf decs >>= return . FunDec
      VarDec name typ init esc -> expf init >>= return . (\init -> VarDec name typ init esc)
      TypeDec decs -> do
               let (names, types) = unzip decs
               types' <- mapM tyf types
               return $ TypeDec (zip names types')

mapMFundecF :: (Monad m) => (exp -> m exp') -> FundecF exp -> m (FundecF exp')
mapMFundecF expf (FundecF name params result body) =
    expf body >>= return . (FundecF name params result)


transformExp f combine nil (Exp (x, e)) =
    let (e', res) = transformExpF f (transformExpsInVar f combine nil)
                    (transformExpsInDec f combine nil)
                    combine
                    nil
                    e
    in (Exp (x, e'), res)

transformExpsInVar f combine nil (Var (x, v)) =
    let (v', res) = transformVarF f (transformExpsInVar f combine nil) combine nil v
    in (Var (x, v'), res)

transformExpsInDec f combine nil (Dec (x, dec)) =
    let (dec', res) = transformDecF f (transformExpsInFundec f combine nil)
                      (\x -> (x, nil)) combine nil dec
    in (Dec (x, dec'), res)

transformExpsInFundec f combine nil (Fundec (x, FundecF name params result body)) =
    let (body', res) = f body
    in (Fundec (x, FundecF name params result body'), res `combine` nil)



mapExp f (Exp (x, e)) = Exp (x, mapExpF f (mapToExpsInVar f) (mapToExpsInDec f) e)
mapDec f (Exp (x, e)) = Exp (x, mapExpF (mapToDecsInExp f) (mapToExpsInVar $ mapDec f) f e)
mapVar f (Exp (x, e)) = Exp (x, mapExpF (mapToVarsInExp f) f (mapToExpsInDec $ mapVar f) e)

mapToExpsInVar f (Var (x, v)) = Var (x, mapVarF f (mapToExpsInVar f) v)

mapToExpsInDec f (Dec (x, dec)) = Dec (x, mapDecF f (mapToExpsInFundec f) id dec)

mapToExpsInFundec f (Fundec (x, dec)) = Fundec (x, fmap f dec)

mapToDecsInExp f (Exp (x, e)) = Exp (x, mapExpF (mapToDecsInExp f)
                                          (mapToExpsInVar $ mapToDecsInExp f) f e)

mapToVarsInExp f (Exp (x, e)) = Exp (x, mapExpF (mapToVarsInExp f) f
                                          (mapToExpsInDec $ mapToVarsInExp f) e)

mapMExp f (Exp (x, e)) = mapMExpF f (mapMToExpsInVar f) (mapMToExpsInDec f) e
                         >>= return . Exp . (,) x

mapMToExpsInVar f (Var (x, v)) = mapMVarF f (mapMToExpsInVar f) v >>= return . Var . (,) x
mapMToExpsInDec f (Dec (x, d)) = mapMDecF f (mapMToExpsInFundec f) return d
                                 >>= return . Dec . (,) x
mapMToExpsInFundec f (Fundec (x, d)) = mapMFundecF f d >>= return . Fundec . (,) x


type PosExp = Exp Pos Pos Pos Pos Pos
type PosVar = Var Pos Pos Pos Pos Pos
type PosDec = Dec Pos Pos Pos Pos Pos
type PosTy = Ty Pos
type PosFundec = Fundec Pos Pos Pos Pos Pos
