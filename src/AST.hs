-- Transliteration of what's in the book
-- http://lambda-the-ultimate.org/node/4170#comment-63836
module AST(VarF(..), ExpF(..), DecF(..), FundecF(..), TyF(..), Oper(..), Field(..),
           Exp(..), Var(..), Dec(..), Fundec(..), Ty(..),
           PosExp, PosVar, PosTy, PosDec, PosFundec,
           mapExp, mapMExp) where
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
                                 forBody :: exp} -- escape?
                       | BreakExp
                       | LetExp {letDecs :: [dec], letBody :: exp}
                       | ArrayExp {arrayTyp :: Symbol, arraySize :: exp,
                                   arrayInit :: exp}
                         deriving (Show, Eq)

data DecF exp fundec ty = FunDec [fundec]
                        | VarDec {varName :: Symbol, -- escape?
                                  varTyp :: Maybe Symbol, varInit :: exp}
                        | TypeDec [(Symbol, ty)] -- typename and type
                          deriving (Show, Eq)

data TyF = NameTy Symbol
         | RecordTy [Field]
         | ArrayTy Symbol
           deriving (Show, Eq)

data Field = Field {fieldName :: Symbol, fieldTyp :: Symbol} --escape
              deriving (Show, Eq)

data FundecF exp = FundecF {funName :: Symbol, funParams :: [Field],
                            funResult :: Maybe Symbol, funBody :: exp}
                   deriving (Show, Eq)

newtype Exp e v d t f = Exp (e, ExpF (Exp e v d t f) (Var e v d t f) (Dec e v d t f)) deriving (Show, Eq)
newtype Var e v d t f = Var (v, VarF (Exp e v d t f) (Var e v d t f)) deriving (Show, Eq)
newtype Dec e v d t f = Dec (d, DecF (Exp e v d t f) (Fundec e v d t f) (Ty t)) deriving (Show, Eq)
newtype Ty t = Ty (t, TyF) deriving (Show, Eq)
newtype Fundec e v d t f = Fundec (f, FundecF (Exp e v d t f)) deriving (Show, Eq)

mapExpF :: (exp -> exp') -> (var -> var') -> (dec -> dec') -> ExpF exp var dec -> ExpF exp' var' dec'
mapExpF expf varf decf exp =
  case exp of
    VarExp var             -> VarExp $ varf var
    NilExp                 -> NilExp
    IntExp i               -> IntExp i
    StringExp s            -> StringExp s
    CallExp f args         -> CallExp f $ map expf args
    OpExp left op right    -> OpExp (expf left) op (expf right)
    RecordExp fields typ   -> RecordExp (map (fmap expf) fields) typ
    SeqExp exps            -> SeqExp $ map expf exps
    AssignExp lval rval    -> AssignExp (varf lval) (expf rval)
    IfExp pred conseq alt  -> IfExp (expf pred) (expf conseq) (fmap expf alt)
    WhileExp test body     -> WhileExp (expf test) (expf body)
    ForExp i lo hi body    -> ForExp i (expf lo) (expf hi) (expf body)
    BreakExp               -> BreakExp
    LetExp decs body       -> LetExp (map decf decs) (expf body)
    ArrayExp typ size init -> ArrayExp typ (expf size) (expf init)

mapVarF :: (exp -> exp') -> (var -> var') -> VarF exp var -> VarF exp' var'
mapVarF expf varf var =
    case var of
      SimpleVar s      -> SimpleVar s
      FieldVar v s     -> FieldVar (varf v) s
      SubscriptVar v e -> SubscriptVar (varf v) (expf e)

mapDecF :: (exp -> exp') -> (fundec -> fundec') -> (ty -> ty') -> DecF exp fundec ty -> DecF exp' fundec' ty'
mapDecF expf fundecf tyf dec =
    case dec of
      FunDec decs          -> FunDec $ map fundecf decs
      VarDec name typ init -> VarDec name typ (expf init)
      TypeDec decs         -> TypeDec $ map (fmap tyf) decs

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
      ForExp i lo hi body -> do
               lo' <- expf lo
               hi' <- expf hi
               body' <- expf body
               return $ ForExp i lo' hi' body'
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
      VarDec name typ init -> expf init >>= return . (VarDec name typ)
      TypeDec decs -> do
               let (names, types) = unzip decs
               types' <- mapM tyf types
               return $ TypeDec (zip names types')

mapMFundecF :: (Monad m) => (exp -> m exp') -> FundecF exp -> m (FundecF exp')
mapMFundecF expf (FundecF name params result body) =
    expf body >>= return . (FundecF name params result)


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
