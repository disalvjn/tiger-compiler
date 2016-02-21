-- Transliteration of what's in the book
-- http://lambda-the-ultimate.org/node/4170#comment-63836
module AST where
import Lex(Pos)

type Symbol = String

data VarF exp var = SimpleVar Symbol
                  | FieldVar var Symbol
                  | SubscriptVar var exp
                    deriving (Show, Eq)

data Oper = PlusOp  | MinusOp  | TimesOp  | DivideOp | EqOp
          | NeqOp | LtOp | LeOp | GtOp | GeOp | AndOp | OrOp
          deriving (Show, Eq)

data ExpF var dec exp  = VarExp var
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

data TyF field = NameTy Symbol
               | RecordTy [field]
               | ArrayTy Symbol
                 deriving (Show, Eq)

data Field = Field {fieldName :: Symbol, fieldTyp :: Symbol} --escape
              deriving (Show, Eq)

data FundecF field exp = FundecF {funName :: Symbol, funParams :: [field],
                                  funResult :: Maybe Symbol, funBody :: exp}
                         deriving (Show, Eq)

newtype Exp e v d t f = Exp (e, ExpF (Var e v d t f) (Dec e v d t f) (Exp e v d t f)) deriving (Show, Eq)
newtype Var e v d t f = Var (v, VarF (Exp e v d t f) (Var e v d t f)) deriving (Show, Eq)
newtype Dec e v d t f = Dec (d, DecF (Exp e v d t f) (Fundec e v d t f) (Ty t)) deriving (Show, Eq)
newtype Ty t = Ty (t, TyF Field) deriving (Show, Eq)
newtype Fundec e v d t f = Fundec (f, FundecF Field (Exp e v d t f)) deriving (Show, Eq)

type PosAST = Exp Pos Pos Pos Pos Pos

{--
updateExpsInVars :: (Exp1 var t -> Exp1 var exp) -> Var1 t var -> Var1 exp var
updateExpsInVars f v =
    case v of
      Var1 (x, SimpleVar s p) -> Var1 (x, SimpleVar s p)
      Var1 (x, FieldVar v s p) -> Var1 (x, FieldVar (updateExpsInVars f v) s p)
      Var1 (x, SubscriptVar v e p) -> Var1 (x, SubscriptVar (updateExpsInVars f v) (f e) p)

--mapExp :: (Exp1 a b -> Exp1 a b) -> (Exp1 a b) -> (Exp1 a b)
mapExp f exp =
    case exp of
      Exp1 (x, VarExp v) -> Exp1 (x, VarExp $ updateExpsInVars f v)
      Exp1 (x, IntExp i) -> Exp1 (x, IntExp i)
      Exp1 (x, StringExp s p) -> Exp1 (x, StringExp s p)
      Exp1 (x, CallExp fn args p) -> Exp1 (x, CallExp fn (map f args) p)

incInts exp =
    case exp of
      Exp1 (x, IntExp i) -> Exp1 (x, IntExp $ i+1)
      e -> mapExp incInts e


instance Functor ((,,) a b) where
    fmap f (x,y,z) = (x,y,f z)

instance Functor (VarF exp) where
    fmap f var =
        case var of
          SimpleVar s p      -> SimpleVar s p
          FieldVar v s p     -> FieldVar (f v) s p
          SubscriptVar v e p -> SubscriptVar (f v) e p

instance Functor (ExpF var dec) where
    fmap f exp =
        case exp of
          VarExp v                 -> VarExp v
          IntExp i                 -> IntExp i
          StringExp s p            -> StringExp s p
          CallExp fn args p        -> CallExp fn (map f args) p
          OpExp left op right p    -> OpExp (f left) op (f right) p
          RecordExp rfields rtyp p -> RecordExp (map (fmap f) rfields) rtyp p
          SeqExp exps p            -> SeqExp (map f exps) p
          AssignExp var exp p      -> AssignExp var (f exp) p
          IfExp pred conseq alt p  -> IfExp (f pred) (f conseq) (fmap f alt) p
          WhileExp test body p     -> WhileExp (f test) (f body) p
          ForExp var lo hi body p  -> ForExp var (f lo) (f hi) (f body) p
          BreakExp p               -> BreakExp p
          LetExp decs body p       -> LetExp decs (f body) p
          ArrayExp typ size init p -> ArrayExp typ (f size) (f init) p

--}
