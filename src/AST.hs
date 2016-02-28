-- Transliteration of what's in the book
-- http://lambda-the-ultimate.org/node/4170#comment-63836
module AST where
import Lex(Pos)
import Symbol(Symbol)

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

type PosExp = Exp Pos Pos Pos Pos Pos
type PosVar = Var Pos Pos Pos Pos Pos
type PosDec = Dec Pos Pos Pos Pos Pos
type PosTy = Ty Pos
type PosFundec = Fundec Pos Pos Pos Pos Pos
