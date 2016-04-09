module AST.Core(VarF(..), ExpF(..), DecF(..), FundecF(..), TyF(..), Oper(..), Field(..),
                Exp(..), Var(..), Dec(..), Fundec(..), Ty(..),
                PosExp, PosVar, PosTy, PosDec, PosFundec) where
import FrontEnd.Lex(Pos)
import Symbol(Symbol)

{-- The AST is similar to what Appel uses in the book, but I've chosen a more flexible
representation as described here  http://lambda-the-ultimate.org/node/4170#comment-63836.

    Essentially, I parameterize-out the recursion from the AST types. So, if a Var
can contain other Vars and Exps, I define a non-recursive container VarF parameterized over
types expf and var. (See below)

    Then I tie the knot with types Exp, Var, etc. The end-result is that each AST node is wrapped
in a tuple, and the accompanying annotation can differ between Exps and Vars etc. So after
typechecking, I can define types such that Exps and Vars will have accompanying types, but Tys and Decs won't.

    The ability to annotate nodes with whatever I want means there's no need to litter the tree with
positions. I also don't have a field for escaping variables in the tree; I build a set of escaping variables after uniquely naming them in a translation pass.
--}
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
                                 forBody :: exp}
                       | BreakExp
                       | LetExp {letDecs :: [dec], letBody :: exp}
                       | ArrayExp {arrayTyp :: Symbol, arraySize :: exp,
                                   arrayInit :: exp}
                         deriving (Show, Eq)

data DecF exp fundec ty = FunDec [fundec]
                        | VarDec {varName :: Symbol, varTyp :: Maybe Symbol,
                                  varInit :: exp}
                        | TypeDec [(Symbol, ty)] -- typename and type
                          deriving (Show, Eq)

data TyF = NameTy Symbol
         | RecordTy [Field]
         | ArrayTy Symbol
           deriving (Show, Eq)

data Field = Field {fieldName :: Symbol, fieldTyp :: Symbol}
              deriving (Show, Eq)

data FundecF exp = FundecF {funName :: Symbol, funParams :: [Field],
                            funResult :: Maybe Symbol, funBody :: exp}
                   deriving (Show, Eq)

newtype Exp e v d t f = Exp (e, ExpF (Exp e v d t f) (Var e v d t f) (Dec e v d t f)) deriving (Show, Eq)
newtype Var e v d t f = Var (v, VarF (Exp e v d t f) (Var e v d t f)) deriving (Show, Eq)
newtype Dec e v d t f = Dec (d, DecF (Exp e v d t f) (Fundec e v d t f) (Ty t)) deriving (Show, Eq)
newtype Ty t = Ty (t, TyF) deriving (Show, Eq)
newtype Fundec e v d t f = Fundec (f, FundecF (Exp e v d t f)) deriving (Show, Eq)

type PosExp = Exp Pos Pos Pos Pos Pos
type PosVar = Var Pos Pos Pos Pos Pos
type PosDec = Dec Pos Pos Pos Pos Pos
type PosTy = Ty Pos
type PosFundec = Fundec Pos Pos Pos Pos Pos
