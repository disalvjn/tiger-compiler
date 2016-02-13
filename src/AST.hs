-- Transliteration of what's in the book
module AST where
type Symbol = String

data Var = SimpleVar Symbol
         | FieldVar Var Symbol
         | SubscriptVar Var Exp

data Oper = PlusOp | MinusOp | TimesOp | DivideOp | EqOp
          | NeqOp | LtOp | LeOp | GtOp | GeOp

data Exp = VarExp Var
         | NilExp
         | IntExp Int
         | StringExp String
         | CallExp {callfunc :: Symbol, callargs :: [Exp] }
         | OpExp {opleft :: Exp, opoper :: Oper, opright :: Exp}
         | RecordExp {recfields :: [(Symbol, Exp)], rectyp :: Symbol}
         | SeqExp [Exp]
         | AssignExp {assignvar :: Var, assignexp :: Exp}
         | IfExp {ifpred :: Exp, ifconseq :: Exp, ifalt :: Maybe Exp}
         | WhileExp {whiletest :: Exp, whilebody :: Exp}
         | ForExp {forvar :: Symbol, forlo :: Exp, forhi :: Exp, forbody :: Exp} -- escape?
         | BreakExp
         | LetExp {letdecs :: [Dec], letbody :: Exp}
         | ArrayExp {arraytyp :: Symbol, arraysize :: Exp, arrayinit :: Exp}

data Dec = FunDec [Fundec]
         | VarDec {varname :: Symbol, -- escape?
                   vartyp :: Maybe Symbol, varinit :: Exp}
         | TypeDec [(Symbol, Ty)] -- typename and type

data Ty = NameTy Symbol
        | RecordTy [Field]
        | ArrayTy Symbol

data Field = Field {fieldname :: Symbol, fieldtyp :: Symbol}

data Fundec = Fundec {funname :: Symbol, funparams :: [Field],
                      funresult :: Maybe Symbol, funbody :: Exp}