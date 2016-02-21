{
module Parse (parse) where
import qualified Lex as L
import Data.Maybe
import AST
}

%name parse
%tokentype {L.Token}
%error {parseError}

{-- %expect 22 --}

%token
   type     {L.Token L.Type _}
   var      {L.Token L.Var _}
   function {L.Token L.Function _}
   break    {L.Token L.Break _}
   of       {L.Token L.Of _}
   end      {L.Token L.End _}
   in       {L.Token L.In _}
   nil      {L.Token L.Nil _}
   let      {L.Token L.Let _}
   do       {L.Token L.Do _}
   to       {L.Token L.To _}
   for      {L.Token L.For _}
   while    {L.Token L.While _}
   else     {L.Token L.Else _}
   then     {L.Token L.Then _}
   if       {L.Token L.If _}
   array    {L.Token L.Array _}
   assign   {L.Token L.Assign _}
   or       {L.Token L.Or _}
   and      {L.Token L.And _}
   '>='     {L.Token L.Ge _}
   '>'      {L.Token L.Gt _}
   '<='     {L.Token L.Le _}
   '<'      {L.Token L.Lt _}
   '!='     {L.Token L.Neq _}
   '='      {L.Token L.Eq _}
   '/'      {L.Token L.Divide _}
   '*'      {L.Token L.Times _}
   '-'      {L.Token L.Minus _}
   '+'      {L.Token L.Plus _}
   '.'      {L.Token L.Dot _}
   '}'      {L.Token L.Rbrace _}
   '{'      {L.Token L.Lbrace _}
   ']'      {L.Token L.Rbrack _}
   '['      {L.Token L.Lbrack _}
   ')'      {L.Token L.Rparen _}
   '('      {L.Token L.Lparen _}
   ';'      {L.Token L.Semicolon _}
   ':'      {L.Token L.Colon _}
   ','      {L.Token L.Comma _}
   string   {L.Token (L.String _) _ }
   int      {L.Token (L.Int _) _}
   id       {L.Token (L.Id _) _}


%%

Exp : OpExp {$1}

ExpNoBinop : Var {let Var v = $1 in Exp (fst v, VarExp $ Var v)}
| nil {Exp (pos $1, NilExp)}
| int {Exp (pos $1, IntExp $ getInt $1)}
| string {Exp (pos $1, StringExp (getStr $1))}
| id '(' FunArgs ')' {Exp (pos $1, CallExp (getId $1) $3) }
| id '{' RecordFields '}' {Exp (pos $1, RecordExp $3 (getId $1))}
| '(' SeqExp ')' {Exp (pos $1, SeqExp $2)}
| Var assign Exp {Exp (pos $2, AssignExp $1 $3)}
| IfExp {$1}
| while Exp do Exp {Exp (pos $1, WhileExp $2 $4)}
| for id assign Exp to Exp do Exp {Exp (pos $1, ForExp (getId $2) $4 $6 $8)}
| break {Exp (pos $1, BreakExp)}
| let DecList in Exp end {Exp (pos $1, LetExp $2 $4)}
| id '[' Exp ']' of Exp {Exp (pos $1, ArrayExp (getId $1) $3 $6)}

IfExp : if Exp then Exp AltExp {Exp (pos $1, IfExp $2 $4 $5)}

AltExp : {Nothing}
| else Exp {Just $2}

Var : id {Var (pos $1, SimpleVar (getId $1))}
| Var '.' id {Var (pos $2, FieldVar $1 (getId $3))}
| Var '[' Exp ']' {Var (pos $2, SubscriptVar $1 $3)}
| id '[' Exp ']' {Var (pos $2, SubscriptVar (Var (pos $1, SimpleVar (getId $1))) $3)}

FunArgs : FunArgsx {reverse $1}
FunArgsx : {- empty -} {[]}
| Exp {[$1]}
| FunArgsx ',' Exp {$3 : $1}

RecordFields : RecordFieldsx {reverse $1}
RecordFieldsx : {- empty -} {[]}
| id '=' Exp {[((getId $1), $3)]}
| RecordFieldsx ',' id '=' Exp { ((getId $3), $5) : $1 }

SeqExp : SeqExpx {reverse $1}
SeqExpx : Exp { [$1] }
| SeqExpx ';' Exp { $3 : $1 }

DecList : DecListr {reverse $1}
DecListr : Dec {[$1]}
| DecListr Dec {$2 : $1}

Dec : FunDecList {Dec ((-1,-1), FunDec $1)}
| VarDec {$1}
| TypeDecList {Dec ((-1, -1), TypeDec $1)}

FunDec : function id '(' TyFields ')' '=' Exp {Fundec (pos $1, FundecF (getId $2) $4 Nothing $7)}
| function id '(' TyFields ')' ':' id '=' Exp {Fundec (pos $1, FundecF (getId $2) $4 (Just $ getId $7) $9)}

FunDecList : FunDecListr {reverse $1}
FunDecListr : FunDec {[$1]}
| FunDecListr FunDec {$2 : $1}

VarDec : var id assign Exp {Dec (pos $1, VarDec (getId $2) Nothing $4)}
| var id ':' id assign Exp {Dec (pos $1, VarDec (getId $2) (Just (getId $4)) $6)}

TypeDecList : TypeDecListr {reverse $1}

TypeDecListr : TypeDec {[$1]}
| TypeDecListr TypeDec {$2 : $1}

TypeDec : type id '=' Ty {((getId $2), $4)}

Ty : id {Ty (pos $1, NameTy (getId $1))}
| '{' TyFields '}' {Ty (pos $1, RecordTy (reverse $2))}
| array of id {Ty (pos $1, ArrayTy (getId $3))}

TyFields : {[]}
| SomeTyFields {reverse $1}

SomeTyFields : SomeTyFields ',' TyField {$3 : $1}
| TyField {[$1]}
TyField : id ':' id {Field (getId $1) (getId $3) }

OpExp : Equality {$1}
| OpExp and Equality {Exp (pos $2, OpExp $1 AndOp $3)}
| OpExp or Equality {Exp (pos $2, OpExp $1 OrOp $3)}

Equality : Comparison {$1}
| Equality '=' Comparison {Exp (pos $2, OpExp $1 EqOp $3)}
| Equality '!=' Comparison {Exp (pos $2, OpExp $1 NeqOp $3)}


Comparison : Sum {$1}
| Comparison '<' Sum {Exp (pos $2, OpExp $1 LtOp $3)}
| Comparison '>' Sum {Exp (pos $2, OpExp $1 GtOp $3)}
| Comparison '<=' Sum {Exp (pos $2, OpExp $1 LeOp $3)}
| Comparison '>=' Sum {Exp (pos $2, OpExp $1 GeOp $3)}


Sum: Product {$1}
| Sum '+' Product {Exp (pos $2, OpExp $1 PlusOp $3)}
| Sum '-' Product {Exp (pos $2, OpExp $1 MinusOp $3)}

Product : ExpNoBinop {$1}
| Product '*' ExpNoBinop {Exp (pos $2, OpExp $1 TimesOp $3)}
| Product '/' ExpNoBinop {Exp (pos $2, OpExp $1 DivideOp $3)}


{

pos (L.Token _ p) = p
getId (L.Token (L.Id i) _) = i
getInt (L.Token (L.Int i) _) = i
getStr (L.Token (L.String s) _) = s

parseError :: [L.Token] -> a
parseError ((L.Token _ (l,c)) : _) =
  error $ "Parse error at line " ++ (show l) ++ " col " ++  (show c)

parseError [] = error "Error (and no tokens remain)"
}
