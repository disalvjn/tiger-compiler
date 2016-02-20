{
module Parse (parse) where
import Lex(Token(..), TokenType(..), AlexPosn(..))
import Data.Maybe
import AST
}

%name parse
%tokentype {Lex.Token}
%error {parseError}

{-- %expect 22 --}

%token
   type     {Token Type _}
   var      {Token Var _}
   function {Token Function _}
   break    {Token Break _}
   of       {Token Of _}
   end      {Token End _}
   in       {Token In _}
   nil      {Token Nil _}
   let      {Token Let _}
   do       {Token Do _}
   to       {Token To _}
   for      {Token For _}
   while    {Token While _}
   else     {Token Else _}
   then     {Token Then _}
   if       {Token If _}
   array    {Token Array _}
   assign   {Token Assign _}
   or       {Token Or _}
   and      {Token And _}
   '>='     {Token Ge _}
   '>'      {Token Gt _}
   '<='     {Token Le _}
   '<'      {Token Lt _}
   '!='     {Token Neq _}
   '='      {Token Eq _}
   '/'      {Token Divide _}
   '*'      {Token Times _}
   '-'      {Token Minus _}
   '+'      {Token Plus _}
   '.'      {Token Dot _}
   '}'      {Token Rbrace _}
   '{'      {Token Lbrace _}
   ']'      {Token Rbrack _}
   '['      {Token Lbrack _}
   ')'      {Token Rparen _}
   '('      {Token Lparen _}
   ';'      {Token Semicolon _}
   ':'      {Token Colon _}
   ','      {Token Comma _}
   string   {Token (String $$) _ }
   int      {Token (Int $$) _}
   id       {Token (Id $$) _}

{-- %left '+' '-' and or '>' '<' '>=' '<=' '=' '*' '/' --}

%%

Exp : OpExp {$1}

ExpNoBinop : Var {VarExp $1}
| nil {NilExp}
| int {IntExp $1}
| string {StringExp $1}
| id '(' FunArgs ')' {CallExp $1 $3}
| id '{' RecordFields '}' {RecordExp $3 $1}
| '(' SeqExp ')' {SeqExp $2}
| Var assign Exp {AssignExp $1 $3}
| IfExp {$1}
| while Exp do Exp {WhileExp $2 $4}
| for id assign Exp to Exp do Exp {ForExp $2 $4 $6 $8}
| break {BreakExp}
| let DecList in Exp end {LetExp $2 $4}
| id '[' Exp ']' of Exp {ArrayExp $1 $3 $6}

IfExp : if Exp then Exp AltExp {IfExp $2 $4 $5}

AltExp : {Nothing}
| else Exp {Just $2}

Var : id {SimpleVar $1}
| Var '.' id {FieldVar $1 $3}
| Var '[' Exp ']' {SubscriptVar $1 $3}
| id '[' Exp ']' {SubscriptVar (SimpleVar $1) $3}

FunArgs : FunArgsx {reverse $1}
FunArgsx : {- empty -} {[]}
| Exp {[$1]}
| FunArgsx ',' Exp {$3 : $1}

RecordFields : RecordFieldsx {reverse $1}
RecordFieldsx : {- empty -} {[]}
| id '=' Exp {[($1, $3)]}
| RecordFieldsx ',' id '=' Exp { ($3, $5) : $1 }

SeqExp : SeqExpx {reverse $1}
SeqExpx : Exp { [$1] }
| SeqExpx ';' Exp { $3 : $1 }

DecList : DecListr {reverse $1}
DecListr : Dec {[$1]}
| DecListr Dec {$2 : $1}

Dec : FunDecList {FunDec $1}
| VarDec {$1}
| TypeDecList {TypeDec $1}

FunDec : function id '(' TyFields ')' '=' Exp {Fundec $2 $4 Nothing $7}
| function id '(' TyFields ')' ':' id '=' Exp {Fundec $2 $4 (Just $7) $9}

FunDecList : FunDecListr {reverse $1}
FunDecListr : FunDec {[$1]}
| FunDecListr FunDec {$2 : $1}

VarDec : var id assign Exp {VarDec $2 Nothing $4}
| var id ':' id assign Exp {VarDec $2 (Just $4) $6}

TypeDecList : TypeDecListr {reverse $1}

TypeDecListr : TypeDec {[$1]}
| TypeDecListr TypeDec {$2 : $1}

TypeDec : type id '=' Ty {($2, $4)}

Ty : id {NameTy $1}
| '{' TyFields '}' {RecordTy (reverse $2)}
| array of id {ArrayTy $3}

TyFields : {[]}
| SomeTyFields {reverse $1}

SomeTyFields : SomeTyFields ',' TyField {$3 : $1}
| TyField {[$1]}
TyField : id ':' id {Field $1 $3}

OpExp : Equality {$1}
| OpExp and Equality {OpExp $1 AndOp $3}
| OpExp or Equality {OpExp $1 OrOp $3}

Equality : Comparison {$1}
| Equality '=' Comparison {OpExp $1 EqOp $3}
| Equality '!=' Comparison {OpExp $1 NeqOp $3}


Comparison : Sum {$1}
| Comparison '<' Sum {OpExp $1 LtOp $3}
| Comparison '>' Sum {OpExp $1 GtOp $3}
| Comparison '<=' Sum {OpExp $1 LeOp $3}
| Comparison '>=' Sum {OpExp $1 GeOp $3}


Sum: Product {$1}
| Sum '+' Product {OpExp $1 PlusOp $3}
| Sum '-' Product {OpExp $1 MinusOp $3}

Product : ExpNoBinop {$1}
| Product '*' ExpNoBinop {OpExp $1 TimesOp $3}
| Product '/' ExpNoBinop {OpExp $1 DivideOp $3}


{
parseError :: [Token] -> a
parseError ((Token _ (AlexPn _ l c)) : _) =
  error $ "Parse error at line " ++ (show l) ++ " col " ++  (show c)

parseError [] = error "Error (and no tokens remain)"
}
