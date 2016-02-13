{
module Parse (parse) where
import Lex(Token(..), TokenType(..))
import Data.Maybe
import AST
}

%name parse
%tokentype {Lex.Token}

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
   eof      {Token Eof _}

%%

Exp : VarExp {$1}
| nil {NilExp}
| int {IntExp $1}
| string {StringExp $1}
| id '(' FunArgs ')' {CallExp $1 $3}
| id '{' RecordFields '}' {RecordExp $3 $1}
| '(' SeqExp ')' {SeqExp $2}
| VarExp assign Exp {AssignExp $1 $3}
| IfExp {$1}
| while Exp do Exp {WhileExp $2 $4}
| for VarExp assign Exp to Exp do Exp {ForExp $2 $4 $6 $8}
| break {BreakExp}
| let DecList in Exp end {LetExp $2 $4}
| id '[' Exp ']' of Exp {ArrayExp $1 $3 $6}

IfExp : if Exp then Exp AltExp {IfExp $2 $4 $5}

AltExp : {Nothing}
| else Exp {Just $2}

VarExp : id {SimpleVar $1}
| VarExp id {FieldVar $1 $2}
| VarExp '[' Exp ']' {SubscriptVar $1 $3}

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

Dec : FunDecList {$1}
| VarDec {$1}
| TypeDecList {$1}

FunDec : function id '(' TyFields ')' '=' Exp {Fundec $2 $4 Nothing $6}
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
| '{' TyFields '}' {reverse $2}
| array of id {ArrayTy $3}

TyFields : {[]}
| TyFields ',' id ':' id { (Field $3 $5) : $1}


Operator : '+' {PlusOp}
| '-' {MinusOp}
| '*' {TimesOp}
| '/' {DivideOp}
| '<' {LtOp}
| '>' {GtOp}
| '>=' {GeOp}
| '<=' {LeOp}
| '=' {EqOp}

OpExp : Exp Operator Exp {OpExp $1 $2 $3}
