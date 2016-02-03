{
  import Lex(Token)
  module Parse (parse) where
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
