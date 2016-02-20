{
module Lex (Token(..), TokenType(..), AlexPosn(..), alexScanTokens) where
}

%wrapper "posn"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				;
  "/*".*"*/"				;
  type     {\p s -> Token Type p }
  var      {\p s -> Token Var p}
  function {\p s -> Token Function p}
  break    {\p s -> Token Break p}
  of       {\p s -> Token Of p}
  end      {\p s -> Token End p}
  in       {\p s -> Token In p}
  nil      {\p s -> Token Nil p}
  let      {\p s -> Token Let p}
  do       {\p s -> Token Do p}
  to       {\p s -> Token To p}
  for      {\p s -> Token For p}
  while    {\p s -> Token While p}
  else     {\p s -> Token Else p}
  then     {\p s -> Token Then p}
  if       {\p s -> Token If p}
  array    {\p s -> Token Array p}
  ":="     {\p s -> Token Assign p}
  "&"      {\p s -> Token And p}
  "|"      {\p s -> Token Or p}
  ">="     {\p s -> Token Ge p}
  ">"      {\p s -> Token Gt p}
  "<="     {\p s -> Token Le p}
  "<"      {\p s -> Token Lt p}
  "!="     {\p s -> Token Neq p}
  "="      {\p s -> Token Eq p}
  "/"      {\p s -> Token Divide p }
  "*"      {\p s -> Token Times p}
  "-"      {\p s -> Token Minus p}
  "+"      {\p s -> Token Plus p}
  "."      {\p s -> Token Dot p}
  "{"      {\p s -> Token Lbrace p}
  "}"      {\p s -> Token Rbrace p}
  "["      {\p s -> Token Lbrack p}
  "]"      {\p s -> Token Rbrack p}
  "("      {\p s -> Token Lparen p}
  ")"      {\p s -> Token Rparen p}
  ";"      {\p s -> Token Semicolon p}
  ":"      {\p s -> Token Colon p}
  ","      {\p s -> Token Comma p}
  \" [^\"]* \" {\p s -> Token (String s) p}

  $digit+				{ \p s -> Token (Int (read s)) p }
  $alpha [$alpha $digit \_ \']*		{ \p s -> Token (Id s) p }



{
-- Each action has type :: String -> Token

-- The token type:
data TokenType =
   Type          |
   Var           |
   Function      |
   Break         |
   Of            |
   End           |
   In            |
   Nil           |
   Let           |
   Do            |
   To            |
   For           |
   While         |
   Else          |
   Then          |
   If            |
   Array         |
   Assign        |
   Or            |
   And           |
   Ge            |
   Gt            |
   Le            |
   Lt            |
   Neq           |
   Eq            |
   Divide        |
   Times         |
   Minus         |
   Plus          |
   Dot           |
   Rbrace        |
   Lbrace        |
   Rbrack        |
   Lbrack        |
   Rparen        |
   Lparen        |
   Semicolon     |
   Colon         |
   Comma         |
   String String |
   Int Int       |
   Id String     |
   Eof
   deriving (Eq,Show)

data Token = Token TokenType AlexPosn deriving (Eq, Show)

}
