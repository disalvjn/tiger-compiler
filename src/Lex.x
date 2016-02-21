{
module Lex (Token(..), TokenType(..), Pos, alexScanTokens) where
}

%wrapper "posn"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				;
  "/*".*"*/"				;
  type     {\p s -> Token Type (alexPosnToPos p) }
  var      {\p s -> Token Var (alexPosnToPos p)}
  function {\p s -> Token Function (alexPosnToPos p)}
  break    {\p s -> Token Break (alexPosnToPos p)}
  of       {\p s -> Token Of (alexPosnToPos p)}
  end      {\p s -> Token End (alexPosnToPos p)}
  in       {\p s -> Token In (alexPosnToPos p)}
  nil      {\p s -> Token Nil (alexPosnToPos p)}
  let      {\p s -> Token Let (alexPosnToPos p)}
  do       {\p s -> Token Do (alexPosnToPos p)}
  to       {\p s -> Token To (alexPosnToPos p)}
  for      {\p s -> Token For (alexPosnToPos p)}
  while    {\p s -> Token While (alexPosnToPos p)}
  else     {\p s -> Token Else (alexPosnToPos p)}
  then     {\p s -> Token Then (alexPosnToPos p)}
  if       {\p s -> Token If (alexPosnToPos p)}
  array    {\p s -> Token Array (alexPosnToPos p)}
  ":="     {\p s -> Token Assign (alexPosnToPos p)}
  "&"      {\p s -> Token And (alexPosnToPos p)}
  "|"      {\p s -> Token Or (alexPosnToPos p)}
  ">="     {\p s -> Token Ge (alexPosnToPos p)}
  ">"      {\p s -> Token Gt (alexPosnToPos p)}
  "<="     {\p s -> Token Le (alexPosnToPos p)}
  "<"      {\p s -> Token Lt (alexPosnToPos p)}
  "!="     {\p s -> Token Neq (alexPosnToPos p)}
  "="      {\p s -> Token Eq (alexPosnToPos p)}
  "/"      {\p s -> Token Divide (alexPosnToPos p) }
  "*"      {\p s -> Token Times (alexPosnToPos p)}
  "-"      {\p s -> Token Minus (alexPosnToPos p)}
  "+"      {\p s -> Token Plus (alexPosnToPos p)}
  "."      {\p s -> Token Dot (alexPosnToPos p)}
  "{"      {\p s -> Token Lbrace (alexPosnToPos p)}
  "}"      {\p s -> Token Rbrace (alexPosnToPos p)}
  "["      {\p s -> Token Lbrack (alexPosnToPos p)}
  "]"      {\p s -> Token Rbrack (alexPosnToPos p)}
  "("      {\p s -> Token Lparen (alexPosnToPos p)}
  ")"      {\p s -> Token Rparen (alexPosnToPos p)}
  ";"      {\p s -> Token Semicolon (alexPosnToPos p)}
  ":"      {\p s -> Token Colon (alexPosnToPos p)}
  ","      {\p s -> Token Comma (alexPosnToPos p)}
  \" [^\"]* \" {\p s -> Token (String s) (alexPosnToPos p)}

  $digit+				{ \p s -> Token (Int (read s)) (alexPosnToPos p) }
  $alpha [$alpha $digit \_ \']*		{ \p s -> Token (Id s) (alexPosnToPos p) }



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

data Token = Token TokenType Pos deriving (Eq, Show)

type Pos = (Int, Int)

alexPosnToPos (AlexPn _ l c) = (l,c)

}
