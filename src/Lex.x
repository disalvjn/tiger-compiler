{
module Lex (Token(..), AlexPosn(..), alexScanTokens) where
}

%wrapper "posn"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				;
  "--".*				;
  type     {\p s -> Type p }
  var      { \p s -> Var p}
  function { \p s -> Function p}
  break    { \p s -> Break p}
  of       { \p s -> Of p}
  end      { \p s -> End p}
  in       { \p s -> In p}
  nil      { \p s -> Nil p}
  let      { \p s -> Let p}
  do       { \p s -> Do p}
  to       { \p s -> To p}
  for      { \p s -> For p}
  while    { \p s -> While p}
  else     { \p s -> Else p}
  then     { \p s -> Then p}
  if       { \p s -> If p}
  array    { \p s -> Array p}
  ":="     {\p s -> Assign p}
  or       { \p s -> Or p}
  and      { \p s -> And p}
  ">="     { \p s -> Ge p}
  ">"      { \p s -> Gt p}
  "<="     {\p s -> Le p}
  "<"      {\p s -> Lt p}
  "!="     {\p s -> Neq p}
  "="      {\p s -> Eq p}
  "/"      {\p s -> Divide p }
  "*"      {\p s -> Times p}
  "-"      {\p s -> Minus p}
  "+"      {\p s -> Plus p}
  "."      {\p s -> Dot p}
  "{"      {\p s -> Lbrace p}
  "}"      {\p s -> Rbrace p}
  "["      {\p s -> Lbrack p}
  "]"      {\p s -> Rbrack p}
  "("      {\p s -> Lparen p}
  ")"      {\p s -> Rparen p}
  ";"      {\p s -> Semicolon p}
  ":"      {\p s -> Colon p}
  ","      {\p s -> Comma p}
  \" .* \" {\p s -> String p s}

  $digit+				{ \p s -> Int p (read s) }
  $alpha [$alpha $digit \_ \']*		{ \p s -> Id p s }



{
-- Each action has type :: String -> Token

-- The token type:
data Token =
   Type AlexPosn          |
   Var AlexPosn           |
   Function AlexPosn      |
   Break AlexPosn         |
   Of AlexPosn            |
   End AlexPosn           |
   In AlexPosn            |
   Nil AlexPosn           |
   Let AlexPosn           |
   Do AlexPosn            |
   To AlexPosn            |
   For AlexPosn           |
   While AlexPosn         |
   Else AlexPosn          |
   Then AlexPosn          |
   If AlexPosn            |
   Array AlexPosn         |
   Assign AlexPosn        |
   Or AlexPosn            |
   And AlexPosn           |
   Ge AlexPosn            |
   Gt AlexPosn            |
   Le AlexPosn            |
   Lt AlexPosn            |
   Neq AlexPosn           |
   Eq AlexPosn            |
   Divide AlexPosn        |
   Times AlexPosn         |
   Minus AlexPosn         |
   Plus AlexPosn          |
   Dot AlexPosn           |
   Rbrace AlexPosn        |
   Lbrace AlexPosn        |
   Rbrack AlexPosn        |
   Lbrack AlexPosn        |
   Rparen AlexPosn        |
   Lparen AlexPosn        |
   Semicolon AlexPosn     |
   Colon AlexPosn         |
   Comma AlexPosn         |
   String AlexPosn String |
   Int AlexPosn Int       |
   Id AlexPosn String     |
   Eof AlexPosn
   deriving (Eq,Show)

}
