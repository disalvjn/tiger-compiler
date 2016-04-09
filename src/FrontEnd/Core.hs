module FrontEnd.Core(lexAndParse) where
import FrontEnd.Parse(parse)
import FrontEnd.Lex(tokenize)
import qualified AST.Core as AST
import qualified Symbol as S
import qualified Control.Monad.State.Strict as ST

lexAndParse :: String -> ST.State S.SymbolTable AST.PosExp
lexAndParse = (fmap parse) . tokenize
