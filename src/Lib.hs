import Parse(parse)
import Lex(alexScanTokens)
import AST
import Control.Applicative

p file = parse <$> alexScanTokens <$> readFile file

p2 = parse . alexScanTokens
