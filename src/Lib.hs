import Parse(parse)
import Lex(tokenize)
import AST
import Control.Applicative

p file = parse <$> snd . tokenize <$> readFile file

p2 = parse . snd . tokenize
