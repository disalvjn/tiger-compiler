This is an attempt to learn simultaneously about compilers (from Andrew Appel's Modern Compiler Implementation in ML) and Haskell.

To build, generate the lexer with `alex src/Lex.x` and the parser with `happy src/Parse.y`. (First you will probably need to install alex and happy). Then run `stack build`. It might work! The tests can be run with `stack test`.
