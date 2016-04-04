module Assem(Assem(..), Instr(..)) where
-- http://www.mrc.uidaho.edu/mrc/people/jff/digital/MIPSir.html
data Assem temp label = ADD temp temp temp
                      | SUB temp temp temp
                      | ADDI temp temp Int
                      | DIV temp temp temp
                      | MUL temp temp temp
                      | BEQ temp temp label
                      | BNE temp temp label
                      | BGT temp temp label
                      | BLT temp temp label
                      | BGE temp temp label
                      | BLE temp temp label
                      | B label
                      | JR temp
                      | LW temp temp Int
                      | SW temp temp Int
                      | LI temp Int
                      | MOVE temp temp
                      | JAL label
                      | NOOP
                      deriving (Show)

data Instr temp label = Oper (Assem temp label) [temp] [temp] (Maybe [label])
                      | Label label
                      deriving (Show)
