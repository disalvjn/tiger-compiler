module CodeGen.Assem(Assem(..), Instr(..), sourceRegs, destRegs, jumpsTo) where
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

data Instr temp label = Oper (Assem temp label) [temp] [temp] (Maybe [label]) -- assem, src, dest
                      | Label label
                      deriving (Show)

isMove (Oper (MOVE _ _) _ _ _) = True
isMove _ = False

sourceRegs :: Instr temp label -> [temp]
sourceRegs (Oper _ src _ _) = src
sourceRegs _ = []

destRegs :: Instr temp label -> [temp]
destRegs (Oper _ _ dest _) = dest
destRegs _ = []

jumpsTo :: Instr temp label -> [label]
jumpsTo (Oper _ _ _ list) = maybe [] id list
jumpsTo _ = []
