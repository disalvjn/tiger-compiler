module CodeGen.Assem(Assem(..), Instr(..), sourceRegs, destRegs, jumpsTo,
                     canFallThrough, format) where
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
                      | J label
                      | JAL label
                      | JR temp
                      | LW temp temp Int -- into, from, from's offset
                      | SW temp temp Int -- into, from, into's offset
                      | LI temp Int
                      | MOVE temp temp
                      | NOOP
                      deriving (Show)

data Instr temp label = Oper (Assem temp label) [temp] [temp] (Maybe [label]) -- assem, src, dest
                      | Label label
                      deriving (Show)

canFallThrough :: Instr temp label -> Bool
canFallThrough (Oper (J _) _ _ _) = False
canFallThrough (Oper (JR _) _ _ _) = False
-- This was the cause of a nasty, nasty bug!
-- I'm leaving this here as a reminder of what happens when you don't think! stupid stupid!
-- of course, it raises the question of whether J and JR can "fall through", but I'll
-- think about that some other time...
-- canFallThrough (Oper (JAL _) _ _ _) = False
canFallThrough _ = True

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


formatAssem :: Assem temp label -> (temp -> String) -> (label -> String) -> String
formatAssem (ADD t1 t2 t3) ts ls = "add " ++ (ts t1) ++ ", " ++ (ts t2) ++ ", " ++ (ts t3)
formatAssem (ADDI t1 t2 i) ts ls = "addi " ++ (ts t1) ++ ", " ++ (ts t2) ++ ", " ++ (show i)
formatAssem (SUB t1 t2 t3) ts ls = "sub " ++ (ts t1) ++ ", " ++ (ts t2) ++ ", " ++ (ts t3)
formatAssem (DIV t1 t2 t3) ts ls = "div " ++ (ts t1) ++ ", " ++ (ts t2) ++ ", " ++ (ts t3)
formatAssem (MUL t1 t2 t3) ts ls = "mul " ++ (ts t1) ++ ", " ++ (ts t2) ++ ", " ++ (ts t3)
formatAssem (BEQ t1 t2 l1) ts ls = "beq " ++ (ts t1) ++ ", " ++ (ts t2) ++ ", " ++ (ls l1)
formatAssem (BNE t1 t2 l1) ts ls = "bne " ++ (ts t1) ++ ", " ++ (ts t2) ++ ", " ++ (ls l1)
formatAssem (BGT t1 t2 l1) ts ls = "bgt " ++ (ts t1) ++ ", " ++ (ts t2) ++ ", " ++ (ls l1)
formatAssem (BLT t1 t2 l1) ts ls = "blt " ++ (ts t1) ++ ", " ++ (ts t2) ++ ", " ++ (ls l1)
formatAssem (BGE t1 t2 l1) ts ls = "bge " ++ (ts t1) ++ ", " ++ (ts t2) ++ ", " ++ (ls l1)
formatAssem (BLE t1 t2 l1) ts ls = "ble " ++ (ts t1) ++ ", " ++ (ts t2) ++ ", " ++ (ls l1)
formatAssem (J l) ts ls = "j " ++ (ls l)
formatAssem (JR t) ts ls = "jr " ++ (ts t)
formatAssem (JAL l) ts ls = "jal " ++ (ls l)
formatAssem (LW t1 t2 i) ts ls = "lw " ++ (ts t1) ++ ", " ++ (show i) ++ "(" ++ (ts t2) ++ ")"
formatAssem (SW t1 t2 i) ts ls = "sw " ++ (ts t2) ++ ", " ++ (show i) ++ "(" ++ (ts t1) ++ ")"
formatAssem (LI t1 i) ts ls = "li " ++ (ts t1) ++ ", " ++ (show i)
formatAssem (MOVE t1 t2) ts ls = "move " ++ (ts t1) ++ ", " ++ (ts t2)
formatAssem NOOP _ _ = "nop"

format :: Instr temp label -> (temp -> String) -> (label -> String) -> String
format (Oper instr _ _ _) ts ls = formatAssem instr ts ls
format (Label l) ts ls = "\n" ++ ls l ++ ":"
