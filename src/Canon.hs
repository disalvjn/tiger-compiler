module Canon(canonicize) where

import qualified Symbol as S
import Tree

import qualified Control.Monad.State.Strict as ST
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Maybe(fromJust)
import Data.List(find)
import Debug.Trace (trace)
import Control.Monad(liftM, (<=<))

maybeOr x y =
    case x of
      Nothing -> y
      Just _ -> x

spy x = trace (show x) x

{-- This module very closely follows Appel's provided code
  (as in, I consulted it before hand, mulled it over, tried re-implementing it,
  then double checked my work against it) Appel's provided code which can be found here
  http://www.cs.princeton.edu/~appel/modern/ml/chap8/ in canon.sml --}

canonicize :: Stm -> ST.State S.SymbolTable [Stm]
canonicize stm = do
  linear <- linearize stm
  blocks <- basicBlocks (spy linear)
  traced <- traceSchedule (spy blocks)
  return (spy traced)

{-- Post conditions:
  1. The parent of every Stm node is a Stm
  2. The result of every function call is immediately moved into a new temporary.
--}

linearize :: Stm -> ST.State S.SymbolTable [Stm]
linearize = liftM preorderSeq . walkStm

preorderSeq :: Stm -> [Stm]
preorderSeq stm =
    go stm []
    where go (Seq x y) = (go x) . (go y)
          go x = (x:)

commutesWith :: Exp -> Stm -> Bool
commutesWith (Const _) _ = True
commutesWith  (Name _) _ = True
commutesWith _ (ExpStm (Const _)) = True
commutesWith _ _ = False

mseq :: Maybe Stm -> Maybe Stm -> Maybe Stm
mseq x y =
    case (x, y) of
      (Nothing, Nothing) -> Nothing
      (Just a, Nothing) -> Just a
      (Nothing, Just b) -> Just b
      (Just a, Just b) -> Just $ Seq a b

reorder :: [Exp] -> ST.State S.SymbolTable (Maybe Stm, [Exp])
reorder exps =
    case exps of
      [] -> return (Nothing, [])
      (exp:es) -> do
                (reorderStms, es') <- reorder es
                (expStms, exp') <- walkExp exp
                let allStms = expStms `mseq` reorderStms
                case reorderStms of
                  Nothing -> return (allStms, exp':es')
                  Just stm -> if exp' `commutesWith` stm
                              then return (allStms, exp' : es')
                              else do
                                temp <- S.genTemp
                                let move = Move (Temp temp) exp'
                                return ((Just move) `mseq` allStms, (Temp temp) : es')

reorderExp :: [Exp] -> ([Exp] -> Exp) -> ST.State S.SymbolTable (Maybe Stm, Exp)
reorderExp exps build = do
  (stm, exps') <- reorder exps
  return (stm, build exps')

reorderStm :: [Exp] -> ([Exp] -> Stm) -> ST.State S.SymbolTable Stm
reorderStm exps build = do
  (stm, exps') <- reorder exps
  let rebuiltStm = build exps'
  return $ maybe rebuiltStm (\s -> Seq s rebuiltStm) stm

walkStm :: Stm -> ST.State S.SymbolTable Stm
walkStm stm =
  case stm of
    Seq x y -> do
             x' <- walkStm x
             y' <- walkStm y
             return $ Seq x' y'
    Jump exp labels -> reorderStm [exp] (\[exp] -> Jump exp labels)
    CJump op e1 e2 l1 l2 -> reorderStm [e1, e2] (\[e1, e2] -> CJump op e1 e2 l1 l2)
    Move (Temp t) (Call f args) -> do
             (stms, newCall) <- reorderExp (f:args) (\ (f:args) -> Call f args)
             let newMove = Move (Temp t) newCall
             return $ maybe newMove (\s -> Seq s newMove) stms
    -- The lhs of a move is treated as a destination, not really an expression.
    Move (Temp t) e1 -> reorderStm [e1] (\ [e1] -> Move (Temp t) e1)
    Move e1 e2 -> reorderStm [e1, e2] (\ [e1, e2] -> Move e1 e2)
    ExpStm exp -> reorderStm [exp] (\ [exp] -> ExpStm exp)
    other -> return other

walkExp :: Exp -> ST.State S.SymbolTable (Maybe Stm, Exp)
walkExp exp =
    case exp of
      Binop op l r -> reorderExp [l, r] (\ [l, r] -> Binop op l r)
      Mem e -> reorderExp [e] (\ [e] -> Mem e)
      Eseq s e -> do
               (eseqStm, e') <- walkExp e
               stm <- walkStm s
               return ((Just stm) `mseq` eseqStm, e')
      Call f args -> do
               temp <- S.genTemp
               walkExp (Eseq (Move (Temp temp) (Call f args)) (Temp temp))
      other -> return (Nothing, other)


{-- A basic block is a sequence of statements s.t.
  * The first statement is a label
  * The last statement is a Jump or Cjump
  * There are no other labels, jumps, or cjumps in the block

--}
-- starting label, list of stms EXCLUDING starting label and end jump, end jump
data Block = Block S.Label [Stm] Stm
             deriving (Show)

basicBlocks :: [Stm] -> ST.State S.SymbolTable ([Block], S.Label) -- blocks, epilogue label
basicBlocks stms = do
  epilogue <- trace "begin basicBlocks" S.genLabel
  let go stms block blocks curLabel =
          let nextBlock restStms jump =
                  let blockStms = reverse block
                      blockDatum = Block (fromJust curLabel) blockStms jump
                  in go restStms [] (blockDatum : blocks) curLabel
          in case block of
            -- If we're starting a new block
            [] -> case stms of
                    -- and there are no statements left, then we're done
                    [] -> return $ reverse blocks
                    -- if the next statement is a label, we're good
                    (labStm@(Label lab) : stms') -> go stms' [labStm] blocks (Just lab)
                    -- if the next statement isn't a label, we need to create one
                    _ -> do
                        entryLabel <- S.genLabel
                        go stms [Label entryLabel] blocks (Just entryLabel)

            -- if we're in the middle of a block
            _ -> case stms of
                    -- and no statements remain, end block w/ a jump to the epilogue label
                    [] -> nextBlock [] $ Jump (Name epilogue) [epilogue]

                    -- if the next statement is a jump or cjump, we're done with this block
                    (jump@(Jump _ _ ) : stms') -> nextBlock stms' jump
                    (cjump@(CJump _ _ _ _ _) : stms') -> nextBlock stms' cjump

                    -- if the next statement is a label, create a jump to it and end the block.
                    (lab@(Label l) : _) -> nextBlock stms $ Jump (Name l) [l]

                    -- otherwise, just add the stm to the block
                    (stm : stms') -> go stms' (stm : block) blocks curLabel

  blocks <- go stms [] [] Nothing
  return (blocks, epilogue)
blocksByLabels :: [Block] -> M.Map S.Label Block
blocksByLabels = M.fromList . map (\ block@(Block lab _ _) -> (lab, block))

{--
    A trace is any sequence of stms that could be consecutively executed in the program.
  The goal of traceSchedule is to find a minimal set of traces that exactly covers the program.

    Also, the blocks should be re-arranged so that:
        * Every unconditional jump is immediately followed by its target label (allowing us to merge
          the two blocks)
        * Conditional jumps are immediately followed by their fall-through false branch.

    Once the blocks have been re-arranged, they're flattened back into a list of statements
  for simplicity.
--}

type Trace = [Block]

traceSchedule :: ([Block], S.Label) -> ST.State S.SymbolTable [Stm]
traceSchedule (blocks, epilogueLabel) = do
  let traces = trace "findTraces" (findTraces blocks)
  newStms <- merge epilogueLabel $ concat traces
  return newStms

findTraces :: [Block] -> [Trace]
findTraces blocks =
    let byLabels = blocksByLabels blocks
        startingWith label =  M.lookup label byLabels

        unmarkedSuccessor marked stm =
            let isMarked label = Set.member label marked
            in case stm of
                 Jump _ labels -> find (not . isMarked) labels >>= startingWith
                 CJump _ _ _ f t -> case (not . isMarked $ f, not . isMarked $ t) of
                                      (True, True) -> startingWith f `maybeOr` startingWith t
                                      (True, False) -> startingWith f
                                      (False, True) -> startingWith t
                                      _ -> Nothing
                 _ -> Nothing

        traces blocks marked tlist =
              case blocks of
                [] -> reverse tlist
                (block : blocks') -> traceBlock blocks' marked tlist block []

        traceBlock blocks marked tlist block@(Block label _ jump) trace =
            let nextTrace marked trace = traces blocks marked (reverse trace : tlist)
            in if label `Set.member` marked
               then nextTrace marked trace
               else let marked' = Set.insert label marked
                        nextBlock = unmarkedSuccessor marked' jump
                    in case nextBlock of
                         Nothing -> nextTrace marked' (block : trace)
                         Just next -> traceBlock blocks marked' tlist next (block : trace)

    in traces blocks Set.empty []


-- In the traces generated above, not all Cjumps are necessarily followed by their
-- false label. Therefore, once we flatten the [Trace] into [Block], we need to scan
-- through the blocks, and for every Cjump, if it's followed by its :
--    false label, do nothing.
--    true label, switch the true and false labels and negate the relop.
--    neither t/f label, create a new false label f' whose sole statement is JUMP f.
-- If a Jump is followed directly by its only label, remove the jump and label.
merge :: S.Label -> [Block] -> ST.State S.SymbolTable [Stm]
merge epilogue [] = return [Label epilogue]
merge epilogue (Block lab stms jump : restBlocks) = do
  restStms <- merge epilogue restBlocks
  let joinedStms = (Label lab) : (stms ++ (jump : restStms))
      joinedStmsWithNewCJump = do
        f' <- S.genLabel
        let (CJump op e1 e2 t f) = jump
            newJump = [CJump op e1 e2 t f', Label f', Jump (Name f) [f]]
        return $ (Label lab) : (stms ++ newJump ++ restStms)
  case (jump, restStms) of
    (Jump _ [label], (Label l : restStms')) ->
        if label == l
        then return $ (Label lab) : (stms ++ restStms')
        else return joinedStms

    (CJump op e1 e2 t f, (Label l : _)) ->
        if f == l then return joinedStms else
        if t ==l then let newJump = CJump (Tree.negate op) e1 e2 f t
                          in return $ (Label lab) : (stms ++ (newJump : restStms))
        else joinedStmsWithNewCJump

    (CJump _ _ _ _ _, _) -> joinedStmsWithNewCJump

    _ -> return joinedStms
