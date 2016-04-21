{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Allocation.Core (RAState(..), emptyRAState, build
                       , InterferenceGraph) where
import Control.Lens
import Allocation.Liveness
import qualified Allocation.DirectedGraph as Graph
import qualified CodeGen.Assem as A
import qualified Data.Set as Set
import qualified Symbol as S
import qualified Data.Map as M
import qualified Control.Monad.State.Strict as ST
import Data.Maybe(fromJust, fromMaybe)
import Control.Monad(forM_, when)
import Util

newtype Color = Color Int deriving (Ord, Eq, Show)

data WorkList = Precolored | Initial | Simplify | Freeze | PotSpilled | ActSpilled
              | CoalescedWL | Colored | Select
              deriving (Eq)
type WorkSet = Set.Set S.Temp


-- Mutually disjoint. Every temp is in exactly one.
data WorkSets = WorkSets { _precolored :: WorkSet -- machine registers
                             -- temps neither precolored nor processed
                           , _initial :: WorkSet
                             -- low degree non-move-related nodes
                           , _simplifyWorklist :: WorkSet
                             -- low degree move-related nodes
                           , _freezeWorklist :: WorkSet
                             -- high degree nodes
                           , _spilledWorklist :: WorkSet
                             -- nodes marked for spilling
                           , _spilledNodes :: WorkSet
                             -- coalesced temps; if u <- v is coalesced, v is added to this set
                             -- and u is put into a worklist
                           , _coalescedNodes :: WorkSet
                             -- nodes successfully colored
                           , _coloredNodes :: WorkSet
                             -- temps removed from graph
                           , _selectedStack :: WorkSet
                             -- which worklist is each node a part of?
                           , _nodeMembership :: M.Map S.Temp WorkList}


-- mutually disjoint. Every move is in exactly one.
data MoveList = Coalesced | Constrained | Frozen | WorkList | Active
              deriving (Eq)

type Move = (S.Temp, S.Temp)
type MoveSet = Set.Set (S.Temp, S.Temp)
type InterferenceGraph = Graph.DGraph S.Temp

data MoveSets = MoveSets { _coalescedMoves :: MoveSet
                           -- source and target interfere; impossible to coalesce
                         , _constrainedMoves :: MoveSet
                           -- no longer considered for coalescing
                         , _frozenMoves :: MoveSet
                           -- enabled for possible coalescing
                         , _worklistMoves :: MoveSet
                           -- not yet ready for coalescing; possibly ready in future
                         , _activeMoves :: MoveSet
                         , _moveMembership :: M.Map Move MoveList }

data RAState = RAState { _workLists :: WorkSets
                       , _moveSets :: MoveSets
                       , _iGraph :: InterferenceGraph
                         -- degrees changes when nodes are simplified, coalesced, etc.
                       , _degree :: M.Map S.Temp Int
                       , _tempMoves :: M.Map S.Temp MoveSet
                         -- when move u <- v is coalesced and v is entered into
                         -- MoveSets' coalescedNodes, alias(v) = u
                       , _alias :: M.Map S.Temp S.Temp
                       , _color :: M.Map S.Temp Color
                       }

emptyWorkSets = WorkSets Set.empty Set.empty Set.empty Set.empty Set.empty Set.empty
                Set.empty Set.empty Set.empty M.empty

emptyMoveSets = MoveSets Set.empty Set.empty Set.empty Set.empty Set.empty M.empty

emptyRAState = RAState emptyWorkSets emptyMoveSets Graph.empty M.empty M.empty M.empty M.empty

makeLenses ''WorkSets
makeLenses ''MoveSets
makeLenses ''RAState

fromWorkList Precolored = workLists . precolored
fromWorkList Initial = workLists . initial
fromWorkList Simplify = workLists . simplifyWorklist
fromWorkList Freeze = workLists . freezeWorklist
fromWorkList PotSpilled = workLists . spilledWorklist
fromWorkList ActSpilled = workLists . spilledNodes
fromWorkList CoalescedWL = workLists . coalescedNodes
fromWorkList Colored = workLists . coloredNodes
fromWorkList Select = workLists . selectedStack

fromMoveList Active = moveSets . activeMoves
fromMoveList WorkList = moveSets . worklistMoves
fromMoveList Frozen = moveSets . frozenMoves
fromMoveList Constrained = moveSets . constrainedMoves
fromMoveList Coalesced = moveSets . coalescedMoves

addToWL :: S.Temp -> WorkList -> ST.State RAState ()
addToWL temp wl = do
  memberOf <- ST.gets $ M.lookup temp . _nodeMembership . _workLists
  whenJust memberOf (\ wl -> (fromWorkList wl) %= Set.delete temp)
  workLists . nodeMembership %= M.insert temp wl
  (fromWorkList wl) %= Set.insert temp

addToML :: Move -> MoveList -> ST.State RAState ()
addToML move ml = do
  memberOf <- ST.gets $ M.lookup move . _moveMembership . _moveSets
  whenJust memberOf (\ ml -> (fromMoveList ml) %= Set.delete move)
  moveSets . moveMembership %= M.insert move ml
  (fromMoveList ml) %= Set.insert move



--isPrecolored u = ST.gets $ Set.member u .  _precolored . _workLists
update f default' = M.alter (Just . maybe default' f)
getDegree temp = ST.gets $ fromMaybe 0 . M.lookup temp . _degree

isMoveRelated temp = fmap (not . Set.null) . nodeMoves $ temp
isActiveMove temp = ST.gets $ (== Active) . fromMaybe WorkList
                    . M.lookup temp . _moveMembership . _moveSets

-- Get every move related to a temp that is either
-- currently being considered for coalescing or could be in the future.
nodeMoves :: S.Temp -> ST.State RAState MoveSet
nodeMoves temp = do
  movesForTemp <- ST.gets $ fromMaybe Set.empty . M.lookup temp . _tempMoves
  actives <- ST.gets $  _activeMoves . _moveSets
  worklists <- ST.gets $  _worklistMoves . _moveSets
  return $ movesForTemp `Set.union` (actives `Set.intersection` worklists)

-- Every node that's neither coalesced nor selected adjacent to another
adjacent :: S.Temp -> ST.State RAState (Set.Set S.Temp)
adjacent temp = do
  -- The graph is assumed to be constructed so that it simulates un-directedness
  allAdj <- ST.gets $ Set.fromList . Graph.successors temp . _iGraph
  selected <- ST.gets $ _selectedStack . _workLists
  coalesced <- ST.gets $ _coalescedNodes . _workLists
  return $ allAdj `Set.difference` (selected `Set.union` coalesced)


doTo :: (s -> b) -> (b -> ST.State s ()) -> ST.State s ()
doTo accessor action = ST.gets accessor >>= action

-- the '$' stands for 'state'
(%=$) x y = x %= ST.execState y

--allocateRegisters :: FlowGraph -> LivenessMap -> M.Map S.Temp Color

-- Pre-conditions : None
-- Post-conditions :
--     1) _iGraph contains a complete interference graph.
--     2) _degree is complete
--     3) _moveSets . _worklistMoves is complete
--     4) tempMoves is complete
--     5) _workLists . _initial contains every temp
build :: FlowGraph -> LivenessMap -> ST.State RAState ()
build (FlowGraph control def use) liveness =
  -- to-do optimization : don't represent precolored nodes in adjacency list representation.
  -- Post-conditions: degrees of u,v incremented. obviously the edge.
  let addEdge (u, v) =
        when (u /= v) $ do iGraph %=$ Graph.newBiEdge u v
                           degree %= update succ 0 u
                           degree %= update succ 0 v

      buildForNode node = do
        let defs = fromJust $ M.lookup node def
            outs = Set.toList . snd . fromJust $ M.lookup node liveness
            live = defs ++ outs
        forM_ live $ \temp -> iGraph %=$ Graph.addNode temp >> addToWL temp Initial
        let edges = [(def,out) | def <- defs, out <- outs]
        case Graph.content node of
          A.Oper (A.MOVE u v) _ _ _ -> do
            -- the move is considered for coalescing by default
            addToML (u,v) WorkList
            -- every node that's live here is associated with the move
            forM_ live $ (\n -> tempMoves %= update (Set.insert (u,v)) Set.empty n)
            -- If we have a move like a <- c, where b1... bj are live-out, then
            -- edge (a, bi) should be created only when c /= bi, because a and c might
            -- not interfere, and not creating an edge will allow them to coalesce.
            -- If c is used later, then the edge (a,c) will be created eventually.
            forM_ edges $ (\(x, y) -> when (x /= v && y /= v) (addEdge (x,y)))

          _ -> forM_ edges addEdge


  in mapM_ buildForNode (Graph.nodes control)

-- Preconditions:
--    1) It's known whether every temp is move related.
--    2) _initial . _workLists contains every temp.
-- Postconditions:
--    1) Temps of degree >= k are placed into the potential spill worklist. [1]
--    2) Temps of degree < k and move-related are placed in the freeze worklist. [2]
--    3) Temps of degree < k and not move-related are placed in the simplify worklist.
-- [1] : We do optimistic spilling; later when assigning colors, we'll know whether this
--       is an actual spill. The reason we don't know now: consider a graph
--       ( {a,b,c,d} , { {a,b}, {c, b}, {d, b}} ) w/ k = 3. Even though b has 3 edges,
--       we can still color the graph with <= 3 colors -- let b be Black and everything else White.
-- [2] : If a temp is move-related, then we'd prefer for it to be coalesced rather than simplified.
--       Placing it on the freeze worklist gives it a chance to be coalesced; if coalescing fails,
--       then it'll be put on the simplify worklist.
makeWorkLists :: Int -> ST.State RAState ()
makeWorkLists k =
  let decideTemp temp = do
        deg <- getDegree temp
        moveRelated <- isMoveRelated temp
        let workList = if deg >= k then PotSpilled else if moveRelated then Freeze else Simplify
        addToWL temp workList
  in doTo (_initial . _workLists) (mapM_ decideTemp)

-- Conceptually, remove nodes of insignificant degree from the interference graph.
-- Each time a node is removed, decrement the degrees of its neighbors. If any neighbor's [n]
-- degree drops below K, then 'unspill' n and enable every active move related to n and n's neighbors
-- for coalescing.
simplify :: Int -> ST.State RAState ()
simplify k =
  let simplifyTemp temp = do
        -- Since we (conceptually) remove the node from the graph, we need to decrement
        -- the degrees of its neighbors.
        addToWL temp Select
        adjacent temp >>= mapM_ decrementDegree

      decrementDegree temp = do
        deg <- fmap pred $ getDegree temp
        degree %= M.insert temp deg
        -- if the new degree is < k, then we can consider the node for simplifying/coalescing
        when (deg < k) $ unspill temp

      unspill temp = do
        adjacent temp >>= mapM_ enableMove
        moveRelated <- isMoveRelated temp
        let wl = if moveRelated then Freeze else Simplify
        addToWL temp wl

      enableMove temp = do
        moves <- nodeMoves temp
        forM_ moves $ \m -> do
          activeMove <- isActiveMove m
          when activeMove (addToML m WorkList)

  in doTo (_simplifyWorklist . _workLists) (mapM_ simplifyTemp)
