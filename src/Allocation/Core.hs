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
import Data.Maybe(fromJust)
import Control.Monad(forM_, when)

newtype Color = Color Int deriving (Ord, Eq, Show)

data WorkList = Simplify | Freeze | Spilled
type WorkSet = Set.Set S.Temp

-- Mutually disjoint. Every temp is in exactly one.
data WorkSets = WorkSets { _precolored :: WorkSet -- machine registers
                             -- temps neither precolored nor processed
                           , _initial :: [S.Temp]
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

type Move = (S.Temp, S.Temp)
type MoveSet = Set.Set (S.Temp, S.Temp)
type InterferenceGraph = Graph.DGraph S.Temp

data MoveSets = MoveSets { _coalescedMoves :: MoveSet
                           -- source and target interfere; impossible to coalesce
                         , _contrainedMoves :: MoveSet
                           -- no longer considered for coalescing
                         , _frozenMoves :: MoveSet
                           -- enabled for possible coalescing
                         , _worklistMoves :: MoveSet
                           -- not yet ready for coalescing; possibly ready in future
                         , _activeMoves :: MoveSet
                         , _moveMembership :: M.Map S.Temp MoveList }

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

emptyWorkSets = WorkSets Set.empty [] Set.empty Set.empty Set.empty Set.empty
                Set.empty Set.empty Set.empty M.empty

emptyMoveSets = MoveSets Set.empty Set.empty Set.empty Set.empty Set.empty M.empty

emptyRAState = RAState emptyWorkSets emptyMoveSets Graph.empty M.empty M.empty M.empty M.empty

makeLenses ''WorkSets
makeLenses ''MoveSets
makeLenses ''RAState

--isPrecolored u = ST.gets $ Set.member u .  _precolored . _workLists
update f default' = M.alter (Just . maybe default' f)

-- the '$' stands for 'state'
(%=$) x y = x %= ST.execState y

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
        forM_ live $ \temp -> iGraph %=$ Graph.addNode temp
        let edges = [(def,out) | def <- defs, out <- outs]
        case Graph.content node of
          A.Oper (A.MOVE u v) _ _ _ -> do
            -- the move is considered for coalescing by default
            moveSets . worklistMoves %= Set.insert (u, v)
            -- every node that's live here is associated with the move
            forM_ live $ (\n -> tempMoves %= update (Set.insert (u,v)) Set.empty n)
            -- If we have a move like a <- c, where b1... bj are live-out, then
            -- edge (a, bi) should be created only when c /= bi, because a and c might
            -- not interfere, and not creating an edge will allow them to coalesce.
            -- If c is used later, then the edge (a,c) will be created eventually.
            forM_ edges $ (\(x, y) -> when (x /= v && y /= v) (addEdge (x,y)))

          _ -> forM_ edges addEdge


  in mapM_ buildForNode (Graph.nodes control)
