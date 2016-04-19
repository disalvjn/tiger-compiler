{-# LANGUAGE TemplateHaskell #-}
module Allocation.DirectedGraph(Node, DGraph,
                                empty, newNode, addNode, newEdge, newBiEdge,
                                successors, predecessors, nodes, isEdge) where
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Control.Monad.State.Strict as ST
import Data.Maybe(fromJust)
import Control.Lens
import Data.Maybe(fromMaybe)

{-- Node is a wrapper for types that don't support Ord --}
newtype NodeId = NodeId Int deriving (Ord,Eq,Show)
newtype Node a = Node (NodeId, a) deriving (Show)
content (Node (_, x)) = x

instance Eq (Node a) where
  (Node (id1, _)) == (Node (id2, _)) = id1 == id2

instance Ord (Node a) where
  (Node (id1, _)) <= (Node (id2, _)) = id1 <= id2

-- Uses both the adjacency list and adjacency matrix representations
-- because register allocation requires both.
data DGraph a = DGraph { _succs :: M.Map a [a]
                       , _preds :: M.Map a [a]
                       , _edges :: Set.Set (a, a)
                       , _gnodes :: [a]
                       , _nextId :: Int
                       } deriving (Show)

makeLenses ''DGraph

empty = DGraph M.empty M.empty Set.empty [] 0

newNode :: a -> ST.State (DGraph (Node a)) (Node a)
newNode content = do
  thisId <- ST.gets _nextId
  let node = Node (NodeId thisId, content)
  ST.modify $ (\g -> g {_nextId = thisId + 1})
  addNode node
  return node

addNode :: Ord a => a -> ST.State (DGraph a) ()
addNode node = do
  exists <- ST.gets $ maybe False (const True) . M.lookup node . _succs
  ST.when (not exists) (gnodes %= (node:) >>
                        preds %= M.insert node [] >>
                        succs %= M.insert node [])

-- Requires that `from` and `to` are nodes
newEdge :: Ord a => a -> a -> ST.State (DGraph a) ()
newEdge from to = do
  exists <- ST.gets $ isEdge from to
  ST.when (not exists) (succs %= M.adjust (to:) from >>
                        preds %= M.adjust (from:) to >>
                        edges %= Set.insert (from, to))

newBiEdge :: Ord a => a -> a -> ST.State (DGraph a) ()
newBiEdge n1 n2 = newEdge n1 n2 >> newEdge n2 n1

successors :: Ord a => a -> DGraph a -> [a]
successors node dgraph =
  maybe [] id (M.lookup node $ _succs dgraph)

predecessors :: Ord a => a -> DGraph a -> [a]
predecessors node dgraph =
  maybe [] id (M.lookup node $ _preds dgraph)

nodes :: DGraph a -> [a]
nodes = _gnodes

isEdge :: Ord a => a -> a -> DGraph a -> Bool
isEdge from to = Set.member (from, to) . _edges
