module DirectedGraph(Node, DGraph,
                     empty, newNode, addNode, newEdge, newBiEdge,
                     successors, predecessors, nodes, isEdge) where
import qualified Data.Map as M
import qualified Control.Monad.State.Strict as ST
import Data.Maybe(fromJust)

{-- Node is a wrapper for types that don't support Ord --}
newtype NodeId = NodeId Int deriving (Ord,Eq,Show)
newtype Node a = Node (NodeId, a) deriving (Show)
content (Node (_, x)) = x

instance Eq (Node a) where
  (Node (id1, _)) == (Node (id2, _)) = id1 == id2

instance Ord (Node a) where
  (Node (id1, _)) <= (Node (id2, _)) = id1 <= id2

-- This isn't a great representation...
-- If you're going to be 'mutating' a lot of nodes,
-- then this won't work. But if you're just creating/removing
-- nodes and edges, then this is fine.
data DGraph a = DGraph { dgSucc :: M.Map a [a]
                       , dgPred :: M.Map a [a]
                       , dgNodes :: [a]
                       , dgNextId :: Int
                       } deriving (Show)

empty = DGraph M.empty M.empty [] 0

newNode :: a -> ST.State (DGraph (Node a)) (Node a)
newNode content = do
  dgraph <- ST.get
  let thisId = dgNextId dgraph
      nextId = thisId + 1
      nodeId = NodeId thisId
      node = Node (nodeId, content)
  ST.put $ dgraph {dgNextId = nextId}
  addNode node
  return node

addNode :: Ord a => a -> ST.State (DGraph a) ()
addNode node = do
  orig@(DGraph succ pred nodes nextId) <- ST.get
  let exists = maybe False (const True) (M.lookup node succ)
  if exists
    then return ()
    else let succ' = M.insert node [] succ
             pred' = M.insert node [] pred
             nodes' = node : nodes
             newGraph = DGraph succ' pred' nodes' nextId
         in ST.put newGraph

-- Requires that `from` and `to` are nodes
newEdge :: Ord a => a -> a -> ST.State (DGraph a) ()
newEdge from to = do
  graph@(DGraph succ pred _ _) <- ST.get
  let succ' = M.adjust (to:) from succ
      pred' = M.adjust (from:) to pred
      newGraph = graph {dgSucc = succ', dgPred = pred'}
  ST.put newGraph

newBiEdge :: Ord a => a -> a -> ST.State (DGraph a) ()
newBiEdge n1 n2 = newEdge n1 n2 >> newEdge n2 n1

successors :: Ord a => a -> DGraph a -> [a]
successors node dgraph =
  maybe [] id (M.lookup node $ dgSucc dgraph)

predecessors :: Ord a => a -> DGraph a -> [a]
predecessors node dgraph =
  maybe [] id (M.lookup node $ dgPred dgraph)

nodes :: DGraph a -> [a]
nodes = dgNodes

isEdge :: Ord a => a -> a -> DGraph a -> Bool
isEdge from to dgraph =
  let succs = maybe [] id $ M.lookup from $ dgSucc dgraph
  in to `elem` succs
