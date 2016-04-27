-- TO DO :
--   Rewrite program
--   More efficient work list representations
--   Spill priorities / spill heuristic

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Allocation.Core (RAState(..), emptyRAState, build, allocateRegisters
                       , InterferenceGraph) where
import Control.Lens
import Allocation.Liveness
import qualified Allocation.DirectedGraph as Graph
import qualified CodeGen.Assem as A
import qualified Data.Set as Set
import qualified Symbol as S
import qualified Data.Map as M
import qualified Control.Monad.State.Strict as ST
import Data.Maybe(fromMaybe, catMaybes, isJust)
import Control.Monad(forM_, when, unless)
import Util
import Debug.Trace(trace)

type Color = Int
type Instr = A.Instr S.Temp S.Label

data WorkList = Precolored | Initial | Simplify | Freeze | PotSpilled | ActSpilled
              | CoalescedWL | Colored | Select
              deriving (Show, Eq)
type WorkSet = Set.Set S.Temp

data MoveList = Coalesced | Constrained | Frozen | WorkList | Active
              deriving (Show, Eq)

type Move = (S.Temp, S.Temp)
type MoveSet = Set.Set (S.Temp, S.Temp)
type InterferenceGraph = Graph.DGraph S.Temp

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
  lg ("Adding " ++ (show temp) ++ " to worklist " ++ (show wl))
  memberOf <- ST.gets $ M.lookup temp . _nodeMembership . _workLists
  let add = do
        workLists . nodeMembership %= M.insert temp wl
        (fromWorkList wl) %= Set.insert temp
  case memberOf of
    Just Precolored -> --lg ("trying to add precolored " ++ (show temp) ++ " to " ++ (show wl)) >>
                       return ()
    Just wl -> (fromWorkList wl) %= Set.delete temp >> add
    Nothing -> add

popWL :: WorkList -> ST.State RAState S.Temp
popWL wl = do
  work <- use $ fromWorkList wl
  let (elem, set) = Set.deleteFindMin work
  fromWorkList wl .= set
  return elem

addToML :: Move -> MoveList -> ST.State RAState ()
addToML move ml = do
  memberOf <- ST.gets $ M.lookup move . _moveMembership . _moveSets
  whenJust memberOf (\ ml -> (fromMoveList ml) %= Set.delete move)
  moveSets . moveMembership %= M.insert move ml
  (fromMoveList ml) %= Set.insert move

update f default' = M.alter (Just . maybe default' f)

isPrecolored :: S.Temp -> RAState -> Bool
isPrecolored u = Set.member u .  _precolored . _workLists

getDegree :: S.Temp -> RAState -> Int
getDegree temp state =  fromMaybe 0 . M.lookup temp . _degree $ state

isMoveRelated :: S.Temp -> RAState -> Bool
isMoveRelated temp state = (not . Set.null) . nodeMoves temp $ state

moveIsOfType :: MoveList -> Move -> RAState -> Bool
moveIsOfType typ move state = (== typ) . fromMaybe WorkList
                              . M.lookup move . _moveMembership . _moveSets $ state

isActiveMove :: Move -> RAState -> Bool
isActiveMove = moveIsOfType Active

isCoalesced :: Move -> RAState -> Bool
isCoalesced = moveIsOfType Coalesced

isFreeze :: S.Temp -> RAState -> Bool
isFreeze temp = Set.member temp . _freezeWorklist . _workLists

interferes :: Move -> RAState -> Bool
interferes (u, v) = Graph.isEdge u v . _iGraph

-- Get every move related to a temp that is either
-- currently being considered for coalescing or could be in the future.
nodeMoves :: S.Temp -> RAState -> MoveSet
nodeMoves temp state =
  let movesForTemp = fromMaybe Set.empty . M.lookup temp . _tempMoves $ state
      actives =  _activeMoves . _moveSets $ state
      worklists =  _worklistMoves . _moveSets $ state
  in movesForTemp `Set.union` (actives `Set.intersection` worklists)

getAlias :: S.Temp -> RAState -> S.Temp
getAlias temp state =
  if Set.member temp . _coalescedNodes . _workLists $ state
  then let Just alias = M.lookup temp . _alias $ state
       in getAlias alias state
  else temp

allAdjacent :: S.Temp -> RAState -> Set.Set S.Temp
-- The graph is assumed to be constructed so that it simulates un-directedness
allAdjacent temp state = Set.fromList . Graph.successors temp . _iGraph $ state

-- Every node that's neither coalesced nor selected adjacent to another
adjacent :: S.Temp -> RAState -> Set.Set S.Temp
adjacent temp state =
  let allAdj = allAdjacent temp state
      selected = _selectedStack . _workLists $ state
      coalesced =  _coalescedNodes . _workLists $ state
      pre = _precolored . _workLists $ state
  in allAdj `Set.difference` (selected `Set.union` coalesced `Set.union` pre)

colorOf :: S.Temp -> RAState -> Maybe Color
colorOf temp state = M.lookup temp . _color $ state

doTo :: (s -> b) -> (b -> ST.State s ()) -> ST.State s ()
doTo accessor action = ST.gets accessor >>= action

-- the '$' stands for 'state'
(%=$) x y = x %= ST.execState y


allocateRegisters :: Int -> M.Map S.Temp Color -> Set.Set Color -> [Instr]
                  -> ST.State S.SymbolTable ([Instr], (M.Map S.Temp Color))
allocateRegisters k precolored reserved program =
  let flowgraph = flowGraph program
      livenessMap = liveness flowgraph

      loop = do
        doSimplify <- ST.gets $ not . Set.null . _simplifyWorklist . _workLists
        doCoalesce <- ST.gets $ not . Set.null . _worklistMoves . _moveSets
        doFreeze <- ST.gets $ not . Set.null . _freezeWorklist . _workLists
        doSpill <- ST.gets $ not . Set.null . _spilledWorklist . _workLists
        if doSimplify then simplify k >> loop
          else if doCoalesce then coalesce k >> loop
               else if doFreeze then freeze k >> loop
                    else if doSpill then selectSpill k >> loop else return ()

      go = do
        printFlowGraph flowgraph
        printLivenessMap livenessMap
        --lg $ show flowgraph
        build precolored flowgraph livenessMap
        makeWorkLists k
        loop
        assignColors k reserved
        ST.gets $ _spilledNodes . _workLists

      run =
        let (spilledNodes, raState) = ST.runState go emptyRAState
        in if Set.null spilledNodes
           then let colors = _color raState
                in return (removeCoalescedMoves colors program, colors)
           else rewriteProgram spilledNodes program >>= allocateRegisters k precolored reserved

  in run


-- to-do optimization : don't represent precolored nodes in adjacency list representation.
-- Post-conditions: degrees of u,v incremented. obviously the edge.
addEdge (u, v) =
    when (u /= v) $ do iGraph %=$ Graph.newBiEdge u v
                       degree %= update succ 0 u
                       degree %= update succ 0 v

-- Pre-conditions : None
-- Post-conditions :
--     1) _iGraph contains a complete interference graph.
--     2) _degree is complete
--     3) _moveSets . _worklistMoves is complete
--     4) tempMoves is complete
--     5) _workLists . _initial contains every temp
build :: M.Map S.Temp Color -> FlowGraph -> LivenessMap -> ST.State RAState ()
build precolored (FlowGraph control def use) liveness =
  let buildPrecolored = do
        color .= precolored
        forM_ (M.keys precolored) (\t -> addToWL t Precolored)

      buildForNode node = do
        let defs = fromMaybe [] $ M.lookup node def
            outs = Set.toList . snd . fromMaybe (Set.empty, Set.empty) $ M.lookup node liveness
            live = defs ++ outs
        forM_ live $ \temp -> iGraph %=$ Graph.addNode temp >> addToWL temp Initial
        let edges = [(def,out) | def <- defs, out <- outs]
        case Graph.content node of
          A.Oper (A.MOVE u v) _ _ _ -> do
            -- the move is considered for coalescing by default
            addToML (u,v) WorkList
            -- every node that's live here is associated with the move
            forM_ live $ (\n -> tempMoves %= update (Set.insert (u,v)) Set.empty n)
            -- If we have a move like u <- v, where b1... bj are live-out, then
            -- edge (u, bi) should be created only when v /= bi, because u and v might
            -- not interfere, and not creating an edge will allow them to coalesce.
            -- If v is used later, then the edge (u,v) will be created eventually.
            forM_ edges $ (\(x, y) -> unless (x == u && y == v) (addEdge (x,y)))

          _ -> forM_ edges addEdge


  in buildPrecolored >> mapM_ buildForNode (Graph.nodes control)  >> doTo _iGraph (lg . show)

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
        state <- ST.get
        let (deg, moveRelated) = (getDegree temp state, isMoveRelated temp state)
            workList = if deg >= k then PotSpilled else if moveRelated then Freeze else Simplify
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
        ST.gets (adjacent temp) >>= mapM_ (decrementDegree k)

  in doTo (_simplifyWorklist . _workLists) (mapM_ simplifyTemp)

enableMove :: S.Temp -> ST.State RAState ()
enableMove temp = do
  moves <- ST.gets $ nodeMoves temp
  forM_ moves $ \m -> do
    activeMove <- ST.gets $ isActiveMove m
    when activeMove (addToML m WorkList)

decrementDegree :: Int -> S.Temp -> ST.State RAState ()
decrementDegree k temp = do
  deg <- fmap pred . ST.gets $ getDegree temp
  degree %= M.insert temp deg
  -- if the new degree is < k, then we can consider the node for simplifying/coalescing
  when (deg < k) $ unspill temp
    where unspill temp = do
            ST.gets (adjacent temp) >>= mapM_ enableMove
            moveRelated <- ST.gets $ isMoveRelated temp
            let wl = if moveRelated then Freeze else Simplify
            addToWL temp wl

coalesce :: Int -> ST.State RAState ()
coalesce k =
  let coalesceMove m@(x', y') = do
        state <- ST.get
        let (x, y) =  (getAlias x' state, getAlias y' state)
        -- [1] v precolored -> u precolored
        let (u, v) = if isPrecolored y state then (y, x) else (x, y)
        determine m (u, v) state

      determine move@(x', y') (u, v) state
        | isCoalesced move state = return ()
      -- if u == v, then the move can be trivially coalesced. Then consider simplifying u.
        | u == v =
          addToML move Coalesced >> consider u

      -- if v is precolored, then u is precolored, so u,v interfere (see [1])
      -- if u,v interfere, then they cannot be coalesced, and the move is constrained.
      -- consider both u,v for simplification.
        | isPrecolored v state || interferes (u, v) state =
          addToML move Constrained >> consider u >> consider v

      -- If u and v can be coalesced according to either the George or Briggs heuristic
      -- (George is used on precolored to prevent looking at the precolored's huge adj list)
      -- then combine u,v and consider u for simplification.
        | (isPrecolored u state && georgeCoalesceHeuristic u v state)
          || ((not $ isPrecolored u state) && briggsCoalesceHeuristic u v state) =
            addToML (x', y') Coalesced >> addToML (y', x') Coalesced
            >> combine (u, v) >> consider u

      -- maybe we can coalesce it in the future
        | otherwise = addToML move Active

      -- Add a node to the Simplify worklist if it's not precolored, isn't move related, and of
      -- insignificant degree.
      consider u = do
        st <- ST.get
        when ((not $ isPrecolored u st) && (not $ isMoveRelated u st) && (getDegree u st < k))
          $ addToWL u Simplify

      combine (u, v) = do
        lg $ "Combining: " ++ (show u) ++ " and " ++ (show v)
        addToWL v CoalescedWL
        alias %= M.insert v u
        -- every move associated with v becomes associated with u
        uMoves <- ST.gets $ fromMaybe Set.empty . M.lookup u . _tempMoves
        vMoves <- ST.gets $ fromMaybe Set.empty . M.lookup v . _tempMoves
        tempMoves %= M.insert u (uMoves `Set.union` vMoves)
        -- every active move associated with v is put in the worklist for coalescing
        enableMove v
        -- every neighbor of v becomes a neighbor of u
        vNeighbors <- ST.gets $ {--allAdjacent v--} adjacent v
        forM_ vNeighbors $ \t -> addEdge (t, u) >> decrementDegree k t
        -- if u is of significant degree and frozen, it becomes a potential spill
        uDeg <- ST.gets $ getDegree u
        uIsFreeze <- ST.gets $ isFreeze u
        when (uDeg >= k && uIsFreeze) $ addToWL u PotSpilled

      -- u, v can be safely coalesced if every neighbor of v is either of insignificant degree,
      -- precolored, or already interferes with u.
      georgeCoalesceHeuristic u v state =
        let okay t = getDegree t state < k || isPrecolored t state || interferes (t, u) state
        in all okay $ adjacent v state

      -- u,v can be safely coalesced if the coalesced node uv has less than k neighbors with
      -- significant degree. Why? After simplification, uv will have only those neighbors of
      -- insignificant degree, which will allow it to be simplified. Once uv has been simplified,
      -- the new graph is equivalent to the simplified old graph.
      briggsCoalesceHeuristic u v state =
        let nodes = adjacent u state `Set.union` adjacent v state
            notOkay t = getDegree t state >= k
        in (Set.size $ Set.filter notOkay nodes) < k

  in doTo (_worklistMoves . _moveSets) (mapM_ coalesceMove)

freeze :: Int -> ST.State RAState ()
freeze k = do
  u <- popWL Freeze
  addToWL u Simplify
  freezeMoves k u

-- Add every move related to u to the frozen worklist. For every temp that is now
-- no longer move related and of insignificant degree, add it to the simplify worklist.
freezeMoves :: Int -> S.Temp -> ST.State RAState ()
freezeMoves k u =
  let freezeMove m@(x', y') = do
        state <- ST.get
        let yAlias = getAlias y' state
            -- v == u only if the move is u <- u
            -- if the move is u <- a or a <- u, v = a, for some a /= u.
            v = if yAlias == getAlias u state then getAlias x' state else yAlias
        addToML m Frozen
        -- Now that we've removed a move from v, it's possible that it's no longer move related.
        -- if it' snot, and if it's of insignificant degree, add it to the simplify list.
        (vMoves, vDeg) <- ST.gets $ \st -> (nodeMoves v st, getDegree v st)
        when (Set.null vMoves && vDeg < k) $ addToWL v Simplify

  in ST.gets (nodeMoves u) >>= mapM_ freezeMove

selectSpill :: Int -> ST.State RAState ()
selectSpill k = do
  -- TO DO : make sure m doesn't have a tiny live range
  m <- popWL PotSpilled
  addToWL m Simplify
  freezeMoves k m

assignColors :: Int -> Set.Set Color -> ST.State RAState ()
assignColors k reserved =
  let loop = do
        selectStackEmpty <- ST.gets $ Set.null . _selectedStack . _workLists
        when (not selectStackEmpty) assignNode

      assignNode = do
        n <- popWL Select
        okColors <- ST.gets $ okayColors n
        if Set.null okColors
          then addToWL n ActSpilled
          else addToWL n Colored >> colorTemp n okColors
        loop

      okayColors u state = Set.fromList [0..k-1]
                           `Set.difference` neighborColors u state
                           `Set.difference` reserved

      neighborColors n state =
        let neighbors = allAdjacent n state
            theirColors = Set.map (\t -> fromMaybe (-1) . colorOf (getAlias t state) $ state) neighbors
        in theirColors

      colorTemp n colors = color %= M.insert n (Set.findMin colors)

      colorCoalescedNodes = do

        aliases <- ST.gets _alias
        colors <- ST.gets _color
        lg (show aliases)
        lg (show colors)

        allCoalesced <- ST.gets $ _coalescedNodes . _workLists
        lg (show allCoalesced)
        forM_ allCoalesced $ \t -> do
          t' <- ST.gets $ getAlias t
          Just tCol <- ST.gets $ colorOf t'
          color %= M.insert t tCol

  in loop >> colorCoalescedNodes

rewriteProgram :: Set.Set S.Temp -> [Instr] ->  ST.State S.SymbolTable [Instr]
rewriteProgram spilled instrs = return (trace "should be rewriting program!" instrs)


removeCoalescedMoves colors instrs =
  let go instrs acc =
        case instrs of
          [] -> reverse acc
          (i@(A.Oper (A.MOVE t1 t2) _ _ _) : rest) -> if (M.lookup t1 colors) == (M.lookup t2 colors)
                                                      then go rest acc
                                                      else go rest (i : acc)
          (i : rest) -> go rest (i : acc)
  in go instrs []
