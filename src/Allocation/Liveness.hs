module Allocation.Liveness(flowGraph, liveness, FlowGraph(..), LivenessMap) where
import Util
import qualified Allocation.DirectedGraph as Graph
import qualified Control.Monad.State.Strict as ST
import qualified Symbol as S
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified CodeGen.Assem as A
import Data.Maybe(fromMaybe)
import Control.Monad(when)

type Instr = A.Instr S.Temp S.Label
type InstrNode = Graph.Node Instr
type InstrGraph = Graph.DGraph InstrNode

{-- Building a Flow Graph

--}
data FlowGraph = FlowGraph { fgControl :: InstrGraph -- control flow graph
                           , fgDef :: M.Map InstrNode [S.Temp] -- node -> [defined temps] (dest)
                           , gfUse :: M.Map InstrNode [S.Temp] -- node -> [used temps] (src)
                           } deriving (Show)

createLabelTable :: [Instr] -> ST.State InstrGraph (M.Map S.Label InstrNode)
createLabelTable instrs =
  go instrs M.empty
  where go instrs labelMap =
          case instrs of
            [] -> return labelMap
            (instr@(A.Label label) : rest) -> do
              node <- Graph.newNode instr
              go rest (M.insert label node labelMap)
            (_ : rest) -> go rest labelMap

buildFlowGraph instrs getNodeWithLabel =
  let getNode instr = case instr of
                        A.Label label -> return $ getNodeWithLabel label
                        _ -> Graph.newNode instr

      build instrs def use lastNode =
        case instrs of
            [] -> return (def, use)
            (instr : rest) -> do
              node <- getNode instr
              let def' = M.insert node (A.destRegs instr) def
                  use' = M.insert node (A.sourceRegs instr) use
              whenJust lastNode (\last -> when (A.canFallThrough $ Graph.content last)
                                          $ Graph.newEdge last node)
              mapM_ (Graph.newEdge node . getNodeWithLabel) (A.jumpsTo instr)
              build rest def' use' (Just node)

  in build instrs M.empty M.empty Nothing

flowGraph :: [Instr] -> FlowGraph
flowGraph instrs =
  let go = do
        labelTable <- createLabelTable instrs
        (def, use) <- buildFlowGraph instrs (\label -> let Just l = M.lookup label labelTable in l)
        return (def, use)

      ((def, use), graph) = ST.runState go Graph.empty

  in FlowGraph graph def use

{-- Building the Interference Graph

--}

type LivenessMap = M.Map InstrNode (Set.Set S.Temp, Set.Set S.Temp) -- node -> (liveIn, liveOut)


liveness :: FlowGraph -> LivenessMap
liveness (FlowGraph control def use) =
  let liveOutAtNode node = do
        let defs = Set.fromList . fromMaybe [] $ M.lookup node def
            uses = Set.fromList . fromMaybe [] $ M.lookup node use
            successors = Graph.successors node control

            -- live-in[node] = use[n] U (live-out[node] - def[n])
            -- live-out[node] = U live-in[s], s in succ[n]
            loop (liveIn, liveOut) = do
              let liveIn' = uses `Set.union` (liveOut `Set.difference` defs)
              ST.modify $ M.insert node (liveIn', liveOut)
              buildLivenessMap successors
              succLiveIns <- mapM (ST.gets . (\n -> fst . fromMaybe (Set.empty, Set.empty)
                                                    . M.lookup n)) successors
              let liveOut' = Set.unions succLiveIns
              ST.modify $ M.insert node (liveIn', liveOut')

              if liveIn == liveIn' && liveOut == liveOut'
              then return (liveIn, liveOut)
              else loop (liveIn', liveOut')

        --init <- ST.gets $ fromMaybe (Set.empty, Set.empty) . M.lookup node
        cache <- ST.gets $ M.lookup node
        case cache of
          Just liveness -> return liveness
          Nothing -> loop (Set.empty, Set.empty)

      buildLivenessMap = mapM liveOutAtNode

  in ST.execState (buildLivenessMap (Graph.nodes control)) M.empty
