module TestLiveness(tests) where

import Test.HUnit

import qualified CodeGen.Assem as A
import qualified Symbol as S
import qualified Allocation.Liveness as L
import qualified Allocation.Core as AC
import qualified Control.Monad.State.Strict as ST
import qualified Allocation.DirectedGraph as Graph
import qualified Data.Set as Set
import qualified Data.Map as M
import Data.Maybe(fromJust)
import Control.Monad(when)
import Debug.Trace(trace)

build instrs = do
  let flowGraph = L.flowGraph instrs
      livenessMap = L.liveness flowGraph
  AC.build M.empty flowGraph livenessMap
  igraph <- ST.gets AC._iGraph
  return (igraph, livenessMap)

color k instrs = ST.evalState (AC.allocateRegisters k M.empty Set.empty instrs) S.empty


{-- This is an example based on Appel p230 (graph 11.1)
  It differs because Appel assumes k,j are live going into the block
and d,k,j are live coming out of it, but my liveness analyzer doesn't.
So, the interference graph doesn't quite match up with what's in the book.
--}
testIG1 = do
  let (k':j':g':h':f':e':m':b':c':d':_) = ST.evalState (mapM (\ _ -> S.genTemp) [0..50]) S.empty
      temps@[k,j,g,h,f,e,m,b,c,d] = [ (k', "k"), (j', "j"), (g', "g"), (h', "h"), (f', "f")
                                    , (e', "e"), (m', "m"), (b', "b"), (c', "c"), (d', "d") ]
      s = Set.fromList
      -- live-in[node] = use[n] U (live-out[node] - def[n])
      -- live-out[node] = U live-in[s], s in succ[n]
      -- instrs = [(instr, (live-in, live-out))]
      instrs = [ (A.Oper (A.LW g' j' 12) [j'] [g'] Nothing, (s [j',k'], s [j', k', g']))
               , (A.Oper (A.ADDI h' k' 1) [k'] [h'] Nothing, (s [j',k',g'], s [h',j',g']))
               , (A.Oper (A.MUL f' g' h') [g', h'] [f'] Nothing, (s [h',j',g'], s [f', j']))
               , (A.Oper (A.LW e' j' 8) [j'] [e'] Nothing, (s [f', j'], s [e', j', f']))
               , (A.Oper (A.LW m' j' 16) [j'] [m'] Nothing, (s [e',j',f'], s [m',f',e']))
               , (A.Oper (A.LW b' f' 0) [f'] [b'] Nothing, (s [m',f',e'], s [b',m',e']))
               , (A.Oper (A.ADDI c' e' 8) [e'] [c'] Nothing, (s [m',e',b'], s [c',m',b']))
               , (A.Oper (A.MOVE d' c') [c'] [d'] Nothing, (s [c',m',b'], s [m',b']))
               , (A.Oper (A.ADDI k' m' 4) [m'] [k'] Nothing, (s [m',b'], s [b']))
               , (A.Oper (A.MOVE j' b') [b'] [j'] Nothing, (s [b'], s []))
               ]
      (igraph, livenessMap) = ST.evalState (build (map fst instrs)) AC.emptyRAState
      colors = color 4 $ map fst instrs


      interferes (t1, n1) does doesnt = do
        let Just leftColor = M.lookup t1 colors
        mapM_ (\(t2, n2) -> assertBool ("error: expected " ++ n1 ++  " to interfere w/ " ++ n2)
                            (Graph.isEdge t1 t2 igraph))
          does
        mapM_ (\(t2, n2) -> assertBool ("error: expected " ++ n1 ++  " to not interfere w/ " ++ n2)
                            (not (Graph.isEdge t1 t2 igraph)))
          doesnt

        mapM_ (\ (t2, n2) -> let Just rightColor = M.lookup t2 colors
                             in assertBool ("error: both " ++ n1 ++ " and " ++ n2 ++ " have color: "
                                            ++ (show leftColor))
                                (rightColor /= leftColor))
          does

      haveBeenCoalesced (t1, n1) (t2, n2) = do
        let Just col1 = M.lookup t1 colors
            Just col2 = M.lookup t2 colors
        assertBool ("error: expected " ++ n1 ++ " and " ++ n2 ++ " to have been coalesced.")
          (col1 == col2)

  assertBool "You got your live-ins and live-outs wrong!" $ (map snd instrs) == (M.elems livenessMap)

  {-- print the colors map
  when (trace (show . map (\(t, s) ->
                            (fromJust $ M.lookup t colors, s, t)) $ temps) True) $ return ()
  --}

  haveBeenCoalesced j b
  haveBeenCoalesced c d
  interferes j [f, e, h, g]       $ [k, b, c, m, d]
  interferes h [j, g]             $ [k, d, c, b, m, e, f]
  interferes g [h, k, j]          $ [d, c, b, m, e, f]
  interferes k [g, b]             $ [d, j, h, f, e, m, c]
  interferes d [b, m]             $ [k, j, h, g, f, e, c]
  interferes c [b, m]             $ [j, h, g, k, d, f, e]
  interferes b [k, d, c, e, m]    $ [j, h, g, f]
  interferes m [f, e, b, c, d]    $ [j, h, g, k]
  interferes f [j, e, m]          $ [k, h, g, d, c, b]
  interferes e [f, j, b, m]       $ [h, g, k, d, c]

tests = test [testIG1]
