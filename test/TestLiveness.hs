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

build instrs = do
  let flowGraph = L.flowGraph instrs
      livenessMap = L.liveness flowGraph
  AC.build flowGraph livenessMap
  igraph <- ST.gets AC._iGraph
  return (igraph, livenessMap)


{-- This is an example based on Appel p230 (graph 11.1)
  It differs because Appel assumes k,j are live going into the block
and d,k,j are live coming out of it, but my liveness analyzer doesn't.
So, the interference graph doesn't quite match up with what's in the book.
--}
testIG1 = do
  let (k':j':g':h':f':e':m':b':c':d':_) = ST.evalState (mapM (\ _ -> S.genTemp) [0..50]) S.empty
      [k,j,g,h,f,e,m,b,c,d] = [ (k', "k"), (j', "j"), (g', "g"), (h', "h"), (f', "f")
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

      interferes (t1, n1) does doesnt = do
        mapM_ (\(t2, n2) -> assertBool ("error: expected " ++ n1 ++  " to interfere w/ " ++ n2)
                            (Graph.isEdge t1 t2 igraph))
          does
        mapM_ (\(t2, n2) -> assertBool ("error: expected " ++ n1 ++  " to not interfere w/ " ++ n2)
                            (not (Graph.isEdge t1 t2 igraph)))
          doesnt

  assertBool "You got your live-ins and live-outs wrong!" $ (map snd instrs) == (M.elems livenessMap)

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
