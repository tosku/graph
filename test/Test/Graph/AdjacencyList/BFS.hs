module Test.Graph.AdjacencyList.BFS where

import Data.Maybe
import Data.List
import Data.List.Unique
import TestHS

import qualified Data.IntMap.Strict as IM
import Data.Maybe

import qualified Data.Graph.Inductive as I
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.BFS as IBFS

import Data.Graph.AdjacencyList
import Data.Graph.AdjacencyList.BFS
import Data.Graph.AdjacencyList.Grid

fastTests :: [Test]
fastTests = [ test1
            , test2
            , spanningtreetest
            , spanningtreeUndirected
            ]


graphTest1 = 
  let vs = [1..7]
      neis = (\v -> let nei 1 = [2,5,6]
                        nei 2 = [5,3]
                        nei 3 = [4]
                        nei 4 = []
                        nei 5 = [4,7]
                        nei 6 = [7]
                        nei 7 = [4]
                     in nei v
             )
   in createGraph vs neis

test1 :: Test
test1 = do
  let name = "Test bfs on TestGraph1"
      out = level $ bfs graphTest1 1
      expe = IM.fromList [(1,0),(2,1),(5,1),(6,1),(3,2),(4,2),(7,2)]
   in case  out == expe of
        True -> testPassed name "passed!"
        False -> testFailed name $ (,) (show expe) (show out)

test2 :: Test
test2 = do
  let name = "BFS in undirected grid tested against fgl library"
      l    = (6 :: L)
      d    = (3 :: D)
      lat  = graphCubicPBC (PBCSquareLattice  l d)
      latbfs = bfs lat 18
      out = sort $ IM.toList (level latbfs)
      vs = map (\v -> (v,())) $ vertices lat :: [G.UNode]
      es = map (\(f,t) -> (f,t,1)) $ (map toTuple (edges lat)) :: [G.LEdge Double]
      ingr = G.mkGraph vs es :: I.Gr () Double
      expe = sort $ IBFS.level 18 ingr
  case expe == out of
    True -> testPassed name $ "passed!"
    False -> testFailed name $ (,) ("\n" ++ show expe) 
      ("\n" ++ show out ++ "\n" ++ show latbfs ++ "\n" ++ show lat)

spanningtreetest :: Test
spanningtreetest = do
  let name = "Get Spanning Tree from BFS"
      out = spanningTree $ bfs graphTest1 1
      expe = map fromTuple [(1,2),(2,3),(5,4),(1,5),(1,6),(5,7)]
   in case  out == expe of
        True -> testPassed name "passed!"
        False -> testFailed name $ (,) (show expe) (show out)

spanningtreeUndirected :: Test
spanningtreeUndirected = do
  let name = "Get Spanning Tree from BFS undirected graph"
      ungr = makeUndirected graphTest1
      out = spanningTree $ bfs ungr 1
      expe = map fromTuple [(1,2),(2,3),(5,4),(1,5),(1,6),(5,7)]
   in case  out == expe of
        True -> testPassed name "passed!"
        False -> testFailed name $ (,) (show expe) (show out)
