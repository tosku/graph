module Test.Graph.AdjacencyList.WFI where

import Data.Maybe
import Data.List
import Data.List.Unique
import TestHS

import qualified Data.IntMap.Strict as IM
import Data.Maybe

import Data.Graph.AdjacencyList
import Data.Graph.AdjacencyList.WFI
import Data.Graph.AdjacencyList.Grid


fastTests :: [Test]
fastTests = [ testWFI1
            , testWFI2
            ]

-- | DAG
graphTest1 = 
  let vs = [1..8]
      neis = (\v -> let nei 1 = [2,5,6]
                        nei 2 = [3,5]
                        nei 3 = [4,6]
                        nei 4 = [7]
                        nei 5 = [4,7]
                        nei 6 = [8,7]
                        nei 7 = []
                        nei 8 = [7]
                     in nei v
             )
   in createGraph vs neis

testWFI1 :: Test
testWFI1 = do
  let name = "Test Shortest paths Floyd-Warshall algorithm on a directed graph"
      out :: [(Vertex,Rational)]
      out = IM.toList $ fromJust $ IM.lookup 1 $ unweightedShortestDistances graphTest1
      expe = [(1,0),(2,1),(3,2),(4,2),(5,1),(6,1),(7,2),(8,2)]
   in case out == expe of
        True -> testPassed name $ "passed!" <> (show out)
        False -> testFailed name $ (,) (show expe) (show out)

testWFI2 :: Test
testWFI2 = do
  let name = "Test Shortest paths Floyd-Warshall algorithm undirected graph"
      out :: [(Vertex,Rational)]
      out = IM.toList $ fromJust $ IM.lookup 1 $ unweightedShortestDistances $ makeUndirected graphTest1
      expe = [(1,0),(2,1),(3,2),(4,2),(5,1),(6,1),(7,2),(8,2)]
   in case out == expe of
        True -> testPassed name $ "passed!" <> (show out)
        False -> testFailed name $ (,) (show expe) (show out)
