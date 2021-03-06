module Test.Graph.AdjacencyList.WFI where

import Data.Maybe
import Data.List
import Data.List.Unique
import TestHS

import qualified Data.IntMap.Strict as IM
import Data.Maybe

import Data.Graph.AdjacencyList
import Data.Graph.AdjacencyList.Grid

import Data.Graph.AdjacencyList.WFI

fastTests :: [Test]
fastTests = [ testWFI1
            , testWFI2
            , testDisconnected
            ]

-- | DAG
graphTestWFI = 
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

graphTestDisco =
  let vs = [1..10]
      neis = (\v -> let nei 1 = [2,5,6]
                        nei 2 = [3,5]
                        nei 3 = [4,6]
                        nei 4 = [7]
                        nei 5 = [4,7]
                        nei 6 = [8,7]
                        nei 7 = []
                        nei 8 = [7]
                        nei 9 = [10]
                        nei 10 = []
                     in nei v
             )
   in createGraph vs neis

testWFI1 :: Test
testWFI1 = do
  let name = "Test Shortest paths Floyd-Warshall algorithm on a directed graph"
      (Distances dists) = unweightedShortestDistances graphTestWFI
      out :: [(Vertex,Rational)]
      out = IM.toList $ fromJust $ IM.lookup 1 $ dists
      expe = [(1,0),(2,1),(3,2),(4,2),(5,1),(6,1),(7,2),(8,2)]
   in case out == expe of
        True -> testPassed name $ "passed!"
        False -> testFailed name $ (,) (show expe) (show out)

testWFI2 :: Test
testWFI2 = do
  let name = "Test Shortest paths Floyd-Warshall algorithm undirected graph"
      (Distances dists) = unweightedShortestDistances $ makeUndirected graphTestWFI
      out :: [(Vertex,Rational)]
      out = IM.toList $ fromJust $ IM.lookup 1 $ dists
      expe = [(1,0),(2,1),(3,2),(4,2),(5,1),(6,1),(7,2),(8,2)]
   in case out == expe of
        True -> testPassed name $ "passed!" 
        False -> testFailed name $ (,) (show expe) (show out)

testDisconnected :: Test
testDisconnected = do
  let name = "Test Shortest paths Floyd-Warshall algorithm on disconnected graph"
      (Distances dists) = unweightedShortestDistances graphTestDisco
      out :: [(Vertex,Rational)]
      out = IM.toList $ fromJust $ IM.lookup 1 $ dists
      expe = [(1,0),(2,1),(3,2),(4,2),(5,1),(6,1),(7,2),(8,2)]
   in case out == expe of
        True -> testPassed name $ "passed!" <> (show dists)
        False -> testFailed name $ (,) (show expe) (show out)
