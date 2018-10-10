module Test.Graph.AdjacencyList.DFS where

import Data.Maybe
import Data.List
import Data.List.Unique
import TestHS

import qualified Data.IntMap.Strict as IM
import qualified Data.Sequence as Seq
import qualified Data.IntSet as Set
import Data.Maybe

import qualified Data.Graph.Inductive as I
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.DFS as IDFS

import Data.Graph.AdjacencyList
import Data.Graph.AdjacencyList.DFS
import Data.Graph.AdjacencyList.Grid

fastTests :: [Test]
fastTests = [ testdfs1
            , testlongest1
            , testlongest2
            , testlongest3
            , testlongest4
            , testdfs2
            , outofrange
            , getdirect
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

testdfs1 :: Test
testdfs1 = do
  let name = "Test DFS topsort on a graph with hamiltonian path"
      testgraph = graphFromEdges $ (edges graphTest1) ++ [(Edge 3 5),(Edge 5 6),(Edge 8 4)]
      out = dfs testgraph 1
      expe = [1,2,3,5,6,8,4,7]
   in case  topsort out == expe of
        True -> testPassed name $ "passed!" <> (show out)
        False -> testFailed name $ (,) (show expe) (show out)

-- | DAG
graphTest2 = 
  let vs = [1..4]
      neis = (\v -> let nei 1 = [2,3]
                        nei 2 = []
                        nei 3 = [4]
                        nei 4 = [2]
                     in nei v
             )
   in createGraph vs neis

testdfs2 :: Test
testdfs2 = do
  let name = "Test DFS on TestGraph2"
      out = dfs graphTest2 1
      expe = [1,3,4,2]
   in case  topsort out == expe of
        True -> testPassed name $ "passed!" <> (show out)
        False -> testFailed name $ (,) (show expe) (show $ topsort out)

testlongest1 :: Test
testlongest1 = do
  let name = "Test longest path 1 7 on TestGraph1"
      out = map toTuple $ longestPath graphTest1 1 7
      outdfs = dfs graphTest1 1
      expe = [(1,2),(2,3),(3,6),(6,8),(8,7)]
   in case  out == expe of
        True -> testPassed name $ "passed!" <> (show out)
        False -> testFailed name $ (,) (show expe) (show out <> show outdfs)

testlongest2 :: Test
testlongest2 = do
  let name = "Test longest path 1 8 on TestGraph1"
      out = map toTuple $ longestPath graphTest1 1 8
      tdfs = dfs graphTest1 1
      expe = [(1,2),(2,3),(3,6),(6,8)]
   in case  out == expe of
        True -> testPassed name $ "passed!" <> (show out)
        False -> testFailed name $ (,) (show expe) (show out <> show tdfs)

testlongest3 :: Test
testlongest3 = do
  let name = "Test longest path 2 8 on TestGraph2"
      out = map toTuple $ longestPath graphTest1 2 8
      expe = [(2,3),(3,6),(6,8)]
   in case  out == expe of
        True -> testPassed name $ "passed!" <> (show out)
        False -> testFailed name $ (,) (show expe) (show out)

graphTest3 = 
  let edges = 
        map fromTuple 
        [(1,3),(2,1),(2,3),(2,4),(2,5),(2,6),(4,1),(4,3),(4,5),(5,1),(5,3),(6,1),(6,3),(6,4)]
   in graphFromEdges edges

testlongest4 :: Test
testlongest4 = do
  let name = "topsort 2 3 on TestGraph3"
      tdfs = dfs graphTest3 2
      out = postordering tdfs 
      expe = [3,1,5,4,6,2]
      tgr = map (neighbors graphTest3) [1..6]
   in case  out == expe of
        True -> testPassed name $ "passed!"
        False -> testFailed name $ (,) (show expe) (show tdfs <> show tgr)

outofrange :: Test
outofrange = do
  let name = "longest from 3 to 2 on TestGraph3"
      tdfs = dfs graphTest3 3
      out  = longestPath graphTest3 3 2
   in case null out of
        True  -> testPassed name $ "passed!"
        False -> testFailed name $ (,) ("[]") (show out)

getdirect :: Test
getdirect = do
  let name = "longest of direct"
      gr = graphFromEdges $ map fromTuple [(4,5),(1,5)]
      tdfs = dfs gr 1
      out  = longestPath gr 1 5
      expe = map fromTuple [(1,5)]
   in case out == expe of
        True  -> testPassed name $ "passed!"
        False -> testFailed name $ (,) ("[(1,5)]") (show out)
