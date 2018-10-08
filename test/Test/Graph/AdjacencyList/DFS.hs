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
            , testdfs2
            ]


-- | DAG
graphTest1 = 
  let vs = [1..8]
      neis = (\v -> let nei 1 = [2,5,6]
                        nei 2 = [3,5]
                        nei 3 = [4,6]
                        nei 4 = []
                        nei 5 = [4,7]
                        nei 6 = [8,7]
                        nei 7 = [4]
                        nei 8 = [7]
                     in nei v
             )
   in createGraph vs neis

testdfs1 :: Test
testdfs1 = do
  let name = "Test DFS on TestGraph1"
      out = dfs graphTest1 1
      expe = [1,2,3,5,4,6,8,7]
   in case  topsort out == expe of
        True -> testPassed name $ "passed!" <> (show out)
        False -> testFailed name $ (,) (show expe) (show out)

testlongest1 :: Test
testlongest1 = do
  let name = "Test DFS on TestGraph1"
      out = map toTuple $ longestPath graphTest1 1 7
      expe = [(1,2),(2,3),(3,6),(6,8),(8,7)]
   in case  out == expe of
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

--test2 :: Test
--test2 = do
  --let name = "DFS in undirected grid 4^2"
      --l    = (4 :: L)
      --d    = (2 :: D)
      --lat  = graphCubicPBC (PBCSquareLattice  l d)
      --latdfs = dfs lat 7
      --out = postordering latdfs
      --expe = out
   --in case expe == out of
        --True -> testPassed name $ "passed!" <> show latdfs
        --False -> testFailed name $ (,) (show expe) (show out)
