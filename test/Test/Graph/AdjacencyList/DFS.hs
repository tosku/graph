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
fastTests = [ test1
            --, test2
            ]


-- | DAG
graphTest1 = 
  let vs = [1..7]
      neis = (\v -> let nei 1 = [2,5,6]
                        nei 2 = [3,5]
                        nei 3 = [4,6]
                        nei 4 = []
                        nei 5 = [4,7]
                        nei 6 = [7]
                        nei 7 = [4]
                     in nei v
             )
   in createGraph vs neis

test1 :: Test
test1 = do
  let name = "Test DFS on TestGraph1"
      out = dfs graphTest1 1
      expe = [1,2,3,4,6,7,5]
   in case  topsort out == expe of
        True -> testPassed name $ "passed!"
        False -> testFailed name $ (,) (show expe) (show out)

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
