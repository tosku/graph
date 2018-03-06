module Test.Graph.AdjacencyList.PushRelabel.Pure where


import Data.Maybe
import Data.List
import Data.List.Unique
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM

import qualified Data.Graph.Inductive as I
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.MaxFlow as MF
import qualified Data.Graph.Inductive.Query.BFS as IBFS

import TestHS

import Data.Graph.AdjacencyList
import Data.Graph.AdjacencyList.Grid
import Data.Graph.AdjacencyList.Network
import Data.Graph.AdjacencyList.PushRelabel.Pure

fastTests :: [Test]
fastTests = [ 
              test1
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
  
fg = Network { graph = graphTest1
             , source = 1
             , sink = 7
             , capacities = M.fromList $ zip (edges (graph fg)) (map toRational $ repeat 1.2)
             , flow = M.empty
             }

test1 :: Test
test1 = do
  let name = "pushRelabel with FGL's MaxFlow"
  let eout = pushRelabel fg
  let vs = map (\v -> (v,())) $ vertices (graph fg) :: [G.UNode]
  let es = map (\(f,t) -> (f,t,1.2)) $ (map toTuple (edges (graph fg))) :: [G.LEdge Double]
  let mfg = G.mkGraph vs es :: I.Gr () Double
  let expe = MF.maxFlow mfg 1 7 :: Double
  case eout of
    Left err -> testFailed name ("push relabel error", err)
    Right out -> do
        let netout = netFlow out
        let fglout = toRational expe
        case netout == fglout of
          True -> testPassed name $ "passed!" ++ (show expe)
          False -> testFailed name $ (,) (show fglout) (show netout)
