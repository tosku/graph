module Test.Graph.AdjacencyList where

import Data.Bifunctor
import Data.List
import Data.List.Unique
import qualified Data.Binary as Bin

import TestHS

import Data.Graph.AdjacencyList
import Data.Graph.AdjacencyList.Grid

fastTests :: [Test]
fastTests = [ test1
            , testRemoveReverseEdges
            ]

edgesTest1 = map fromTuple 
    [(1,2),(1,5),(1,6)
    ,(2,5),(2,3)
    ,(3,4)
    ,(5,4),(5,7)
    ,(6,7)
    ,(7,4)
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
  let name = "Graph from edges"
      gr1 = graphFromEdges edgesTest1
  case gr1 == graphTest1 of
    True -> testPassed name "passed!"
    False -> testFailed name $ (,) (show graphTest1) (show gr1)

testRemoveReverseEdges :: Test
testRemoveReverseEdges = do
  let name = "Remove reverse edges from komplete 5 graph"
      k5 = completeGraph 5
      dk5 = removeReverseEdges k5
      expected = [ (Edge 1 2)
                 , (Edge 1 3)
                 , (Edge 1 4)
                 , (Edge 1 5)
                 , (Edge 2 3)
                 , (Edge 2 4)
                 , (Edge 2 5)
                 , (Edge 3 4)
                 , (Edge 3 5)
                 , (Edge 4 5)
                 ]
  if edges dk5 == expected
    then 
      testPassed name "passed!"
    else 
      testFailed name $ (,) (show expected) (show dk5)
