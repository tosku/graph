module Test.Graph.AdjacencyList where

import Data.Bifunctor
import Data.List
import Data.List.Unique

import TestHS

import Data.Graph.AdjacencyList
import Data.Graph.AdjacencyList.Grid

fastTests :: [Test]
fastTests = [ test1
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
