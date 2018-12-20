module Test.Graph.AdjacencyList.Metrics where

import Data.Maybe
import Data.List
import Data.List.Unique
import TestHS

import qualified Data.IntMap.Strict as IM
import Data.Maybe

import Data.Graph.AdjacencyList
import Data.Graph.AdjacencyList.Grid

import Data.Graph.AdjacencyList.WFI
import Data.Graph.AdjacencyList.Metrics

fastTests :: [Test]
fastTests = [ testEccentricity
            , testRadius
            , testDiameter
            , testDensity
            ]

-- | DAG
graphTest = 
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

testEccentricity :: Test
testEccentricity = do
  let name = "Eccentricity of vertex 2 in test graph"
      dists = unweightedShortestDistances graphTest
      out = graphEccentricity 2 dists
      expe = Just 3
   in case out == expe of
        True -> testPassed name $ "passed! " <> (show out)
        False -> testFailed name $ (,) (show dists) (show out)

testRadius :: Test
testRadius = do
  let name = "Radius of test graph should be 1 (8-7)"
      dists = unweightedShortestDistances graphTest
      out = graphRadius dists
      expe = Just 1
   in case out == expe of
        True -> testPassed name $ "passed! "
        False -> testFailed name $ (,) (show expe) (show out)

testDiameter :: Test
testDiameter = do
  let name = "Diameter 3 (2-7)"
      dists = unweightedShortestDistances graphTestDisco
      out = graphDiameter dists
      expe = Just 3
   in case out == expe of
        True -> testPassed name $ "passed! "
        False -> testFailed name $ (,) (show expe) (show out)

testDensity :: Test
testDensity = do
  let name = "Density of testgraph should be 13/56"
      out = graphDensity graphTest
      expe = 13 / 56
   in case out == expe of
        True -> testPassed name $ "passed! "
        False -> testFailed name $ (,) (show expe) (show out)
