{-# LANGUAGE BangPatterns #-}  

module Test.Graph.AdjacencyList.Grid where

import Data.Bifunctor
import Data.List
import Data.List.Unique
import Data.Maybe
import qualified Data.Map.Lazy as M

import TestHS

import Data.Graph.AdjacencyList
import Data.Graph.AdjacencyList.Grid

fastTests :: [Test]
fastTests = [ test2dpbc1
            , test2dpbc2
            , test3dpbc1
            , test3dpbc2
            , test4dpbc1
            , testforwards
            , vertexToCVertexToVertex
            , testEdgeUndir
            , testEdgeDir
            ]

test2dpbc1 :: Test
test2dpbc1 = do
  let name = "Neighbors of 1 in a square"
      neigh1 = [2,3]
      out = sortUniq $ neighbors (graphCubicPBC (PBCSquareLattice (2 :: L)  (2 :: D))) (1 :: Vertex)
  case out == neigh1 of
    True -> testPassed name "passed!"
    False -> testFailed name $ (,) (show neigh1) (show out)

test2dpbc2 :: Test
test2dpbc2 = do
  let name = "Neighbors of 1 in L=4 D=2"
      neigh1 = [2,4,5,13]
      out = sortUniq $ neighbors (undirectedGraphCubicPBC (PBCSquareLattice (4 :: L)  (2 :: D))) (1 :: Vertex)
  case out == neigh1 of
    True -> testPassed name "passed!"
    False -> testFailed name $ (,) (show neigh1) (show out)

test3dpbc1 :: Test
test3dpbc1 = do
  let name = "Neighbors of 1 in L=2 D=3"
      neigh1 = [2,3,5]
      out = sortUniq $ neighbors (graphCubicPBC (PBCSquareLattice (2 :: L)  (3 :: D))) (1 :: Vertex)
  case out == neigh1 of
    True -> testPassed name "passed!"
    False -> testFailed name $ (,) (show neigh1) (show out)


test3dpbc2 :: Test
test3dpbc2 = do
  let name = "Neighbors of 1 in L=4 D=3"
      neigh1 = [2,4, 5,13, 17,49]
      out = sortUniq $ neighbors (undirectedGraphCubicPBC (PBCSquareLattice (4 :: L)  (3 :: D))) (1 :: Vertex)
  case out == neigh1 of
    True -> testPassed name "passed!"
    False -> testFailed name $ (,) (show neigh1) (show out)

test4dpbc1 :: Test
test4dpbc1 = do
  let name = "Neighbors of 1 in L=2 D=4"
      neigh1 = [2,3,5,9]
      out = sortUniq $ neighbors (graphCubicPBC (PBCSquareLattice (2 :: L)  (4 :: D))) (1 :: Vertex)
  case out == neigh1 of
    True -> testPassed name "passed!"
    False -> testFailed name $ (bimap <$> id <*> id) show (neigh1, out)

testforwards :: Test
testforwards = do
  let name = "Edges of pbcsql L=3 D=2"
      lat  = graphCubicPBC (PBCSquareLattice  (3 :: L) (2 :: D))
      expe = [(1,2),(1,4),(2,3),(2,5),(3,1),(3,6),(4,5),(4,7),(5,6),(5,8),(6,4),(6,9),(7,8),(7,1),(8,9),(8,2),(9,7),(9,3)]
      out =  map toTuple $ edges lat
  case all id (map (\e -> elem e expe) out) of
    True -> testPassed name "passed!"
    False -> testFailed name $ (bimap <$> id <*> id) show (expe, out)

vertexToCVertexToVertex :: Test
vertexToCVertexToVertex = do
  let name = "Turn vertex to cartesian vertex and back for PBCSquare lattice"
      l    = (3 :: L)
      d    = (3 :: D)
      lat  = undirectedGraphCubicPBC (PBCSquareLattice  l d)
      vs = vertices lat
      cvs = map (vertexToCVertex l d) vs 
      vs' = map (cVertexToVertex l d) cvs
  case vs == vs' of
    True -> testPassed name $ "passed!"
    False -> testFailed name $ (bimap <$> id <*> id) (show . take 10) (vs, vs')

testEdgeUndir :: Test
testEdgeUndir = do
  let name = "grid undirected Edges to ids"
      l    = (10 :: L)
      d    = (2 :: D)
      lattice = undirectedGraphCubicPBC (PBCSquareLattice l d)
      es = edges lattice
      eids = M.fromList $ zip es $ map (\e -> fromJust (edgeIndex lattice e)) es
      expe = edgeMap lattice
  case eids == expe of
    True -> testPassed name "passed!"
    False -> testFailed name $ (,) (show eids) (show es)

testEdgeDir :: Test
testEdgeDir = do
  let name = "grid directed Edges to ids"
      l    = (40 :: L)
      d    = (3 :: D)
      lattice = graphCubicPBC (PBCSquareLattice l d)
      es = edges lattice
      eids = M.fromList $ zip es $ map (\e -> fromJust (pbcEdgeIx l d e)) es
      expe = edgeMap lattice
  case eids == expe of
    True -> testPassed name "passed!"
    False -> testFailed name $ (,) (show eids) (show es)
