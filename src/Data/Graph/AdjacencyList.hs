{-|
Module      : Data.AdjacencyList.Graph
Description : Class definitions

Copyright   : Thodoris Papakonstantinou, 2016
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX

Graph TypeClass definitions
 -}

{-# LANGUAGE BangPatterns #-}  

module Data.Graph.AdjacencyList
    ( Natural
    , Vertex (..)
    , Edge (..)
    , Neighbors (..)
    , EdgeMap (..)
    , Graph (..)
    , fromTuple
    , toTuple
    -- * Graph constructor
    , createGraph
    -- * Graph constructor from edge list
    , graphFromEdges
    , edges
    , reverseEdge
    , reverseEdges
    , reverseGraph
    -- * Filter Graph's vertices
    , filterVertices
    -- * Filter Graph's edges
    , filterEdges
    -- * creates reverse edges making a directed
    -- graph undirected
    , makeUndirected
    , adjacentEdges
    , edgesFromNeighbors
    , adjacencyMap
    , edgeIndex
    , from
    , to
    , numVertices
    , numEdges
    ) where

import Data.List
import Data.List.Unique
import Data.Maybe
import Data.Natural
import qualified Data.Map.Lazy as M
import qualified Data.IntMap.Lazy as IM
import qualified Data.Set as Set

type Vertex = Int

data Edge = Edge Vertex Vertex 
  deriving (Ord)

instance Show Edge where
 show (Edge s t) = "[" ++ show s ++ "->" ++ show t ++ "]"

instance Eq Edge where
  a == b = from a == from b && to a == to b

type EdgeMap = M.Map Edge Int

-- | Takes vertex and outputs neighboring vertices
type Neighbors = (Vertex -> [Vertex])

-- | Graph definition of directed Graphs 
-- undirected graphs should include reverse edges
data Graph = Graph { vertices :: [Vertex]
  -- | The edge map is necessary for appointing edge attributes
                   , edgeMap :: EdgeMap
                   , neighbors :: Neighbors
                   }

-- | gives the position of the edge to the edges list
edgeIndex :: Graph -> Edge -> Maybe Int
edgeIndex g e = M.lookup e $ edgeMap g

edges :: Graph -> [Edge]
edges g = 
  fmap fst $ M.toList $ edgeMap g

edgeMapFromEdges :: [Edge] -> EdgeMap
edgeMapFromEdges es =
  M.fromList $ zip es [1..]

from :: Edge -> Vertex
from (Edge s t) = s

to :: Edge -> Vertex
to (Edge s t) = t

fromTuple :: (Vertex, Vertex) -> Edge
fromTuple (s,t) = Edge s t

toTuple :: Edge -> (Vertex, Vertex)
toTuple (Edge s t) = (s,t)

reverseEdge :: Edge -> Edge
reverseEdge (Edge s t) = Edge t s

reverseEdges :: Graph -> [Edge]
reverseEdges g = fmap reverseEdge $ edges g

numVertices :: Graph -> Int
numVertices g = length $ vertices g
numEdges :: Graph -> Int
numEdges g = length $ edges g


instance Eq Graph where
  (==) g1 g2 = (sort (vertices g1) == sort (vertices g2))
               && (sort (edges g1) == sort (edges g2))

instance Show Graph where
  show g = "vertices: " ++ show (vertices g) ++ "\n" ++
            "edges: " ++ show (edges g) ++ "\n"

-- | The canonical graph constructor
createGraph :: [Vertex] -> Neighbors -> Graph
createGraph vs neis =
  let emap = edgeMapFromEdges $ edgesFromNeighbors neis vs
   in Graph { vertices = vs
            , neighbors = neis
            , edgeMap = emap
            }

graphFromEdges :: [Edge] -> Graph
graphFromEdges es = 
  let vs = Set.toList $ foldl' (\ac (Edge u v) ->
             Set.insert u (Set.insert v ac)) Set.empty es
      esmap = edgeMapFromEdges es
      neimap = IM.fromList 
                  $ fmap 
                    (\v -> 
                      let nes = fmap to 
                                $ M.keys 
                                  $ M.filterWithKey 
                                    (\e _ -> from e == v) 
                                    esmap
                       in (v, nes))
                    vs
      neis = (\v -> 
                 let mns = IM.lookup v neimap
                  in case mns of
                       Nothing -> []
                       Just ns -> ns)
   in Graph { vertices = vs
            , edgeMap = esmap
            , neighbors = neis
            }

edgesFromNeighbors :: Neighbors -> [Vertex] -> [Edge]
edgesFromNeighbors neis vs = 
  let allneis = fmap (\v -> (v,neis v)) vs
   in foldr (\(v,nv) ac -> 
             (fmap (\n -> Edge v n) nv) ++ ac
             ) [] allneis

adjacentEdges :: Graph -> Vertex -> [Edge]
adjacentEdges g v = fmap (\n -> Edge v n) $ neighbors g v

adjacencyMap :: Graph -> IM.IntMap [Vertex]
adjacencyMap g = IM.fromList $ fmap (\v -> (v, (neighbors g v))) vs
                 where vs = vertices g

reverseGraph :: Graph -> Graph
reverseGraph g =
  graphFromEdges $ reverseEdges g

filterVertices :: (Vertex -> Bool) -> Graph -> Graph
filterVertices f g =
  let oldvs = vertices g
      vs = filter f oldvs 
      neis v = 
        let ns = neighbors g v
         in filter f ns
   in createGraph vs neis

filterEdges :: (Edge -> Bool) -> Graph -> Graph
filterEdges f g =
  let vs = vertices g
      neis v = 
        let neis = neighbors g v
         in filter (\n -> f (Edge v n)) neis
   in createGraph vs neis

makeUndirected :: Graph -> Graph
makeUndirected g =
  let rg = reverseGraph g
      vs = vertices g
      newnei v = 
        let nei = neighbors g v
            rnei = neighbors rg v
         in sortUniq $ nei ++ rnei
   in createGraph vs newnei


