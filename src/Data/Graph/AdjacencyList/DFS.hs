{-|
Module      : DFS
Description : Depth first search graph traversal
Copyright   : Thodoris Papakonstantinou, 2018
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX


 -}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}


module Data.Graph.AdjacencyList.DFS
  ( DFS (..)
  , dfs
  -- * get longest path from a vertex to another
  , longestPath
  , postordering
  , areConnected
  , distances
  ) where

import Data.List
import Data.Maybe
import qualified Data.IntMap   as IM
import qualified Data.IntSet   as Set
import qualified Data.Sequence as Seq

import Data.Graph.AdjacencyList

data DFS = DFS { topsort :: [Vertex]
               , visited :: [Vertex]
               , discovered   :: Set.IntSet
               , called :: Int
               } deriving (Eq, Show)

initialDFS :: DFS
initialDFS = DFS { topsort = []
                 , discovered   = Set.empty
                 , visited = []
                 , called = 0
                 }

-- | Depth first search
dfs :: Graph -> Vertex -> DFS
dfs g s = 
  if not $ elem s (vertices g) 
     then initialDFS
     else
       let sbfs = initialDFS
           depthFirstSearch :: Vertex -> DFS -> DFS
           depthFirstSearch v ac =
              let ns = neighbors g v
                  !ac' = foldl' (\ac'' n -> if not (Set.member n (discovered ac''))
                                              then depthFirstSearch n ac''
                                              else ac''
                                ) ac ns
                  newpostord = v: (topsort ac')
                  res = ac' { discovered = Set.insert v (discovered ac')
                            , topsort = newpostord
                            , visited = (visited ac') ++ [v]
                            , called = called ac' + 1
                            }
               in res
        in depthFirstSearch s sbfs

postordering :: DFS -> [Vertex]
postordering = reverse . topsort

-- | :)
type DAG = Graph

-- | Ginen a DAG and a vertex you get the distances
distances' :: DAG  -> Vertex -> IM.IntMap Vertex
distances' g s =
  let topsorted = topsort $ dfs g s
      initdists = foldl' (\ac v -> IM.insert v 0 ac) IM.empty $ vertices g
   in foldl' (\ac v -> 
        let neis = neighbors g v
            distv = case IM.lookup v ac of
                      Nothing -> 0
                      Just d -> d
         in foldl' (\dists' nei -> 
           let neidist = case IM.lookup nei dists' of
                           Nothing -> 0
                           Just nd -> nd
               newdist = max neidist (distv+1)
            in IM.insert nei newdist dists'
                  ) ac neis
      ) initdists topsorted

type Distances = IM.IntMap Vertex

-- | Ginen a DAG and a vertex you get the distances
distances :: DAG  -> DFS -> Vertex -> Distances
distances g dfs' s =
  let topsorted = topsort $ dfs'
      !initdists = foldl' (\ac v -> IM.insert v 0 ac) IM.empty $ vertices g
   in foldl' (\ac v -> 
        let neis = neighbors g v
            distv = case IM.lookup v ac of
                      Nothing -> 0
                      Just d -> d
         in foldl' (\dists' nei -> 
           let neidist = case IM.lookup nei dists' of
                           Nothing -> 0
                           Just nd -> nd
               newdist = max neidist (distv+1)
            in IM.insert nei newdist dists'
                  ) ac neis
      ) initdists topsorted

type TopologicalSorting = [Vertex]
-- |checks if s is predecessor of t
dependsOn :: TopologicalSorting -> Vertex -> Vertex -> Bool
dependsOn topsorted t s = elem t (snd (span ((==) s) topsorted))

areConnected :: Distances -> Vertex -> Vertex -> Bool
areConnected dists u v = (fromJust $ IM.lookup v dists) > 0 || v == u

-- |Longest path from tail to nose
longestPath :: Graph -> Vertex -> Vertex -> [Edge]
longestPath g s t =
  let dfs' = dfs g s
      topsorted = topsort dfs'
      dists = distances g dfs' s
      revg = reverseGraph g
      disconnected = filter (\n -> not (areConnected dists s n)) $ vertices g
   in if not $ dependsOn topsorted t s
     then []
     else 
       if not $ null disconnected
          then
            let cleangraph = filterVertices (\v -> not $ elem v disconnected) g
             in longestPath cleangraph s t
          else
            let path' :: Vertex -> [Edge] -> [Edge]
                path' v p 
                  | v == s = p
                  | otherwise = 
                         let parents = neighbors revg v
                          in if null parents
                                then []
                                else
                                  if parents == [s]
                                   then (Edge s v):p
                                   else 
                                     let pred :: Vertex
                                         pred = fst $ foldl'
                                           (\(prevmax,maxdist) parent ->
                                             let currentDist =
                                                   case IM.lookup parent dists of
                                                     Nothing -> (0,0)
                                                     Just d -> (parent,d)
                                              in if maxdist < snd currentDist
                                                    then currentDist
                                                    else (prevmax,maxdist)
                                                    ) (0,0) parents
                                      in  path' pred $ (Edge pred v): p
             in path' t []
