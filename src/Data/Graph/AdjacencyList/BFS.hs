{-|
Module      : BFS
Description : The reduction of finding the ground state Blume-Capel realization to the max-flow of a flow graph 
Copyright   : Thodoris Papakonstantinou, 2017
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX


 -}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}


module Data.Graph.AdjacencyList.BFS
  ( bfs
  , adjBFS
  , BFS (..)
  , spanningTree
  ) where

import Data.List
import Data.Tuple
import Data.Maybe
import qualified Data.IntMap as IM
import qualified Data.IntSet as Set

import Data.Graph.AdjacencyList

data BFS = BFS { frontier :: Set.IntSet
               , level :: IM.IntMap Int -- ^ Keeps level of vertex
               , parent :: IM.IntMap Vertex -- ^ Gives parent of vertex
               , maxLevel :: Int
               , topSort :: [Vertex] -- ^ Is true topological sorting in DAGs
               } deriving (Eq, Show)

initialBFS :: Vertex -> BFS
initialBFS s = BFS { frontier = Set.singleton s
                   , level = IM.fromList [(s,0)]
                   , parent= IM.empty
                   , maxLevel = 0
                   , topSort = []
                   }

-- | BFS for implicit neighbor definition (grids, infinite graphs)
bfs :: Graph -> Vertex -> BFS
bfs g s = 
  let sbfs = initialBFS s
      breadthFirstSearch b =
        if Set.empty == frontier b || not (elem s (vertices g))
           then b
           else
             let oldLevel = maxLevel b
                 newLevel = oldLevel + 1
                 oldLevels = level b
                 oldFrontiers = frontier b
                 frontPar = 
                    let toCheck = 
                          Set.foldr 
                            (\v ac-> ac ++ (zip (neighbors g v) (repeat v))) 
                            [] oldFrontiers
                     in filter (\(n,p) -> not $ IM.member n oldLevels) toCheck
                 newFrontiers = Set.fromList $ map fst frontPar
                 oldParents = parent b
                 newParents = foldl' 
                            (\ac (n,p) -> IM.insert n p ac) 
                            oldParents frontPar
                 newLevels = Set.foldl' 
                           (\ac v -> IM.insert v newLevel ac) 
                           oldLevels newFrontiers
                 bbfs = breadthFirstSearch (b { frontier = newFrontiers
                                              , level = newLevels 
                                              , parent = newParents
                                              , maxLevel = newLevel
                                              , topSort = (topSort b) 
                                              ++ Set.toList oldFrontiers
                                            })
               in bbfs
   in breadthFirstSearch sbfs

-- | BFS for graph with provided vertex adjacencyList
adjBFS :: IM.IntMap [Vertex] -> Vertex -> BFS
adjBFS neimap s = breadthFirstSearch sbfs
  where neighbors v = case IM.lookup v neimap of
                        Nothing -> []
                        Just ns -> ns
        sbfs = initialBFS s
        breadthFirstSearch b
          | Set.empty == frontier b = b
          | otherwise = bbfs
            where oldLevel = maxLevel b
                  newLevel = oldLevel + 1
                  oldLevels = level b
                  oldFrontiers = frontier b
                  frontPar = 
                    let toCheck = 
                          Set.foldr 
                            (\v ac-> ac ++ (zip (neighbors v) (repeat v))) 
                            [] oldFrontiers
                     in filter (\(n,p) -> not $ IM.member n oldLevels) toCheck
                  newFrontiers = Set.fromList $ map fst frontPar
                  oldParents = parent b
                  newParents = foldl' 
                                 (\ac (n,p) -> IM.insert n p ac) 
                                 oldParents frontPar
                  newLevels = Set.foldl' 
                                 (\ac v -> IM.insert v newLevel ac) 
                                 oldLevels newFrontiers
                  bbfs = breadthFirstSearch (b { frontier = newFrontiers
                             , level = newLevels 
                             , parent = newParents
                             , maxLevel = newLevel
                             , topSort = (topSort b) ++ Set.toList oldFrontiers
                             })

spanningTree :: BFS -> [Edge]
spanningTree b = 
  map (fromTuple . swap) $ IM.toList $ parent b

