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
  , topsort
  , longestPath
  ) where

import Data.List
import Data.Maybe
import qualified Data.IntMap   as IM
import qualified Data.IntSet   as Set
import qualified Data.Sequence as Seq

import Data.Graph.AdjacencyList

data DFS = DFS { postordering :: [Vertex]
               , discovered   :: Set.IntSet
               , called :: Int
               } deriving (Eq, Show)

initialDFS :: DFS
initialDFS = DFS { postordering = []
                 , discovered   = Set.empty
                 , called = 0
                 }

-- | Depth first search
dfs :: Graph -> Vertex -> DFS
dfs g s = 
  if not $ elem s (vertices g) 
     then initialDFS
     else
       let sbfs = initialDFS
           depthFirstSearch :: [Vertex] -> DFS -> DFS
           depthFirstSearch [] ac = ac
           depthFirstSearch (v:stack) ac =
              let ns = neighbors g v
                  disc = discovered ac
                  postord = postordering ac
                  newpostord = foldl' (\ac v' -> 
                                if elem v' ac
                                  then v': (filter (\x -> x /= v') ac)
                                  else v':ac
                                      ) postord (v:ns)
                  newstack = [ u | u <- ns, not (Set.member u (disc))] ++ stack
                  ac' = ac { discovered = Set.insert v disc 
                           , postordering = newpostord
                           , called = called ac + 1
                           }
               in depthFirstSearch newstack ac' 
        in depthFirstSearch [s] sbfs

-- Topological sorting is the reverse of postordering in DAGs
topsort :: DFS -> [Vertex]
topsort = reverse . postordering

-- Longest path from source from to
longestPath :: Graph -> Vertex -> Vertex -> [Edge]
longestPath g s t =
  let (larger, latests) = break ((==) t) $ postordering $ dfs g s
      lp :: [Vertex] -> [Edge] -> [Edge]
      lp [] ac        = ac
      lp (v:parents) ac = 
         if null parents
           then ac
           else 
            let (par:rests) = parents
                edgePath = Edge par v
             in if edgeExists g edgePath
                  then lp parents $ edgePath:ac
                  else lp (v:rests) ac
   in lp latests []
