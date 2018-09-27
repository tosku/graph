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
  ) where

import Data.List
import Data.Maybe
import qualified Data.IntMap   as IM
import qualified Data.IntSet   as Set
import qualified Data.Sequence as Seq

import Data.Graph.AdjacencyList

data DFS = DFS { postordering :: Seq.Seq Vertex
               , discovered   :: Set.IntSet
               } deriving (Eq, Show)

initialBFS :: Vertex -> DFS
initialBFS s = DFS { postordering = Seq.empty
                   , discovered   = Set.insert s Set.empty
                   }

-- | Depth first search
dfs :: Graph -> Vertex -> DFS
dfs g s = 
  let sbfs = initialBFS s
      depthFirstSearch :: Vertex -> DFS -> DFS
      depthFirstSearch v ac =
        let ns = neighbors g v
            disc = discovered ac
         in if null ns
              then ac
              else
                let mnextvertex = 
                      let mns = [ u | u <- ns, not (Set.member u (disc))]
                       in if not (null mns)
                             then Just $ head mns
                             else Nothing
                 in case mnextvertex of
                      Nothing -> ac
                      Just nextvertex -> 
                        let ac' = ac { discovered = Set.insert nextvertex disc }
                         in depthFirstSearch nextvertex ac'
   in depthFirstSearch s sbfs
