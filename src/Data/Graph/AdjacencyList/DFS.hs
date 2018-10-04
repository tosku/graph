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
  ) where

import Data.List
import Data.Maybe
import qualified Data.IntMap   as IM
import qualified Data.IntSet   as Set
import qualified Data.Sequence as Seq

import Data.Graph.AdjacencyList

data DFS = DFS { postordering :: Seq.Seq Vertex
               , discovered   :: Set.IntSet
               , called :: Int
               } deriving (Eq, Show)

initialDFS :: DFS
initialDFS = DFS { postordering = Seq.empty
                 , discovered   = Set.empty
                 , called = 0
                 }

topsort :: DFS -> [Vertex]
topsort d =
  foldl' (\ac v-> v:ac ) [] $ Seq.viewr $ postordering d

unionDFS :: DFS -> DFS -> DFS
unionDFS a b =
  let !psta = postordering a
      !pstb = postordering b
      !disa = discovered a
      !disb = discovered b
      !iter = called b
   in DFS { postordering = pstb
          , discovered = Set.union disa disb
          , called = iter + 1
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
                  newpostord = if Set.member v disc
                                  then postord
                                  else ((Seq.<|) v postord)
                  newstack = [ u | u <- ns, not (Set.member u (disc))] ++ stack
                  ac' = ac { discovered = Set.insert v disc 
                           , postordering = newpostord
                           }
               in unionDFS ac (depthFirstSearch newstack ac') 
        in depthFirstSearch [s] sbfs
