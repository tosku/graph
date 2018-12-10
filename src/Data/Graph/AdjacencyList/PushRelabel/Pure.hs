{-|
Module      : PushRelabel - Pure
Description : Maximum Flow - Push relabel - Loom Algorithm
Copyright   : Thodoris Papakonstantinou, 2017
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX


= Loom - Push Pelabel
The loom algorithm is a push relabel implementation for solving the 
 [max flow problem](https://en.wikipedia.org/wiki/Push%E2%80%93relabel_maximum_flow_algorithm#Practical_implementations) 
The algorithm is tested on directed graphs.

=== Definitions
A network \( N \) is defined by a directed graph \( G \) its source and sink \(s,t\) and the capacities \(C : E \rightarrow R^+ \) 
Following push-relabel's terminology the residual graph \(R\) is a network containing both original edges of \(G\) (forward edges) and backward (reverse) edges, with the additional properties of __preflow__ \(F : E_R \rightarrow R^+ \) on the forward edges and residual capacities \(C_R\) on all edges of \(R\) as well as the following properties on the vertices:
- Height \(H: V \rightarrow R^+\) which determines whether preflow can be pushed through an edge.
- Excess \(X:  V \rightarrow R^+\) recording the excess flow of each vertex. 
When the algorithm terminates all excess is \(0\) and the preflow of each edges is
the actual maximum flow of \(N\).

=== Operations
The main difference in the definitions lies in the split of the PR push operation
into two, depending on whether it is performed in a forward edge or backward
edge. The former operation is called __push__ (from now on, /push/ will be used in
this context) and the latter __pull__.
Relabel is the usual PR adjusting of heights.

/PR/ guaranties that there is a cut between the source and sink in the residual
graph partitioning \(R\) into \(S\) and \(T\) vertices.

The algorithm is iterative and each iteration consists of three steps

1. global-relabel
2. global-push
3. global-pull

 -}

{-# LANGUAGE BangPatterns #-}

module Data.Graph.AdjacencyList.PushRelabel.Pure where

import Data.List
import Data.Maybe
import qualified Data.Map.Lazy as M
import qualified Data.IntMap.Lazy as IM
import qualified Data.IntSet as Set
import Control.Monad

import Data.Graph.AdjacencyList
import Data.Graph.AdjacencyList.Network
import Data.Graph.AdjacencyList.PushRelabel.Internal
import qualified Data.Graph.AdjacencyList.BFS as BFS

-- | Implementation of the push relabel algorithm
pushRelabel :: Network -> Either String ResidualGraph
pushRelabel net =
  let initg = initializeResidualGraph net
      res = loom initg 0
      nvs = vertices $ graph $ network res
      s = source net
      t = sink net
      insouts = filter (\v -> v /= s && v /= t && inflow res v < outflow res v) nvs
      xsflows = filter (\v -> v /= s && v /= t && inflow res v - outflow res v /= excess res v) nvs
      ofvs = IM.foldr (\ovs ac -> Set.union ac ovs) Set.empty $ overflowing res
      notofvs = filter (\ ov -> 
                          let (ResidualVertex v l h x) = fromJust (IM.lookup ov (netVertices res)) 
                              ml = (IM.lookup l (overflowing res)) 
                           in case ml of
                                Nothing -> True
                                Just os -> not $ Set.member ov os
                       ) $ Set.toList $ getOverflowing $ netVertices res
      errovfs = Set.filter (\v -> excess res v == 0) ofvs
   in if null insouts && null xsflows && Set.null errovfs && null notofvs
      then Right res
      else 
        if not $ null insouts 
              then Left $ "Error Inflow < Outflow " ++ show insouts
              else
                if not $ null xsflows 
                  then Left $ "Error vertex excess " ++ show xsflows
                  else
                    if not $ Set.null errovfs 
                      then Left $ "Error not really overflowing " ++ show errovfs
                      else Left $ "Error not in overflowing " ++ show notofvs
                        ++ " overflowings are " ++ show (overflowing res)
                        ++ " nevertices are " ++ show (netVertices res)

-- | The main part of the algorithm. 
loom :: ResidualGraph -> Int -> ResidualGraph 
loom rg steps = 
  let g = rg `seq` (graph $ network rg)
      s = source $ network rg
      t = sink $ network rg
      es = edges g
      vs = vertices g
      olf = netFlow rg
      bfsrg = bfsRelabel rg
      rg' = prePush $ prePull bfsrg 
      nfl = netFlow rg'
      steps' = steps + 1
      oovfls = overflowing rg
      novfls = overflowing rg'
   in if nfl == olf 
         then 
           if oovfls == novfls
              then rg' { network = networkFromResidual rg'
                       , steps = steps'}
              else loom rg' steps'
         else loom rg' steps'

-- | Pushes flow through edges with starting vertices which are the ends of source edges 
-- (U) and ending edges that are the start of sink edges (V)
prePush :: ResidualGraph -> ResidualGraph 
prePush rg = 
  let ovfs = overflowing rg
   in IM.foldl' (\ac lset -> 
         Set.foldl' (\ac' v -> pushNeighbors ac' v)
         ac lset
      ) rg ovfs

prePull :: ResidualGraph -> ResidualGraph 
prePull rg = 
  let ovfs = overflowing rg
   in IM.foldr' (\lset ac -> 
         Set.foldl' (\ac' v -> pullNeighbors ac' v)
         ac lset
               ) rg ovfs

-- | Push through all (forward) residual neighbors
pushNeighbors :: ResidualGraph -> Vertex -> ResidualGraph
pushNeighbors g v =
  let neimap = netNeighborsMap g
      (fns, rns) = fromJust $ IM.lookup v neimap
      feds = map (\n -> fromTuple (v,n)) fns
   in foldl' (\ac e -> 
                let mv = push ac e
                in case mv of 
                    Nothing -> ac
                    Just g'' -> g'') g feds

-- | Push through all (backward) meaning pull all residual neighbors
pullNeighbors :: ResidualGraph -> Vertex -> ResidualGraph
pullNeighbors g v =
  let neimap = netNeighborsMap g
      (fns, rns) = fromJust $ IM.lookup v neimap
      reds = map (\n -> fromTuple (n,v)) rns
   in foldl' (\ac e -> 
                let mv = pull ac e
                 in case mv of 
                      Nothing -> ac
                      Just g'' -> g'') g reds

-- | Global relabel according to bfs from source and sink
bfsRelabel :: ResidualGraph -> ResidualGraph
bfsRelabel rg =
  let g = graph $ network rg
      sh = numVertices g
      (slvs, tlvs) = residualDistances rg
      rg' = IM.foldrWithKey 
              (\ v l ac -> 
                 -- Heights for the source partition vertices is N \+ their distance to the source
                let h = sh + l 
                  in updateHeight ac v h
              ) rg slvs 
   in IM.foldrWithKey (\ v h ac
       -- Heights for the sink partition vertices equals the distance from sink
       -> updateHeight ac v h) 
       rg' tlvs
