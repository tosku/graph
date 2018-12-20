{-|
Module      : Metrics
Description : Various distance and density metrics for graphs
Copyright   : Thodoris Papakonstantinou, 2018
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX

[Wikipedia link for detailed description](https://en.wikipedia.org/wiki/Distance_(graph_theory))
 -}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}


module Data.Graph.AdjacencyList.Metrics
  ( graphEccentricity
  , graphRadius
  , graphDiameter
  , graphDensity
  ) where

import Data.List
import Data.Maybe
import qualified Data.IntMap   as IM

import Data.Graph.AdjacencyList
import Data.Graph.AdjacencyList.WFI

graphEccentricity :: Vertex -> Distances -> Maybe Weight
graphEccentricity v (Distances dis) =
  let vdis = IM.lookup v dis
   in maximum <$> vdis

graphRadius :: Distances -> Maybe Weight
graphRadius dis =
  let (Distances dism) = dis
      vs = IM.keys dism
   in minimum $ filter (\d -> d /= Just 0 && d /= Nothing) 
     $ map (\v -> graphEccentricity v dis) vs

graphDiameter :: Distances -> Maybe Weight
graphDiameter dis =
  let (Distances dism) = dis
      vs = IM.keys dism
   in maximum $ filter (\d -> d /= Just 0 && d /= Nothing) 
     $ map (\v -> graphEccentricity v dis) vs

-- | Since the representation of undirected graphs dublicated edges no need for
-- undirected version of density
graphDensity :: Graph -> Rational
graphDensity g =
  let ne = fromIntegral $ length $ edges g
      nv = fromIntegral $ length $ vertices g
   in ne / (nv * (nv - 1))
