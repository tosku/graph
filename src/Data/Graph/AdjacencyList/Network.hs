{-|
Module      : Network
Description : Network data type
Copyright   : Thodoris Papakonstantinou, 2017
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX
 -}

module Data.Graph.AdjacencyList.Network
  ( Network (..)
  , Capacity
  , Capacities
  , Flow
  , uniformCapacities
  ) where

import Data.List
import Data.Maybe
import qualified Data.Map.Lazy as M
import qualified Data.IntSet as Set

import Data.Graph.AdjacencyList

type Capacity = Rational 
type Capacities = M.Map Edge Capacity 
type Flow = Capacity

showCapacities :: Capacities -> String
showCapacities cps =
  show $ fmap (\c -> fromRational c :: Double) cps

data Network = Network { graph :: !Graph
                       , source :: Vertex
                       , sink :: Vertex
                       , capacities :: Capacities
                       , flow :: Capacities
                       }
                       deriving (Eq)

instance Show Network where
  show net =
    "Network" <> show (graph net) <> "\n"
    <> " source: " <> show (source net) <> "\n"
    <> " sink  : " <> show (sink net) <> "\n"
    <> " capacities: " <> showCapacities (capacities net) <> "\n"
    <> " flows: " <> showCapacities (flow net) <> "\n"

-- | Set all capacities to 1
uniformCapacities :: Graph -> Capacities
uniformCapacities g =
  M.fromList $ map (\e -> (e,1)) $ edges g
