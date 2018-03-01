{-|
Module      : Network
Description : Network data type
Copyright   : Thodoris Papakonstantinou, 2017
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX
|-}

module Data.Graph.AdjacencyList.Network
  ( Network (..)
  , Capacity
  , Capacities
  , Flow
  ) where

import Data.List
import Data.Maybe
import qualified Data.Map.Lazy as M
import qualified Data.IntSet as Set

import Data.Graph.AdjacencyList

type Capacity = Rational 
type Capacities = M.Map Edge Capacity 
type Flow = Capacity

data Network = Network { graph :: Graph
                       , source :: Vertex
                       , sink :: Vertex
                       , capacities :: Capacities
                       , flow :: Capacities
                       }
                       deriving (Show,Eq)


