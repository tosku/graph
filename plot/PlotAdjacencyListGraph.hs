{-|
Module      : plotgraph
Description : function to plot `Data.AdjacencyList.Graph`s
Copyright   : Thodoris Papakonstantinou, 2018
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX

 -}

module PlotAdjacencyListGraph
  ( plotGraph
  , makeDotGraph
  ) where

import Data.Maybe
import Data.List

import qualified Data.IntMap.Strict as IM
import Data.Maybe
import qualified Data.GraphViz as GVZ

import qualified Data.Graph.AdjacencyList as AGR

dotGraphParams :: GVZ.GraphvizParams Int String Bool () String
dotGraphParams = GVZ.defaultParams

makeDotGraph :: AGR.Graph -> GVZ.DotGraph Int
makeDotGraph g = GVZ.graphElemsToDot dotGraphParams nodes eds
 where
   nodes = map (\v -> (v, show v)) $ AGR.vertices g
   eds = map (\(AGR.Edge u v) -> (u, v, True)) $ AGR.edges g

plotGraph :: AGR.Graph -> IO [Char]
plotGraph g =
  GVZ.addExtension (GVZ.runGraphviz (makeDotGraph g)) GVZ.Png "testgraph"

