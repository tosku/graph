{-|
Module      : plotgraph
Description : function to plot `Data.AdjacencyList.Graph`s
Copyright   : Thodoris Papakonstantinou, 2018
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX

 -}

import PlotAdjacencyListGraph
import qualified Data.Graph.AdjacencyList as AGR

-- | Test graph
testgraph = 
  let vs = [1..8]
      neis = (\v -> let nei 1 = [2,5,6]
                        nei 2 = [3,5]
                        nei 3 = [4,6]
                        nei 4 = [7]
                        nei 5 = [4,7]
                        nei 6 = [8,7]
                        nei 7 = []
                        nei 8 = [7]
                     in nei v
             )
   in AGR.createGraph vs neis

main :: IO ()
main = do
  putStrLn "\n"
  putStrLn $ "Plotting test graph"
  plotGraph testgraph
  putStrLn "done!"
