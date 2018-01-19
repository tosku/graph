import qualified TestHS as T
import Test.Graph.AdjacencyList.Grid as G
import Test.Graph.AdjacencyList.BFS as BFS
import Test.Graph.AdjacencyList.PushRelabel.Pure as PRP

main :: IO ()
main = do
  putStrLn "\n"
  putStrLn $ "Test Begins"
  T.reportTests $ G.fastTests 
    ++ BFS.fastTests 
    ++ PRP.fastTests 
