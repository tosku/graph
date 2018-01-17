import qualified Test.GrTest as T
import Test.Graph.Grid as G
import Test.Graph.BFS as BFS

main :: IO ()
main = do
  putStrLn "\n"
  putStrLn $ "Test Begins"
  T.reportTests $ G.fastTests 
    ++ BFS.fastTests 
