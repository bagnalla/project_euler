import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = putStrLn $ show $ evalState (f 20 20) Map.empty

f :: Int -> Int -> State (Map (Int, Int) Int) Int
f 0 _ = return 1
f _ 0 = return 1
f n m = do
  tbl <- get
  case Map.lookup (n, m) tbl of
    Just r -> return r
    Nothing -> do
      l <- f (n - 1) m
      r <- f n (m - 1)
      modify $ Map.insert (n-1, m) l . Map.insert (n, m-1) r
      return $ l + r

-- f :: Int -> Int -> Int
-- f 0 _ = 1
-- f _ 0 = 1
-- f n m = f (n - 1) m + f n (m - 1)
