import Control.Monad.State
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map

import Debug.Trace (trace)

main :: IO ()
main = do
  -- let (n, ks) = evalState run Map.empty
  -- putStrLn $ show n
  -- putStrLn $ show ks
  -- putStrLn $ show $ length ks
  -- putStrLn $ show $ divisors <$> take 10 triangleNumbers
  -- putStrLn $ show $ factors <$> take 10 triangleNumbers
  putStrLn $ show $ head $ filter (\(_, ks) -> length ks > 500) $
    (\n -> (n, factors n)) <$> triangleNumbers

-- run :: State (Map Int [Int]) (Int, [Int])
-- run = do
--   nums_factors <- mapM (\n -> do
--                            fs <- factors n
--                            return (n, fs)) triangleNumbers
--   return $ head $ filter (\(n, ks) -> length ks > 500) nums_factors

factors :: Int -> [Int]
-- factors n = (if divisible n 2 then [2, n `div` 2] else []) ++
--   concatMap (\k -> if divisible n k then [k, n `div` k] else [])
--   [1,3 .. prep $ round $ sqrt $ fromIntegral n]
--   where
--     prep :: Int -> Int
--     prep n = if divisible n 2 then n - 1 else n
factors n = concatMap (\k -> if divisible n k then [k, n `div` k] else [])
            [1 .. prep $ round $ sqrt $ fromIntegral n]
  where
    prep :: Int -> Int
    prep n = if divisible n 2 then n - 1 else n

-- factors :: Int -> State (Map Int [Int]) [Int]
-- factors n = if isPrime n then return [n] else do
--   -- trace ("n: " ++ show n) $ do
--   memo <- get
--   case Map.lookup n memo of
--     Just ks -> return ks
--     Nothing -> do
--       let primes = nub $ primeFactors n
--       others <- concat <$> (forM primes $ \p -> factors $ n `div` p)
--       let ks = nub $ 1 : primes ++ others ++ [n]
--       modify $ Map.insert n ks
--       -- trace ("ks: " ++ show ks)
--       return $ ks

divisible :: Int -> Int -> Bool
divisible n m = n `mod` m == 0

triangleNumbers :: [Int]
triangleNumbers = go 0 1
  where
    go :: Int -> Int -> [Int]
    go acc n = acc + n : go (acc + n) (n+1)

divisors :: Int -> [Int]
divisors n = filter (divisible n) [1..n `div` 2] ++ [n]

isPrime :: Int -> Bool
isPrime n =
  n `elem` [1, 2] ||
  not (divisible n 2) &&
  all (not . divisible n) [3,5 .. prep $ round $ sqrt $ fromIntegral n]
  where
    prep :: Int -> Int
    prep n = if divisible n 2 then n - 1 else n

findPrimeFactor :: Int -> Int
findPrimeFactor n = if n `mod` 2 == 0 then 2 else go 3
  where
    go :: Int -> Int
    go i = if i >= n then 1 else
      if n `mod` i == 0 && isPrime i then i else go (i + 2)

primeFactors :: Int -> [Int]
primeFactors n = if isPrime n then [n] else
  let i = findPrimeFactor n in
    i : primeFactors (n `div` i)

-- multiples of k up to n
multiples :: Int -> Int -> [Int]
multiples k n = [k,2*k..n]
