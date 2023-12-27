import Data.Set (Set)
import qualified Data.Set as Set

import Data.List ((\\), nub)
import Debug.Trace (trace)

main :: IO ()
main = do
  putStrLn $ show $
    sum $ Set.elems $ Set.difference (Set.fromList [1..28123]) (sums abundantNumbers)

  -- putStrLn $ show $ sum $ filter (not . summable abundantNumbers) $ [1..28123]

  -- putStrLn $ show $ filter (summable abundantNumbers) $
  --   Set.elems $ Set.difference (Set.fromList [1..28124]) (sums abundantNumbers)
  
  -- putStrLn $ show $ sums $ take 5 abundantNumbers
    -- map (\n -> (n, properDivisors n, sum $ properDivisors n)) $
    -- [1..maximum (take 20 abundantNumbers)] \\ take 20 abundantNumbers
    -- sums abundantNumbers
    -- length $ f [1..28123] (summables abundantNumbers)
    -- [trace (show n) n | n <- [1..28123]
    --    , not $ summable abundantNumbers n]

-- -- https://stackoverflow.com/a/39539472
-- powerset :: [a] -> [[a]]
-- powerset [] = [[]]
-- powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs

divisible :: Int -> Int -> Bool
divisible n m = n `mod` m == 0

makeOdd :: Int -> Int
makeOdd n = if divisible n 2 then n - 1 else n

properDivisors :: Int -> [Int]
properDivisors n = (if 1 < n then [1] else []) ++
  concat [[x, n `div` x] | x <- [2 .. makeOdd $ round $ sqrt $ fromIntegral n]
                         , divisible n x]

abundantNumbers :: [Int]
abundantNumbers = [n | n <- [1..28123], n < sum (properDivisors n)]

summable :: [Int] -> Int -> Bool
summable [] _ = False
summable (x : xs) n =
  if x > n then False else
    n-x `elem` (x:xs) || summable xs n

sums :: [Int] -> Set Int
sums [] = Set.empty
sums (x:xs) = Set.union (Set.fromList [x + y | y <- (x:xs), x + y <= 28123]) (sums xs)

-- f :: [Int] -> [Int] -> [Int]
-- f xs [] = xs
-- f [] _ = []
-- f (x:xs) (y:ys) = if x == y then f xs ys else f xs (y:ys)
