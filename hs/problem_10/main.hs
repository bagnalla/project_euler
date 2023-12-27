main :: IO ()
main = putStrLn $ show $
  sum $ 2 : filter isPrime [3,5..2000000]

divisible :: Int -> Int -> Bool
divisible n m = n `mod` m == 0

isPrime :: Int -> Bool
isPrime n =
  n `elem` [1, 2] ||
  not (divisible n 2) &&
  all (not . divisible n) [3,5 .. prep $ round $ sqrt $ fromIntegral n]
  where
    prep :: Int -> Int
    prep n = if divisible n 2 then n - 1 else n
