
main :: IO ()
main = putStrLn $ show $ findPrime 10001

isPrime :: Int -> Bool
isPrime n = if n `mod` 2 == 0 then False else
  go $ prep $ round $ sqrt $ fromIntegral n
  where
    prep :: Int -> Int
    prep n = if n `mod` 2 == 0 then n - 1 else n
    go :: Int -> Bool
    go m = if m <= 1 then True else
      if n `mod` m == 0 then False
      else go (m - 2)

findPrime :: Int -> Int
findPrime n = if n <= 1 then 2 else go (n-1) 3
  where
    go :: Int -> Int -> Int
    go n m = if isPrime m then
               if n <= 1 then m else go (n-1) (m + 2)
             else go n (m + 2)
