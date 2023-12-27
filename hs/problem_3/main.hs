import Debug.Trace (trace)

main :: IO ()
-- main = putStrLn $ show $ map (\n -> (n, isPrime n)) [0..20]
-- main = putStrLn $ show $ largestPrimeFactor 13195
main = putStrLn $ show $ primeFactors 600851475143

prep :: Int -> Int
prep n = if n `mod` 2 == 0 then n - 1 else n

isPrime :: Int -> Bool
isPrime n = if n `mod` 2 == 0 then False else
  go $ prep $ round $ sqrt $ fromIntegral n
  where
    go :: Int -> Bool
    go m = if m <= 1 then True else
      if n `mod` m == 0 then False
      else go (m - 2)

-- largestPrimeFactor :: Int -> Int
-- largestPrimeFactor n =
--   -- go $ prep $ n `div` 2
--   go $ prep $ round $ sqrt $ fromIntegral n
-- -- largestPrimeFactor n =
--   -- trace ("n: " ++ show n) $
--   -- go $ prep n
--   where
--     go :: Int -> Int
--     go m =
--       -- trace ("m: " ++ show m) $
--       if m <= 1 then 1 else
--       if n `mod` m == 0 && isPrime m then m else go (m - 2)

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
