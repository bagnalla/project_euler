
-- main :: IO ()
-- main = putStrLn $ show $ search 20

main :: IO ()
main = putStrLn $ show $ head [n | n <- [20,40..], all (divisible n) [2..19]]
  
  -- head $ filter (\n -> all (divisible n) [2..19]) [20,40..]

divisible :: Int -> Int -> Bool
divisible n m = n `mod` m == 0

-- search :: Int -> Int
-- search n = if all (divisible n) [1..19] then n else search $ n + 20

-- search :: Int -> Int
-- search n = if check n then n else search $ n + 20

-- check :: Int -> Bool
-- check n = go 19
--   where
--     go :: Int -> Bool
--     go m = m <= 0 || (n `mod` m == 0 && go (m - 1))
