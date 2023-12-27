
main :: IO ()
main =
  let (a, b, c) = head $ filter (\(a, b, c) -> a + b + c == 1000 && isTriple a b c)
                  [(a, b, c) | a <- [1..1000], b <- [a+1..1000], c <- [b+1..1000]] in
    putStrLn $ show $ a * b * c

isTriple :: Int -> Int -> Int -> Bool
-- isTriple a b c = a < b && b < c && a^2 + b^2 == c^2
isTriple a b c = a^2 + b^2 == c^2

-- f :: Int -> Int -> [[Int]]
-- f n c = if n <= 1 then [[c]] else
  
