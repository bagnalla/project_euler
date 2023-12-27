-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

-- Find the sum of all the multiples of 3 or 5 below 1000.

import Data.List (nub)

main = putStrLn $ show $ sum $ nub $ multiples 3 1000 ++ multiples 5 1000

multiples :: Int -> Int -> [Int]
multiples k n = [k * m | m <- [1..(n-1) `div` k]]
