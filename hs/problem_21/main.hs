
import Debug.Trace (trace)

main :: IO ()
main = putStrLn $ show $ sum [n | n <- [1..9999], d(d(n)) == n, n /= d(n)]

divisible :: Int -> Int -> Bool
divisible n m = n `mod` m == 0

properDivisors :: Int -> [Int]
properDivisors n = 1 : concat [[x, n `div` x] | x <- [2 .. round $ sqrt $ fromIntegral n]
                                              , divisible n x ]
  
d :: Int -> Int
d = sum . properDivisors
