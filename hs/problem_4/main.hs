
import Debug.Trace (trace)

main :: IO ()
main = putStrLn $ show $
  maximum $ filter (isPalindrome . show) [x*y | x <- [100..999], y <- [100..999]]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome s =
  take (length s `div` 2) s == reverse (drop ((length s + 1) `div` 2) s)

-- largestPalindrome :: Int -> Int
-- largestPalindrome = go 0
--   where
--     go :: Int -> Int -> Int
--     go mx n = if n <= 0 then mx else
--       max (inner_go mx n) (go 
