import Data.List (sortOn)

main :: IO ()
main = putStrLn $ show $
  head $ sortOn (negate . length . snd) $ (\n -> (n, collatz n)) <$> [1..1000000]

collatz :: Int -> [Int]
collatz n = n : if n == 1 then [] else
                  collatz $ if n `mod` 2 == 0 then
                              n `div` 2
                            else
                              3 * n + 1
