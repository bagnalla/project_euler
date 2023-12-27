import Data.Char (digitToInt)

main :: IO ()
main = putStrLn $ show $ sum $ digitToInt <$> (show $ factorial 100)

factorial :: Integer -> Integer
factorial n = if n <= 1 then 1 else n * factorial (n-1)
