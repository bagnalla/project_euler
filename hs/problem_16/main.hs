import Data.Char (digitToInt)

main :: IO ()
main = putStrLn $ show $ sum $ digitToInt <$> (show (2^1000))

