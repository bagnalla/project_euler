import Debug.Trace (trace)

main :: IO ()
main = putStrLn $ show $ go 0 0 1

go :: Int -> Int -> Int -> Int
go acc n m = let k = n + m in
  trace (show k) $
  if k > 4000000 then acc else
    go (acc + if k `mod` 2 == 0 then k else 0) m k
