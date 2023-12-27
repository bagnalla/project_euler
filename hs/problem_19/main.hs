
import Debug.Trace (trace)

main :: IO ()
main = putStrLn $ show $ run initDay

data Day =
  Day { weekday :: Int
      , monthday :: Int
      , month :: Int
      , year :: Int
      }
  deriving Show

-- 0 - sunday
-- 1 - monday
-- ...
-- 6 - saturday

initDay :: Day
initDay = Day { weekday = 1
              , monthday = 0
              , month = 0
              , year = 1900 }

daysInMonth :: Int -- current year
            -> Int -- month
            -> Int
daysInMonth year month =
  if month == 1 && year `mod` 4 == 0 &&
     (year `mod` 100 /= 0 || year `mod` 400 == 0) then
    29
  else
    monthDays!!month
  where
    monthDays :: [Int]
    monthDays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

inc :: Day -> Day
inc Day { weekday = wd
        , monthday = md
        , month = m
        , year = y } =
  let wd' = (wd + 1) `mod` 7
      md' = (md + 1) `mod` daysInMonth y m
      m' = if md' == 0 then (m + 1) `mod` 12 else m
      y' = if m' == 0 && md' == 0 then y + 1 else y
  in
    Day { weekday = wd'
        , monthday = md'
        , month = m'
        , year = y' }

run :: Day -> Int
run = go 0
  where
    go :: Int -> Day -> Int
    go count d@(Day { weekday = wd
                    , monthday = md
                    , month = m
                    , year = y }) =
      -- trace ("d: " ++ show d) $
      if y > 2000 then count else
        go (if y >= 1901 && wd == 0 && md == 0 then
              trace ("count: " ++ show (count + 1)) $
              trace ("d: " ++ show d) $ count + 1 else
              count) $ inc d
