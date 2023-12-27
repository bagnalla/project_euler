import Data.Maybe (fromJust, fromMaybe)

import Debug.Trace (trace)

main :: IO ()
main = putStrLn $ show $
  length $ concat $ (toWords <$> [1..999]) ++ ["onethousand"]

ones :: [(Char, String)]
ones = [
  ('1', "one"),
  ('2', "two"),
  ('3', "three"),
  ('4', "four"),
  ('5', "five"),
  ('6', "six"),
  ('7', "seven"),
  ('8', "eight"),
  ('9', "nine")
  ]
  
tens :: [(Char, String)]
tens = [
  ('2', "twenty"),
  ('3', "thirty"),
  ('4', "forty"),
  ('5', "fifty"),
  ('6', "sixty"),
  ('7', "seventy"),
  ('8', "eighty"),
  ('9', "ninety")
  ]
  
teens :: [(Char, String)]
teens = [
  ('0', "ten"),
  ('1', "eleven"),
  ('2', "twelve"),
  ('3', "thirteen"),
  ('4', "fourteen"),
  ('5', "fifteen"),
  ('6', "sixteen"),
  ('7', "seventeen"),
  ('8', "eighteen"),
  ('9', "nineteen")
  ]


toWords :: Int -> String
toWords n =
  -- trace ("n: " ++ show n) $
  let s = show n in
  if length s == 1 then
    fromJust $ lookup (s!!0) ones
  else if length s == 2 then
    fromJust $ toTens s
  else
    fromJust (lookup (s!!0) ones) ++ "hundred" ++
    fromMaybe "" (("and" ++) <$> toTens (tail s))
  where
    toTens :: String -> Maybe String
    toTens s =
      -- trace ("ToTens s: " ++ s) $
      if s!!0 == '0' then
        lookup (s!!1) ones
      else
      if s!!0 == '1' then
        lookup (s!!1) teens
      else
        -- trace ("tens: " ++ show (lookup (s!!0) tens)) $
        -- trace ("ones: " ++ show (lookup (s!!1) ones)) $
        Just $ fromMaybe "" (lookup (s!!0) tens) ++
        fromMaybe "" (lookup (s!!1) ones)
