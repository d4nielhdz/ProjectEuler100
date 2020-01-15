import Data.List

main :: IO ()
main = do
  putStrLn . show $ numOfWordsTillN 1000

numOfWordsTillN :: Int -> Int
numOfWordsTillN n = foldl' (+) 0 (map (\x -> length $ filter (/= ' ') x) (map numToWord [1..n]))

numToWord :: Int -> String
numToWord n
  | div n 1000 == 1   = "one thousand"
  | div n 100 /= 0    = if mod n 100 == 0 then ones !! (div n 100) ++ " hundred" else ones !! (div n 100) ++ " hundred and " ++ numToWord (mod n 100)
  | n >= 10 && n < 20 = tens !! (mod n 10)
  | div n 10 /= 0     = tenths !! ((div n 10) - 2) ++ " " ++ numToWord (mod n 10)
  | otherwise         = ones !! n
    where 
      ones = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
      tenths = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
      tens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
  
