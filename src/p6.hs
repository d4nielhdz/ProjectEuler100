main :: IO ()
main = do
  putStrLn . show $ sumSqDiff 100

sumSqDiff :: Integer -> Integer
sumSqDiff x = (sum [1..x])^2 - (sum (map (^2) [1..x]))