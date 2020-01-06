main = do
  putStrLn . show $ minDivUpTo 20

takeFirst :: (a -> Bool) -> [a] -> a
takeFirst f (x:xs)
  | f x == True = x
  | otherwise   = takeFirst f xs


minDivUpTo :: Integer -> Integer
minDivUpTo x = takeFirst (\n -> all (\m -> mod n m == 0 && n /= 0) [1..x]) [1, x..]