import Utils

main :: IO ()
main = do
  putStrLn . show $ minDivUpTo 20

minDivUpTo :: Integer -> Integer
minDivUpTo x = takeFirst (\n -> all (\m -> mod n m == 0 && n /= 0) [1..x]) [0, x..]