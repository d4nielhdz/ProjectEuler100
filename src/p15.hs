main :: IO ()
main = do
  putStrLn . show $ numOfPaths 20

fac :: Integer -> Integer
fac 1 = 1
fac n = n * (fac (n - 1))

numOfPaths :: Integer -> Integer
numOfPaths n = div (fac (2 * n)) ((fac n) * (fac n))