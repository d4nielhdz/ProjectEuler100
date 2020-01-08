-- d4nielhdz
main :: IO ()
main = do
  putStrLn . show $ filterFib 4000000

filterFib :: Int -> Int
filterFib n = sum $ filter even (fib 0 1 n)

fib :: Int -> Int -> Int -> [Int]
fib a b c
  | a == 0 && b == 1 = 1:(fib 1 1 c)
  | a == 1 && b == 1 = 1:(fib 1 2 c)
  | b > c            = []
  | otherwise        = (b):(fib b (a + b) c)