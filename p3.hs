main :: IO ()
main = do
  putStrLn . show $ lPF 600851475143 2 1

-- Implementation taken from https://math.stackexchange.com/questions/389675/largest-prime-factor-of-600851475143
lPF :: Integer -> Integer -> Integer -> Integer
lPF x y z
  | x == 1 = z
  | mod x y == 0 = lPF (div x y) 2 (max y z)
  | otherwise = lPF x (y+1) z