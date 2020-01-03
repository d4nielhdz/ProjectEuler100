main :: IO ()
main = do
  putStrLn . show $ multOf3And5

multOf3And5 :: Int
multOf3And5 = sum $ [ x | x <- [ 1..999 ], mod x 3 == 0 || mod x 5 == 0 ]