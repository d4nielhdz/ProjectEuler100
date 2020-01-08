import Utils

main :: IO ()
main = do putStrLn . show $ triNumWithNFactors 100

triangulars :: [Int]
triangulars = tri' 0 0
  where  tri' x y = (x + y) : tri' (x + y) (y + 1)

factors :: Int -> Int -> [Int]
factors _ 0 = []
factors _ 1 = 1:[]
factors x y = if mod x y == 0 then y:(factors x (y - 1)) else factors x (y-1)

triNumWithNFactors :: Int -> Int
triNumWithNFactors n = takeFirst (\x -> length (factors x x) > n) triangulars