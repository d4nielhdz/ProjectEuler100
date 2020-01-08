import Utils

main :: IO ()
main = do putStrLn . show $ triNumWithNFactors 100

triangulars :: [Int]
triangulars = tri' 0 0
  where  tri' x y = (x + y) : tri' (x + y) (y + 1)

factors :: Int -> [Int]
factors x = facs x x
  where
    facs _ 0 = []
    facs _ 1 = 1:[]
    facs x y = if mod x y == 0 then y:(facs x (y - 1)) else facs x (y-1)

triNumWithNFactors :: Int -> Int
triNumWithNFactors n = takeFirst (\x -> length (factors x) > n) triangulars