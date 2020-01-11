import qualified Data.List as L

main :: IO ()
main = do
  putStrLn . show $ digSum (2 ^ 1000)

digSum :: Integer -> Integer
digSum n = L.foldl' (+) 0 (map (\x -> (read :: String -> Integer) [x]) (show n))
