import Utils

main :: IO ()
main = do 
  putStrLn . show $ lPN 999

isPalindrome :: Integer -> Bool
isPalindrome x = show x == reverse (show x)

prodIsPalindrome :: Integer -> Integer -> Maybe Integer
prodIsPalindrome x y = if (isPalindrome (x * y)) then Just (x * y) else Nothing

extract :: Maybe Integer -> Integer
extract (Just x) = x
extract Nothing  = 0

fMax :: [Maybe Integer] -> Integer -> Integer
fMax [] n     = n
fMax (x:xs) n = max (extract x) (fMax xs n)

lPN :: Integer -> Integer
lPN x = fMax (ff prodIsPalindrome [x, (x - 1)..1] [x, (x - 1)..1]) 0