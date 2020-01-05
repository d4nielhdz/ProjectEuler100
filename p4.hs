main :: IO ()
main = do 
  putStrLn . show $ lPN 999

isPalindrome :: Integer -> Bool
isPalindrome x = show x == reverse (show x)

prodIsPalindrome :: Integer -> Integer -> Maybe Integer
prodIsPalindrome x y = if (isPalindrome (x * y)) then Just (x * y) else Nothing

valIsJust :: Maybe a -> Bool
valIsJust (Just a) = True
valIsJust _      = False

takeFirst :: (a -> Bool) -> [a] -> a
takeFirst f (x:xs)
  | f x == True = x
  | otherwise   = takeFirst f xs

extract :: Maybe Integer -> Integer
extract (Just x) = x
extract Nothing  = 0

ff :: (Functor f) => (a -> b -> f c) -> [a] -> [b] -> [f c]
ff f xs ys  = fxs <*> ys
  where fxs = map f xs

fMax :: [Maybe Integer] -> Integer -> Integer
fMax [] n     = n
fMax (x:xs) n = max (extract x) (fMax xs n)

lPN :: Integer -> Integer
lPN x = fMax (ff prodIsPalindrome [x, (x - 1)..1] [x, (x - 1)..1]) 0