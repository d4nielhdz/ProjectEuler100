main :: IO ()
main = do
  putStrLn . show $ pythagoreanProduct 1000

fst' :: (a, b, c) -> a
fst' (a, _, _) = a

snd' :: (a, b, c) -> b
snd' (_, b, _) = b

trd :: (a, b, c) -> c
trd (_, _, c) = c

pythagoreanProduct :: Int -> Int
pythagoreanProduct n = (fst' triple) * (snd' triple) * (trd triple)
  where triple = head [ (x, y, z) | z <- [1..n], y <- [1..z], x <- [1..y], x^2 + y^2 == z^2 && x + y + z == n ]