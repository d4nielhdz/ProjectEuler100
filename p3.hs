import qualified Data.Set as PQ

main :: IO ()
main = do
  putStrLn . show $ largestPrimeFactor 600851475143
  
largestPrimeFactor :: Int -> Int
largestPrimeFactor x = takeFirst (\n -> mod x n == 0) (reverse $ primesUpToN x)

takeFirst :: (a -> Bool) -> [a] -> a
takeFirst f (x:xs)
  | f x == True = x
  | otherwise   = takeFirst f xs

-- This function was written by Melissa E. O’Neill. Check out her
-- paper about it at https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
primes :: [Int]
primes = 2:sieve [3,5..]
  where
    sieve (x:xs) = x : sieve' xs (insertprime x xs PQ.empty)

    sieve' (x:xs) table
        | nextComposite == x = sieve' xs (adjust x table)
        | otherwise          = x : sieve' xs (insertprime x xs table)
      where 
        (nextComposite,_) = PQ.findMin table

    adjust x table
        | n == x    = adjust x (PQ.insert (n', ns) newPQ)
        | otherwise = table
      where
        Just ((n, n':ns), newPQ) = PQ.minView table

    insertprime p xs = PQ.insert (p*p, map (*p) xs)

primesUpToN :: Int -> [Int]
primesUpToN n = takeWhile (\x -> x <= n) primes