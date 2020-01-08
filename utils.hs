
module Utils
( ff
, primes
, takeFirst
) where
  
import qualified Data.Set as PQ

takeFirst :: (a -> Bool) -> [a] -> a
takeFirst f (x:xs)
  | f x == True = x
  | otherwise   = takeFirst f xs

-- This sieve implementation is discussed by Melissa E. O’Neill in her paper
-- https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
primes :: [Integer]
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

ff :: (Functor f) => (a -> b -> f c) -> [a] -> [b] -> [f c]
ff f xs ys  = fxs <*> ys
  where fxs = map f xs
