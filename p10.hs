import Utils

main :: IO ()
main = do putStrLn . show $ sumOfPrimesBelowN 2000000

sumOfPrimesBelowN :: Integer -> Integer
sumOfPrimesBelowN n = sum (takeWhile (\x -> x < n) primes)