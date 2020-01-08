import Utils

main :: IO ()
main = do putStrLn . show $ nthPrime 10001

nthPrime :: Int -> Integer
nthPrime n = last (take n primes)