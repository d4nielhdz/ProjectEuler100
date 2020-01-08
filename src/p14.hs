main :: IO ()
main = do
  putStrLn . show $ longestCollatz 1000000

collatz :: Int -> [Int]
collatz 1 = 1:[]
collatz n = if mod n 2 == 0 then n:(collatz (div n 2)) else n:(collatz (3 * n + 1))

longestCollatz :: Int -> (Int, Int)
longestCollatz 1 = (1, 1)
longestCollatz x = 
  let (y, n) = longestCollatz (x - 1)
  in if n > length (collatz x) then (y, n) else (x, length (collatz x))