fib :: Int -> Int
fib n = fst (fibAux n)
  where
    fibAux 0 = (0, 1)
    fibAux 1 = (1, 1)
    fibAux n = let (a, b) = fibAux (n - 1)
               in (b, a + b)

main :: IO ()
main = do
  putStrLn "Enter a number to calculate the Fibonacci number:"
  number <- getLine
  let n = read number :: Int
  print (fib n)
