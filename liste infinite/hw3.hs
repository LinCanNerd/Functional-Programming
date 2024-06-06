<<<<<<< Updated upstream:liste infinite/hw3-LinCan-1994375.hs
--hw3 Can Lin 
=======
>>>>>>> Stashed changes:liste infinite/hw3.hs

--ex1 Insomnia

insonnia :: String
insonnia = concatMap (\n -> show n ++ " sheep ") [1..]


--ex2 Triangolo di Tartaglia
tartaglia :: [[Int]]
tartaglia = iterate nextRow [1]
  where
    nextRow row = zipWith (+) ([0] ++ row) (row ++ [0])


--ex3 Numeri fortunati
numeriFortunati :: [Int]
numeriFortunati = 1 : sieve [3,5..] 3
    where
    sieve (x:xs) n = x : sieve [y | (i,y) <- zip[n..] xs, i `mod` x /= 0] (n+1)



main :: IO ()
--main = putStrLn $ take 5 insonnia
<<<<<<< Updated upstream:liste infinite/hw3-LinCan-1994375.hs
--main = print $ take 10 tartaglia
main = print $ take 10 numeriFortunati
=======
main = print $ take 10 tartaglia
--main = print $ take 10 numeriFortunati
>>>>>>> Stashed changes:liste infinite/hw3.hs
