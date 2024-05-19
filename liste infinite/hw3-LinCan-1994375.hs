--hw3 Can Lin 

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
       

-----OPTIONAL-----

--1D Ricorsione primitiva--  
primRec :: (Int -> a -> a) -> a -> Int -> a
primRec f g 0 = g
primRec f g n = f (n-1) (primRec f g (n-1)) 

primRec2 :: (Int -> a -> a) -> a -> Int -> a
primRec2 f g n = snd $ foldl (\(i, acc) _ -> (i + 1, f i acc)) (0, g) [1..n]




main :: IO ()
--main = putStrLn $ take 5 insonnia
main = print $ take 10 tartaglia
--main = print $ take 10 numeriFortunati
