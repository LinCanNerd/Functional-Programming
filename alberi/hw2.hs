-- Homework 2, Lin Can  ID 1994375

--Ex1 definire il mergeSort iterativo
--1.1

-- Funzione per unire due liste ordinate in una singola lista ordinata
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

singleton :: [a] -> [[a]]
singleton = map(:[])

-- Fonde coppie di liste in una lista di liste
mergePairs :: Ord a => [[a]] -> [[a]]
mergePairs [] = []  -- Se la lista è vuota, restituisci una lista vuota 
mergePairs [x] = [x]  -- Se c'è solo una lista, restituiscila come è
mergePairs (a:b:xs) = merge a b : mergePairs xs  -- Fonde coppie di liste e procede con il resto


-- La funzione 'iterativeMergeSort' 
iterativeMergeSort1 :: Ord a => [a] -> [a]
iterativeMergeSort1 = head . until singleList mergePairs . singleton
  where
    singleList :: [[a]] -> Bool
    singleList [_] = True
    singleList _   = False


--1.2 divide una lista in subliste dove ogni sublista appende solo elementi maggiori all'elemento precedente
divide :: Ord a => [a] -> [[a]]
divide = foldr f []
  where
    f x [] = [[x]]
    f x (y:ys) | x < head y = (x:y):ys
               | otherwise = [x]:y:ys

-- questa funzione trae vantaggio se la lista è già ordinata, o almeno parzialmente ordinata
iterativeMergeSort2 :: Ord a => [a] -> [a]
iterativeMergeSort2 = head . until singleList mergePairs . divide
  where
    singleList :: [[a]] -> Bool
    singleList [_] = True
    singleList _   = False




--2.1 uso BinTree1 e Bintree2 perchè l'accento mi da errore
data BinTree1 a = Node1 a (BinTree1 a) (BinTree1 a) | Empty
data BinTree2 a = Node2 (BinTree2 a) (BinTree2 a) | Leaf a


--mapBT1
mapBT1 :: (a -> b) -> BinTree1 a -> BinTree1 b
mapBT1 _ Empty = Empty
mapBT1 f (Node1 x l r) = Node1 (f x) (mapBT1 f l) (mapBT1 f r)

--mapBT2
mapBT2 :: (a -> b) -> BinTree2 a -> BinTree2 b
mapBT2 f (Leaf x) = Leaf (f x)
mapBT2 f (Node2 l r) = Node2 (mapBT2 f l) (mapBT2 f r)

--foldrBT1
foldrBT1 :: (a -> b -> b -> b) -> b -> BinTree1 a -> b
foldrBT1 _ z Empty = z
foldrBT1 f z (Node1 x l r) = f x (foldrBT1 f z l) (foldrBT1 f z r)

--foldrBT2
foldrBT :: (a -> b -> b) -> b -> BinTree2 a -> b
foldrBT f z (Leaf x) = f x z
foldrBT f z (Node2 l r) = foldrBT f (foldrBT f z r) l





main :: IO ()
main = do
    let inputList = [5,4,6,87,11,2,6,55,4]
    let result = iterativeMergeSort2 inputList
    putStrLn ("Input List: " ++ show inputList)
    putStrLn ("Result : " ++ show result)


