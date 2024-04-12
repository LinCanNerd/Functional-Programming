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
foldrBT1 :: (a -> b -> b) -> b -> BinTree1 a -> b
foldrBT1 _ z Empty = z
foldrBT1 f z (Node1 x l r) = foldrBT1 f (f x (foldrBT1 f z r)) l

--foldrBT2
foldrBT2 :: (a -> b -> b) -> b -> BinTree2 a -> b
foldrBT2 f z (Leaf x) = f x z
foldrBT2 f z (Node2 l r) = foldrBT2 f (foldrBT2 f z r) l

--foldlBT1
foldlBT1 :: (b -> a -> a -> b) -> b -> BinTree1 a -> b
foldlBT1 _ z Empty = z



main = do
    let myTree :: BinTree1 Int
        myTree = Node1 10
                    (Node1 5
                        (Node1 2 Empty Empty)
                        (Node1 7 Empty Empty)
                    )
                    (Node1 15
                        (Node1 12 Empty Empty)
                        (Node1 18 Empty Empty)
                    )
    let treeSum = foldrBT1 (+) 0 myTree
    print treeSum