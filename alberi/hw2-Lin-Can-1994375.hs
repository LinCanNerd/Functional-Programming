import Debug.Trace
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
singleton = map (:[])

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




--2.1 uso BinTree1 e Bintree2 perchè l'apostrofo mi da errore
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
foldlBT1 :: (a -> b -> b) -> b -> BinTree1 a -> b
foldlBT1 _ z Empty = z
foldlBT1 f z (Node1 x l r) = foldlBT1 f (f x (foldrBT1 f z l)) r

--foldlBT2
foldlBT2 :: (a -> b -> b) -> b -> BinTree2 a -> b
foldlBT2 f z (Leaf x) = f x z
foldlBT2 f z (Node2 l r) = foldlBT2 f (foldlBT2 f z l) r


--2.2

--non sono riuscito a fare le funzioni per BT2

--numero dei nodi di un albero binario
countNodesBT1 :: BinTree1 a -> Int
countNodesBT1 = foldrBT1 (\_ acc -> 1 + acc) 0


--altezza dell'albero binario
foldrHBT1 :: (a -> b -> b -> b) -> b -> BinTree1 a -> b
foldrHBT1 _ z Empty = z
foldrHBT1 f z (Node1 x l r) = f x (foldrHBT1 f z r) (foldrHBT1 f z l)


heightBT1 :: BinTree1 a -> Int
heightBT1 = foldrHBT1 (\_ l r -> 1 + max l r) 0

-- massimo sbilanciamento tra i sottoalberi sinistro e destro
max_sbilanciamentoBT1 :: BinTree1 a -> Int
max_sbilanciamentoBT1 (Node1 _ l r) = abs (heightBT1 l - heightBT1 r)



--FACOLTATIVO--
data Tree a = R a [Tree a]

mapT :: (a -> b) -> Tree a -> Tree b
mapT f (R x ts) = R (f x) (map (mapT f) ts)

foldrT :: (a -> b -> b) -> b -> Tree a -> b
foldrT f z (R x ts) = f x (foldr (flip (foldrT f)) z ts)

foldlT :: (b -> a -> b) -> b -> Tree a -> b
foldlT f z (R x ts) = foldl (foldlT f) (f z x) ts
--FACOLTATIVO--


--3 Nodi Equilibrati
--La complessità della funzione è in O(n), perchè attraversa ogni nodo due volte,
--una volta per andare fino in fondo e calcolare la somma dal root a ogni nodo,e una volta per salire
-- e confrontare la somma dei subtree con la somma del root

nodiEquilibrati :: (Num a, Eq a) => BinTree1 a -> [a]
nodiEquilibrati tree = snd $ aux tree 0
  where
    -- La funzione ausiliaria ritorna una tupla (somma sottoalbero, lista di nodi equilibrati)
    aux :: (Num a, Eq a) => BinTree1 a -> a -> (a, [a])
    aux Empty _ = (0, [])
    aux (Node1 x left right) sumFromRoot = (currentSum, result)
      where
        (leftSum, balancedLeft) = aux left (sumFromRoot + x)
        (rightSum, balancedRight) = aux right (sumFromRoot + x)
        currentSum = leftSum + rightSum + x
        -- Controlla se il nodo corrente è equilibrato
        currentBalanced = [x | sumFromRoot == currentSum] 
        result = currentBalanced ++ balancedLeft ++ balancedRight






--4 Alberi Binari di Ricerca
-- la complessità di listToABR è di O(nlogn) perchè il sort richiede O(nlogn)
-- e la costruzione dell'albero richiede O(n), se la lista è già ordinata la complessità è O(n)
listToABR :: Ord a => [a] -> BinTree1 a
listToABR = aux . iterativeMergeSort2 
 where aux :: Ord a => [a] -> BinTree1 a
       aux [] = Empty
       aux xs = Node1 (xs !! mid) (aux left) (aux right)
         where mid = length xs `div` 2
               left = take mid xs
               right = drop (mid + 1) xs



--5 scanr lineare
--reverse lineare
myReverse:: [a] -> [a]
myReverse xs = reverseEff xs [] where
  reverseEff [] acc = acc
  reverseEff (x:xs) acc = reverseEff xs (x:acc)

--per una lista di lunghezza n, la complessità di reverse è O(n)
--e la complessità di myScanl è O(n),
--quindi la complessità di myScanr è O(n)+O(n)+O(n) = 3*O(n) = O(n)
myScanr :: (a -> b -> b) -> b -> [a] -> [b]
myScanr f e xs = myReverse (myScanl f  e (myReverse xs))where
  myScanl f e [] = [e]
  myScanl f e (x:xs) = e : myScanl f (f x e) xs




--TEST--
instance (Show a) => Show (BinTree1 a) where
    show :: Show a => BinTree1 a -> String
    show Empty = "Empty"
    show (Node1 x l r) = "Node1 (" ++ show x ++ ") (" ++ show l ++ ") (" ++ show r ++ ")"

instance (Show a) => Show (BinTree2 a) where
    show :: Show a => BinTree2 a -> String
    show (Leaf x) = "Leaf (" ++ show x ++ ")"
    show (Node2 l r) = "Node2 (" ++ show l ++ ") (" ++ show r ++ ")"

instance (Show a ) => Show (Tree a) where
    show :: Show a => Tree a -> String
    show (R x ts) = "R " ++ show x ++ "  [" ++ show ts ++ "]"

main = do
  let myTree1 :: BinTree1 Int
      myTree1 = Node1 1
                  (Node1 2
                      (Node1 1 (Node1 2 Empty Empty) Empty)
                      (Node1 3 Empty Empty)
                  )
                  (Node1 5
                      (Node1 6 Empty Empty)
                      (Node1 6 Empty Empty)
                  )


  let myTree2 :: BinTree2 Int
      myTree2 = Node2
                  (Node2
                      (Leaf 0)
                      (Leaf 2)
                  )
                  (Node2
                      (Leaf 3)
                      (Leaf 4)
                  )
  
  let myBtree :: BinTree1 Int
      myBtree = Node1 1
                  (Node1 2
                      (Node1 3 Empty Empty)
                      (Node1 0 Empty Empty)
                  )
                  (Node1 5
                      (Node1 6 Empty Empty)
                      (Node1 7 Empty Empty)
                  )

  let myT :: Tree Int
      myT = R 4 [ R 2 [R 3 [], R 4 [], R 5 []]]

  let mylist = [5,3,4,2,1,8,0]
  let result = foldrT (-) 0 myT 
  print result