-- Homework 1, Lin Can  ID 1994375



--1.1   Definire MytakeWhile e MyDropWhile 
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p (x:xs) = if p x then x:myTakeWhile p xs else []

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile p (x:xs) = if not(p x) then x:myDropWhile p xs else []


--1.2   Definire MyRemoveDupsOrd 
myRemoveDupsOrd :: Ord a => [a]->[a]
myRemoveDupsOrd [] = []
myRemoveDupsOrd [x] = [x]
myRemoveDupsOrd (x:y:xs) | x == y = myRemoveDupsOrd (y:xs)
                         | otherwise = x : myRemoveDupsOrd (y:xs)


--1.3 myRemoveDups in O(nlogn)
{-
    qSortTuples1 e qSortTuples2 sono due funzioni di ordinamento che ordinano una lista di tuple in base al primo e secondo elemento rispettivamente.
    e tutti e due sono in O(nlogn) perche' usano il quicksort.
    myRemoveDupsOrdTuple e' una funzione che rimuove i duplicati da una lista di tuple ordinate in base al primo elemento ed e' in O(n) perche' scorre la lista una volta sola.
    map fst pure le prime componenti delle tuple in modo da ottenere la lista di elementi originali ed è in O(n).
    quindi la complessità totale è O(nlogn)+O(nlogn)+O(n)+O(n) = O(nlogn)
-}
qSortTuples1 :: Ord a => [(a, b)] -> [(a, b)]
qSortTuples1 [] = []
qSortTuples1 ((x, y):xs) = qSortTuples1 smaller ++ [(x, y)] ++ qSortTuples1 larger
    where
        smaller = [(a, b) | (a, b) <- xs, a <= x]
        larger = [(c, d) | (c, d) <- xs, c > x]

qSortTuples2 :: Ord a => [(b, a)] -> [(b, a)]
qSortTuples2 [] = []
qSortTuples2 ((x, y):xs) = qSortTuples2 smaller ++ [(x, y)] ++ qSortTuples2 larger
    where
        smaller = [(a, b) | (a, b) <- xs, b < y]
        larger = [(c, d) | (c, d) <- xs, d >= y]

myRemoveDupsOrdTuple :: Ord a => [(a, b)] -> [(a, b)]
myRemoveDupsOrdTuple [] = []
myRemoveDupsOrdTuple [x] = [x]
myRemoveDupsOrdTuple ((a, b):(c, d):xs)
  | a == c    = myRemoveDupsOrdTuple ((c, d) : xs)
  | otherwise = (a, b) : myRemoveDupsOrdTuple ((c, d) : xs)

myRemoveDups :: Ord a => [a] -> [a]
myRemoveDups [] = []
myRemoveDups xs = map fst (qSortTuples2(myRemoveDupsOrdTuple (qSortTuples1 (zip xs [0..]))))


--2.1 Definire il funzionale zipWith f xs ys senza decomporre liste, ma usando un’espressione che contenga zapp, f ed eventualmente xs e ys
zapp :: [a -> b] -> [a] -> [b]
zapp (f:fs)(x:xs)= f x : zapp fs xs
zapp _ _ = []

myzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myzipWith f xs ys = zapp (map f xs ) ys

--2.2 zipWith con zip
myzipWith2 :: (a -> b -> c) -> [a] -> [b] -> [c]
myzipWith2 f [] _ = []
myzipWith2 f (x:xs) (y:ys) = f x y : myzipWith2 f xs ys


--2.3 map con foldr e foldl
myMapR :: (a -> b) -> [a] -> [b]
myMapR f xs = foldr (\x acc -> f x : acc) [] xs

myMapL :: (a -> b) -> [a] -> [b]
myMapL f xs = reverse(foldl (\acc x -> f x : acc) [] xs)

{-2.4 Non possiamo usare foldr e foldl per implementare map perchè
foldr e foldl sono funzioni che operano su liste e restituiscono un valore scalare,
mentre map è una funzione che opera su liste e restituisce una lista.
-}



--3.1
myPrefissi :: [a] -> [[a]]
myPrefissi [] = [[]]
myPrefissi (x:xs) = [] : map (x:) (myPrefissi xs) 


--3.2 
mySuffissi :: [a] -> [[a]]
mySuffissi [] = [[]]
mySuffissi xs = xs : mySuffissi (tail xs)

mySegmenti :: [a] -> [[a]]
mySegmenti xs = mySuffissi xs ++ myPrefissi xs

mySegSomma :: (Num a, Eq a) => [a] -> a -> [[a]]
mySegSomma [] _ = []
mySegSomma xs n = ys where
    ys = mySegmenti xs
    zs = [y | y <- ys , sum y == n]

--3.3
--funzione per prendere tutte le sottoliste di una lista
sottoliste :: [a] -> [[a]]
sottoliste [] = [[]]
sottoliste (x:xs) = [x:ys | ys <- sottoliste xs] ++ sottoliste xs

sublSommaS :: (Num a, Eq a) => [a] -> a -> [[a]]
sublSommaS [] _ = []
sublSommaS xs n = zs where
    ys = sottoliste xs
    zs = [y | y <- ys , sum y == n]






main :: IO ()
main = do
    let inputList = [1,2,3,2,4]
    let list2 = [1,2,3,4,5,6,7,8,9,10]
    let word = "qqqqaaaa"
    let result = myDropWhile (>3) inputList
    putStrLn ("Input List: " ++ show inputList)
    putStrLn ("Result after removing duplicates: " ++ show result)

