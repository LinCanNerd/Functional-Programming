import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.State
import Control.Applicative ()
--Matricola: 1994375 Can Lin

--ex1 Input/Output
charCount :: IO ()
charCount = do
    putStrLn "Enter n:"
    n <- readLn
    putStrLn "Enter the string:"
    strings <- getLine
    let new_string = take n strings
    let count = countChar new_string
    putStrLn "Character frequencies:"
    forM_ (Map.toList count) $ \(c, freq) ->
        putStrLn $ c : ": " ++ show freq

countChar :: String -> Map Char Int
countChar = foldl updateMap Map.empty
  where
    updateMap :: Map Char Int -> Char -> Map Char Int
    updateMap m c = Map.insertWith (+) c 1 m


--ex2 Nodi Equilibrati con applicativi e monadi

data BinTree a = Empty | Node a (BinTree a) (BinTree a)
  deriving (Show, Eq)

nodiEquilibrati :: (Num a, Eq a) => BinTree a -> [a]
nodiEquilibrati tree = evalState (aux tree 0) (getRootValue tree)
  where
    -- Prendere il valore root
    getRootValue :: BinTree a -> a
    getRootValue (Node value _ _) = value
    getRootValue Empty = error "Tree is empty"

    -- funzione ausiliaria per traversare il tree
    aux :: (Num a, Eq a) => BinTree a -> a -> State a [a]
    aux Empty _ = return []
    aux (Node x left right) sumToNode = do
      -- Nuova somma da passare
      let newSumToNode = sumToNode + x

      -- Ricorsione sinistra destra
      balancedLeft <- aux left newSumToNode
      balancedRight <- aux right newSumToNode

      -- Calcolo somma del subtree
      let currentSubtreeSum = subtreeSum (Node x left right)

      -- controlla se il nodo sia bilanciato
      let currentBalanced = [x | currentSubtreeSum == sumToNode]

      -- ritorna tutti i nodi bilanciati
      return (currentBalanced ++ balancedLeft ++ balancedRight)

    -- funzione per calcolare somma del subtree
    subtreeSum :: Num a => BinTree a -> a
    subtreeSum Empty = 0
    subtreeSum (Node x left right) = x + subtreeSum left + subtreeSum right
      

--ex3 Monadi/Eccezioni

data NatBin = NatBin [Int]
    deriving (Eq, Ord, Show)

--funzioni per convertire da binari a decimali e viceversa
intToNatBin :: Int -> NatBin
intToNatBin 0 = NatBin [0]
intToNatBin n = NatBin (reverse (toBits n))
  where
    toBits 0 = []
    toBits m = (m `mod` 2) : toBits (m `div` 2)

natBinToInt :: NatBin -> Int
natBinToInt (NatBin bits) = foldl (\acc bit -> acc * 2 + bit) 0 (reverse bits)


--operazioni aritmetiche
addNatBin :: NatBin -> NatBin -> NatBin
addNatBin a b = intToNatBin $ natBinToInt a + natBinToInt b

mulNatBin :: NatBin -> NatBin -> NatBin
mulNatBin a b = intToNatBin $ natBinToInt a * natBinToInt b

subNatBin :: NatBin -> NatBin -> Maybe NatBin
subNatBin a b = let diff = natBinToInt a - natBinToInt b
                in if diff < 0 then Nothing else Just (intToNatBin diff)

divNatBin :: NatBin -> NatBin -> Maybe NatBin
divNatBin _ (NatBin [0]) = Nothing -- division by zero
divNatBin a b = Just (intToNatBin $ natBinToInt a `div` natBinToInt b)

modNatBin :: NatBin -> NatBin -> Maybe NatBin
modNatBin _ (NatBin [0]) = Nothing -- modulo by zero
modNatBin a b = Just (intToNatBin $ natBinToInt a `mod` natBinToInt b)


















-- Example binary tree
exampleTree :: BinTree Int
exampleTree = Node 10 (Node 10 (Node 20 Empty Empty) Empty) (Node 10 Empty Empty)

-- Main function to run the test
main :: IO ()
main = do
  let result = intToNatBin 10
  print result