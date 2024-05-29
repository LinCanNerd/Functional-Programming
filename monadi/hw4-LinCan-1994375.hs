import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Control.Monad.State



--Matricola: 1994375 Can Lin

--ex1 Input/Output

charCount :: IO ()
charCount = do
    -- Prende n come input
    putStrLn "Enter the number of strings:"
    n <- readLn
    -- legge n stringhe
    putStrLn "Enter the strings:"
    strings <- replicateM n getLine
    -- fa il conteggio dei caratteri
    let charOccurrences = countCharsInStrings strings
    -- printa il conteggio
    mapM_ (printCount charOccurrences) ['a'..'z']
  where
    --funzione ausiliaria print
    printCount :: Map Char Int -> Char -> IO ()
    printCount occurrences char = putStrLn $ char : ": " ++ show (Map.findWithDefault 0 char occurrences)

-- Funzione per contare le occorrenze dei caratteri nelle stringhe, RICEVE SOLO CARATTERI MINUSCOLI
countCharsInStrings :: [String] -> Map Char Int
countCharsInStrings strings =
    let charSets = map Set.fromList strings
        initialMap = Map.fromList [(char, 0) | char <- ['a'..'z']]
    in foldr (Map.adjust (+1)) initialMap (concatMap Set.toList charSets)




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

data NatBin = NatBin [Int] deriving (Eq, Ord, Show)

maxBitLength :: Int
maxBitLength = 8  --Massimo numero di bit

-- Converti numero naturale a numero binario
intToNatBin :: Int -> Maybe NatBin
intToNatBin n
    | n < 0 = Nothing
    | length bits > maxBitLength = Nothing  -- Check for overflow
    | otherwise = Just $ NatBin bits
  where
    toBits 0 = []
    toBits m = (m `mod` 2) : toBits (m `div` 2)
    bits = reverse (toBits n)

-- Converti numero binario a numero naturale
natBinToInt :: NatBin -> Int
natBinToInt (NatBin bits) = foldl (\acc bit -> acc * 2 + bit) 0 bits

-- Definizione delle operazioni
data Operation = Add NatBin NatBin | Sub NatBin NatBin | Mul NatBin NatBin | Div NatBin NatBin | Mod NatBin NatBin

-- Controllo operationi con errori
eval :: Operation -> Maybe NatBin
eval (Add a b) = intToNatBin $ natBinToInt a + natBinToInt b
eval (Sub a b) = let diff = natBinToInt a - natBinToInt b in if diff < 0 then Nothing else intToNatBin diff
eval (Mul a b) = intToNatBin $ natBinToInt a * natBinToInt b
eval (Div _ (NatBin [0])) = Nothing
eval (Div a b) = 
    let dividend = natBinToInt a
        divisor = natBinToInt b
        result = dividend `div` divisor
        remainder = dividend `mod` divisor
    in if remainder == 0 then intToNatBin result else Nothing  --Funziona solo per divisioni esatte, visto che il dominio è naturale
eval (Mod _ (NatBin [0])) = Nothing
eval (Mod a b) = intToNatBin $ natBinToInt a `mod` natBinToInt b

-- funzione per stampare il risultato di eval
printEval :: Maybe NatBin -> IO ()
printEval (Just natBin) = print natBin
printEval Nothing = putStrLn "Error"




--Esempio tree
exampleTree :: BinTree Int
exampleTree = Node 10 (Node 10 (Node 20 Empty Empty) Empty) (Node 10 Empty Empty)

-- TEST
main :: IO ()
main = do
    let a = intToNatBin 251
    let b = intToNatBin (-1)
    printEval $ do
        aVal <- a
        bVal <- b
        eval (Add aVal bVal)
    print $ intToNatBin 255
