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
nodiEquilibrati tree = evalState (aux tree) 0
  where
    -- Funzione ausiliaria che usa lo stato per mantenere la somma dal nodo radice
    aux :: (Num a, Eq a) => BinTree a -> State a [a]
    aux Empty = return []
    aux (Node x left right) = do
      -- Ottieni la somma dal nodo radice
      sumFromRoot <- get
      -- Aggiungi il valore del nodo corrente alla somma
      let newSumFromRoot = sumFromRoot + x
      -- Calcola i risultati per i sottoalberi
      put newSumFromRoot
      balancedLeft <- aux left
      balancedRight <- aux right
      -- Calcola la somma del sottoalbero corrente
      let currentSum = sum balancedLeft + sum balancedRight + x
      -- Verifica se il nodo corrente è equilibrato
      let currentBalanced = ([x | sumFromRoot == currentSum])
      -- Ripristina la somma dal nodo radice
      put sumFromRoot
      return (currentBalanced ++ balancedLeft ++ balancedRight)
      

--ex3 Monadi/Eccezioni



main :: IO ()
main = charCount