import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad (forM_)
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



--ex3 Monadi/Eccezioni

Nat

main :: IO ()
main = charCount