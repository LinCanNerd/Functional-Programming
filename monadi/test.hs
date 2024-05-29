-- Define NatBin as a new data type storing binary numbers as lists of Ints
data NatBin = NatBin [Int] deriving (Eq, Ord, Show)

maxBitLength :: Int
maxBitLength = 8  -- Define a maximum bit length for binary numbers

-- Converts an integer to NatBin, checking for overflow and negative values
intToNatBin :: Int -> Maybe NatBin
intToNatBin n
    | n < 0 = Nothing
    | length bits > maxBitLength = Nothing  -- Check for overflow
    | otherwise = Just $ NatBin bits
  where
    toBits 0 = []
    toBits m = (m `mod` 2) : toBits (m `div` 2)
    bits = reverse (toBits n)

-- Converts NatBin back to an integer   
natBinToInt :: NatBin -> Int
natBinToInt (NatBin bits) = foldl (\acc bit -> acc * 2 + bit) 0 bits

-- Define a type for operations
data Operation = Add NatBin NatBin | Sub NatBin NatBin | Mul NatBin NatBin | Div NatBin NatBin | Mod NatBin NatBin

-- Evaluate operations with error checking
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
    in if remainder == 0 then intToNatBin result else Nothing  -- Check if the division is exact
eval (Mod _ (NatBin [0])) = Nothing
eval (Mod a b) = intToNatBin $ natBinToInt a `mod` natBinToInt b

-- A function to print results of eval
printEval :: Maybe NatBin -> IO ()
printEval (Just natBin) = print natBin
printEval Nothing = putStrLn "Error"

-- Example usage:
main :: IO ()
main = do
    let a = intToNatBin 251
    let b = intToNatBin 2
    printEval $ do
        aVal <- a
        bVal <- b
        eval (Mod aVal bVal)
    print $ intToNatBin 255