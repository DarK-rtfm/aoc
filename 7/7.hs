import Data.List.Split
import Data.List
import Numeric.Statistics.Median
import GHC.Num
type Crab = Int

parse :: String -> [Crab]
parse = map read . splitOn ","

fuelNeeded :: (Int -> Int -> Int) -> [Crab] -> Int
fuelNeeded f crabs =  minimum . map (\x -> sum $ map (f x) crabs) $ [0.. maximum crabs]

p1distance a b = abs $ a - b
p2distance a b = abs $ (a-b)*(abs (a-b) + 1) `div` 2

main = do
    crabs <- parse <$> readFile "input.txt"
    putStrLn "part 1"
    print $ fuelNeeded p1distance crabs
    putStrLn "part 2"
    print $ fuelNeeded p2distance crabs
