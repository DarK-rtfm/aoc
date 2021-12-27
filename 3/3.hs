import Data.List (transpose)
import Control.Monad
type Bit = Int

read':: [Char] -> [Bit]
read'= map (read . pure)

show' :: [Bit] -> [Char]
show' = concatMap show

toDec :: [Bit] -> Int
toDec = sum . zipWith (*) (map (2^) [0..]) . reverse

most :: [Bit] -> Bit
most bx
    | c0 > c1 = 0
    | c0 < c1 = 1
    | otherwise = 1
    where
        c0 = length . filter (==0) $ bx
        c1 = length . filter (==1) $ bx

gammaOf :: [[Bit]] -> [Bit]
gammaOf = map most . transpose

epsilonOf :: [[Bit]] -> [Bit]
epsilonOf = map (1-) . gammaOf

powerC :: [[Bit]] -> Int
powerC = liftM2 (*) (toDec . epsilonOf) (toDec . gammaOf)

oxygenOf :: [[Bit]] -> [Bit]
oxygenOf = oxygenOf' 0

oxygenOf' :: Int -> [[Bit]] -> [Bit]
oxygenOf' i bs
    | length bs == 1 = head bs
    | otherwise      = oxygenOf' (i+1) $ filter (\x -> gammaOf bs !!i == x!!i) bs

co2Of :: [[Bit]] -> [Bit]
co2Of = co2Of' 0

co2Of' :: Int -> [[Bit]] -> [Bit]
co2Of' i bs
    | length bs == 1 = head bs
    | otherwise      = co2Of' (i+1) $ filter (\x -> epsilonOf bs !!i == x!!i) bs

lifeS :: [[Bit]] -> Int
lifeS = liftM2 (*) (toDec . oxygenOf) (toDec . co2Of)

main :: IO()
main = do
    bytes <- map read' . lines <$> readFile "input.txt"
    putStrLn "part 1"
    print . powerC $ bytes
    putStrLn "part 2"
    print . lifeS $ bytes

