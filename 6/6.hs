import Data.List.Split
import Util
import GHC.Utils.Misc
type Fish = (Int, Int)

parse :: String -> [Fish]
parse = map (\x -> (read x,1)) . splitOn ","

sumFish :: [Fish] -> [Fish]
sumFish = sumFish' []
    where
        sumFish' :: [Fish] -> [Fish] -> [Fish]
        sumFish' acc [] = acc
        sumFish' acc (f:fs) = sumFish' ((fst f, (+) (snd f) . sum . map snd $ same):acc) diff
            where
                same = filter (\x -> fst f == fst x) fs 
                diff = filter (\x -> fst f /= fst x) fs

reproduce :: [Fish] -> [Fish]
reproduce = reproduce' []
    where
        reproduce' :: [Fish] -> [Fish] -> [Fish]
        reproduce' acc [] = acc
        reproduce' acc (f:fs)
             | fst f == 0 = reproduce' ((8,snd f):(6,snd f):acc) fs
             | otherwise  = reproduce' ((fst f - 1, snd f):acc) fs

main :: IO()
main = do
    fish <- parse <$> readFile "input.txt"
    putStrLn "part 1"
    print . sum . map snd . nTimes 80 (sumFish . reproduce) $ fish
    putStrLn "part 2"
    print . sum . map snd . nTimes 256 (sumFish . reproduce) $ fish
