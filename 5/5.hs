{-# LANGUAGE TupleSections #-}
import Text.XML.HXT.DOM.Util
import Data.List.Split
import GHC.Utils.Misc
type LineEnds = ((Int,Int),(Int,Int))
type Points = [(Int,Int)]
type Field = [(Int, (Int, Int))]

parse :: String -> [LineEnds]
parse = map (last2 . map (last2 . map read . splitOn ",") . splitOn " -> ") . lines

filterNotStr :: [LineEnds] -> [LineEnds]
filterNotStr = filter (\((x1, y1), (x2, y2)) -> (x1 == x2) || (y1 == y2))

filterNotDiag :: [LineEnds] -> [LineEnds]
filterNotDiag = filter (\((x1, y1), (x2, y2)) -> (x1 == x2) || (y1 == y2) || (abs(x2-x1) == abs(y2-y1)))

genLine :: LineEnds -> Points
genLine ((x1,y1),(x2,y2))
    | y1 == y2               = zip xrange (repeat y1)
    | x1 == x2               = zip (repeat x1) yrange
    | abs(x2-x1)==abs(y2-y1) = zip xrange yrange
    where 
        xrange = (\x -> if x2 > x1 then x else reverse x) [min x1 x2 .. max x1 x2]
        yrange = (\y -> if y2 > y1 then y else reverse y) [min y1 y2 .. max y1 y2]

linesToPoints :: [LineEnds] -> [(Int, Int)]
linesToPoints = concatMap genLine

main :: IO()
main = do
    lineends <- parse <$> readFile "input.txt"
    putStrLn "Hand on! It'll be slooooooow~"
    putStrLn "part 1"
    print . length . doubles . linesToPoints . filterNotStr $ lineends
    putStrLn "part 2"
    print . length . doubles . linesToPoints . filterNotDiag $ lineends
