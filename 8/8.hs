import Data.List
import Data.Tuple
import Data.List.Split
import GHC.Utils.Misc
import Data.Maybe
type Input = [String]
type Output = [String]
type Line = (Input, Output)
type Lookup = [(String, String)]

parseLine :: String -> Line
parseLine = last2 . map words . splitOn " | "

-- Static lookup tables
segmentLookup :: [(Int, String)]
segmentLookup = [ (0, "abcefg" )
                , (1, "cf"     )
                , (2, "acdeg"  )
                , (3, "acdfg"  )
                , (4, "bcdf"   )
                , (5, "abdfg"  )
                , (6, "abdefg" )
                , (7, "acf"    )
                , (8, "abcdefg")
                , (9, "abcdfg" )
                ]

numLookup :: [(String, Int)]
numLookup = map swap segmentLookup

-- Converts that use the static "correct" table
segmentsOfNum :: Int -> String
segmentsOfNum = fromJust . (`lookup` segmentLookup)

numOfSegments :: String -> Int
numOfSegments = fromJust . (`lookup` numLookup) . sort

lenOfNum :: Int -> Int
lenOfNum = length . segmentsOfNum

-- generate lookup table between wrong and correct segments
genLookup :: Input -> Lookup
genLookup i = map (\x -> (sort $ getraw x, sort . fromJust $ lookup x segmentLookup)) [0..9]
    where
        getraw :: Int -> String
        getraw 0 = only $ i `thatHasLenAs` 0 `thatNotInside` getraw 9 `thatNotInside` getraw 6
        getraw 1 = only $ i `thatHasLenAs` 1
        getraw 2 = only $ i `thatHasLenAs` 2 `thatNotExtends` getraw 1 `thatNotInside` getraw 6
        getraw 3 = only $ i `thatHasLenAs` 3 `thatExtends` getraw 1
        getraw 4 = only $ i `thatHasLenAs` 4
        getraw 5 = only $ i `thatHasLenAs` 5 `thatNotExtends` getraw 1 `thatInside` getraw 6
        getraw 6 = only $ i `thatHasLenAs` 6 `thatNotExtends` getraw 1
        getraw 7 = only $ i `thatHasLenAs` 7
        getraw 8 = only $ i `thatHasLenAs` 8
        getraw 9 = only $ i `thatHasLenAs` 9 `thatExtends` getraw 4

        thatHasLenAs sx i = filter (\x -> lenOfNum i == length x) sx
        infixl 8 `thatHasLenAs`
        thatExtends c s = filter (\x -> all (`elem` x) s) c
        infixl 7 `thatExtends`
        thatNotExtends c s = filter (\x -> any (`notElem` x) s) c
        infixl 7 `thatNotExtends`
        thatInside c s = filter (all (`elem` s)) c
        infixl 7 `thatInside`
        thatNotInside c s = filter (any (`notElem` s)) c
        infixl 7 `thatNotInside`

-- Convert output into digits or single num
convertToDigits :: Line -> [Int]
convertToDigits l = map (numOfSegments . (\x -> fromJust $ sort x `lookup` genLookup (fst l))) (snd l)

convertToInt :: Line -> Int
convertToInt l = sum $ zipWith (\x y -> x*(10^y)) (reverse $ convertToDigits l) [0..]

-- io
main :: IO()
main = do
    datas <- map parseLine . lines <$> readFile "input.txt"
    let p1 = length . filter (`elem` [1,4,7,8]) $ concatMap convertToDigits datas
    let p2 = sum $ map convertToInt datas
    putStrLn "part 1"
    print p1
    putStrLn "part 2"
    print p2
    return ()

