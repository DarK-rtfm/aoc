import Data.List
import Data.Maybe
import Control.Monad
import Data.List.Split
import Distribution.Simple.Utils
import Data.Bifunctor
type Card = [[Int]]
type Calls = [Int]
type File = String
type Win = (Card, Calls)
type MaybeWin = (Card, Maybe Calls)

-- PARSING

steps :: File -> [Calls]
steps = tail . inits . steps' 
    where
        steps' :: File -> Calls
        steps'  = map read . splitOn "," . head . lines

cards :: File -> [Card]
cards = (map . map) (map read . words) . splitOn [[]] . tail . tail . lines

-- CHECKING IF CARD WINS
winCheck :: Card -> Calls -> Bool
winCheck card nums = any (all (`elem` nums)) rows
                  || any (all (`elem` nums)) cols
    where
        rows = card
        cols = transpose card

-- CHECKING THE REQUIRED SEQUENCE OF CALLS FOR A CARD TO WIN, RETURN (Card, Nothing) IF IT NEVER WINS
winsIn :: Card -> [Calls] -> MaybeWin
winsIn card sx = (card , safeHead $ dropWhile (not . winCheck card) sx)

-- FILTERING OUT ALL THE NON WINNER CARDS, SAYING WINNERS WITH THEIR SEQUENCE
winners :: [Card] -> [Calls] -> [Win]
winners cards sx = onlyWins $ map (`winsIn` sx) cards
    where onlyWins = map (second fromJust). filter (isJust . snd)

-- SORT BASED ON THE LENGTH OF SEQUENCE NEEDED TO WIN
sortWinners :: [Win] -> [Win]
sortWinners = sortOn (length . snd)

-- CALCULATE POINTS
pointOfCard :: Card -> Calls -> Int
pointOfCard card seq = (*) (last seq) . sum . filter (not . (`elem` seq)) $ concat card

main :: IO()
main = do
    wins <- sortWinners . liftM2 winners cards steps <$> readFile "input.txt"
    putStrLn "part 1"
    print . uncurry pointOfCard . head $ wins
    putStrLn "part 2"
    print . uncurry pointOfCard . last $ wins
