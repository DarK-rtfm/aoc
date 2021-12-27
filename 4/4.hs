import Data.List
import Control.Monad
import Data.List.Split
type Card = [[Int]]
type File = [String]

steps' :: File -> [Int]
steps'  = map read . splitOn "," . head

steps :: File -> [[Int]]
steps = tail . inits . steps'

cards :: File -> [Card]
cards = (map . map) (map read . words) . splitOn [[]] . tail . tail

winCheck :: [Int] -> Card -> Bool
winCheck nums card = any (all (`elem` nums)) rows
                  || any (all (`elem` nums)) cols
    where
        rows = card
        cols = transpose card
        diagonal1 = map (\x -> card!!x!!x) [0.. length card]
        diagonal2 = map (\x -> reverse card !!x !!x) [0.. length card]

winsCheck :: [Int] -> [Card] -> Bool
winsCheck seq= any (winCheck seq)


firstWinner :: [Card] -> [[Int]] -> (Card, [Int])
firstWinner cards seqs = (head $ filter (winCheck winseq) cards, winseq)
    where
        winseq = head $ dropWhile (\x -> not $ winsCheck x cards) seqs

pointOfCard :: Card -> [Int] -> Int
pointOfCard card seq = (*) (last seq) . sum . filter (not . (`elem` seq)) $ concat card

main :: IO()
main = do
    file <- readFile "input.txt"
    putStrLn "part 1"
    print . uncurry pointOfCard . liftM2 firstWinner cards steps . lines $ file
