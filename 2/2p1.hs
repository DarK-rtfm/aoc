import Control.Monad (liftM2)
import Text.Read (Read)
data Action a = Forward a | Up a | Down a deriving (Show)
type Pos = (Int, Int)

instance Read a => Read (Action a) where
    readsPrec _ s 
        | name == "forward" = [(Forward val, "")]
        | name == "up" = [(Up val, "")]
        | name == "down" = [(Down val, "")]
        where
            name = head . words $ s
            val = read . last . words $ s

applyAction :: Pos -> Action Int -> Pos
applyAction (x,y) (Forward val) = (x+val,y)
applyAction (x,y) (Up val)      = (x,y-val)
applyAction (x,y) (Down val)    = (x,y+val)

foldAction :: Pos -> [Action Int] -> Pos
foldAction = foldl applyAction

main :: IO()
main = do
    actions <- map (read :: String -> Action Int) . lines <$> readFile "input.txt"
    let final = foldAction (0,0) actions
    putStrLn "part 1"
    print . liftM2 (*) fst snd $ final
