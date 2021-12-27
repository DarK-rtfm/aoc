import Control.Monad (liftM2)
import Text.Read (Read)
import Data.Tuple.Extra
data Action a = Forward a | Up a | Down a deriving (Show)
type Pos = (Int, Int, Int)

instance Read a => Read (Action a) where
    readsPrec _ s 
        | name == "forward" = [(Forward val, "")]
        | name == "up" = [(Up val, "")]
        | name == "down" = [(Down val, "")]
        where
            name = head . words $ s
            val = read . last . words $ s

applyAction :: Pos -> Action Int -> Pos
applyAction (x,y, aim) (Forward val) = (x+val,y + aim*val, aim)
applyAction (x,y, aim) (Up val)      = (x,y, aim-val)
applyAction (x,y, aim) (Down val)    = (x,y, aim+val)

foldAction :: Pos -> [Action Int] -> Pos
foldAction = foldl applyAction

main :: IO()
main = do
    actions <- map (read :: String -> Action Int) . lines <$> readFile "input.txt"
    let final = foldAction (0,0,0) actions
    putStrLn "part 2"
    print . liftM2 (*) fst3 snd3 $ final
