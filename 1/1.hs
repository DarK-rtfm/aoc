countIncreasing :: (Ord a, Num a) => [a] -> Int
countIncreasing a = length $ filter (>0) $ zipWith (-) (tail a) a 

frameSums :: Num a => [a] -> [a]
frameSums a = zipWith3 (\x y z -> x + y + z) a (tail a) (tail . tail $ a)

main = do
    depths <- map read . lines <$> readFile "input.txt"
    putStrLn "part 1"
    print . countIncreasing $ depths
    putStrLn "part 2"
    print . countIncreasing . frameSums $ depths
