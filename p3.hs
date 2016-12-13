import Data.List (transpose)

valid :: Int -> Int -> Int -> Bool
valid x y z = x+y > z &&
              x+z > y &&
              y+z > x

input :: IO [[Int]]  -- 3 Ints at a time
input = map (map (\w -> read w :: Int)) . map words . lines <$> readFile "input3"

chunk3 :: [a] -> [[a]]
chunk3 xs | length xs > 3  = take 3 xs : chunk3 (drop 3 xs)
          | otherwise      = [xs]

ans1 = do
  ns <- input
  return (length $ filter (\l -> valid (l !! 0) (l !! 1) (l !! 2)) ns)
