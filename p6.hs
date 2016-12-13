module P6 where

import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Ord


toFreqMap :: Ord a => [a] -> Map a Int
toFreqMap = foldl' f Map.empty
  where f :: Ord a => Map a Int -> a -> Map a Int
        f m k = case Map.lookup k m of
                  Nothing -> Map.insert k 1 m
                  Just c  -> Map.insert k (c+1) m

main = do
  input <- lines <$> readFile "input6"
  let cs = transpose input
      ms = map (Map.toList . toFreqMap) cs
      ss sel = map (sel . sortBy (comparing snd)) ms

  -- Part 1
  mapM_ print (ss last)

  -- Part 2
  mapM_ print (ss head)
