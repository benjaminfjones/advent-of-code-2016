import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (sortBy)
import Data.Ord
import Data.Char (ord, chr)

mySort :: (Char, Int) -> (Char, Int) -> Ordering
mySort (a, i) (b, j) =
  if i > j then LT
           else if i < j then GT
                         else compare a b

countLetters :: String -> Map Char Int
countLetters s = countLetters' s Map.empty
  where
    countLetters' [] m = m
    countLetters' (c:rest) m = let m' = case Map.lookup c m of
                                          Nothing -> Map.insert c 1 m
                                          Just k  -> Map.insert c (k+1) m
                               in countLetters' rest m'

checksum :: String -> String
checksum s = take 5 cs
  where cs = map fst . sortBy mySort . Map.toList $ countLetters s

isValidRoom :: (String, Int, String) -> Bool
isValidRoom (name, _, chk) = checksum name == chk



-- Input -----------------------------------------------------------------------

input :: IO [(String, Int, String)]
input = map (wsToTuple . words) . lines <$> readFile "input4"
  where wsToTuple ws = (ws !! 0, read (ws !! 1) :: Int, ws !! 2)

ans1 = do
  inp <- input
  return $ sum (map (\(_, d, _) -> d) (filter isValidRoom inp))

ans2 = do
  inp <- input
  map (\(name, idee, _) -> (idee, rotN idee name)) inp
  -- look for "northpole"


-- Crypto! ---------------------------------------------------------------------


rotN :: Int -> String -> String
rotN n = unwords . map rot . words
  where rot = decode . (rN n) . encode
rot13 = rotN 13

rN :: Int -> [Int] -> [Int]
rN n = map $ (`mod` 26) . (+ n)
r13 = rN 13

encode :: String -> [Int]
encode = map (\c -> ord c - ord 'a')

decode :: [Int] -> String
decode = map (\x -> chr $ x + ord 'a')
