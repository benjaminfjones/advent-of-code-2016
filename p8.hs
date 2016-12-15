{-# LANGUAGE OverloadedStrings #-}
module P8 where

import Data.Vector (Vector, (//))
import qualified Data.Vector as V
import Data.List (intercalate, foldl')
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)
import Data.Attoparsec.ByteString.Char8
import Control.Applicative

type Grid = Vector (Vector Bool)

numGridCols = 50
numGridRows = 6

newGrid :: Grid
newGrid = V.replicate numGridRows (V.replicate numGridCols False)

drawGrid :: Grid -> String
drawGrid g = intercalate "\n" . V.toList $ fmap drawRow g
  where drawRow :: Vector Bool -> String
        drawRow = V.toList . fmap (\b -> if b then '#' else '.')

printGrid :: Grid -> IO ()
printGrid = putStrLn . drawGrid

countGrid :: Grid -> Int
countGrid g = V.sum $ fmap countRow g
  where countRow = V.sum . fmap (\b -> if b then 1 else 0)

-- operations on Grid

rect :: Int -> Int -> Grid -> Grid
rect c r g = g // [ (ri, (g V.! ri) // [ (cj, True) | cj <- [0..c-1] ])
                  | ri <- [0..r-1] ]

rotateRow :: Int -> Int -> Grid -> Grid
rotateRow y n g = g // [ (y, newRow) ]
  where newRow = V.generate numGridCols (\i -> ((g V.! y) V.! ((i-n) `mod` numGridCols)))

rotateCol :: Int -> Int -> Grid -> Grid
rotateCol x n g = g // [ (i, (g V.! i) // [ (x, (g V.! ((i-n) `mod` numGridRows)) V.! x) ])
                       | i <- [0..numGridRows-1] ]


-- Commands --------------------------------------------------------------------

data Command = Rect Int Int
             | RotateRow Int Int
             | RotateCol Int Int
  deriving (Eq, Show)

parseCmds :: ByteString -> [Command]
parseCmds b = case parseOnly cmds b of
            Left err -> error err
            Right c -> c

cmd :: Parser Command
cmd = (rectP <?> "rect") <|> (rotRowP <?> "row") <|> (rotColP <?> "col")

-- | rect 1x1
rectP :: Parser Command
rectP = do
  string "rect"
  skipMany space
  a <- decimal
  string "x"
  b <- decimal
  return $ Rect a b

-- | rotate row y=0 by 5
rotRowP :: Parser Command
rotRowP = do
  string "rotate row y="
  y <- decimal
  skipMany space >> string "by" >> skipMany space
  n <- decimal
  return $ RotateRow y n

-- | rotate column x=0 by 5
rotColP :: Parser Command
rotColP = do
  string "rotate column x="
  x <- decimal
  skipMany space >> string "by" >> skipMany space
  n <- decimal
  return $ RotateCol x n

cmds :: Parser [Command]
cmds = cmd `sepBy` endOfLine


-- MAIN ------------------------------------------------------------------------

main = do
  input <- BS.readFile "input8"
  let cs = parseCmds input
  
  let g = foldl' (\g' c -> case c of
                             Rect a b -> rect a b g'
                             RotateRow y n -> rotateRow y n g'
                             RotateCol x n -> rotateCol x n g'
                 )
                 newGrid cs
  printGrid g
  putStrLn ""
  putStrLn $ "Count = " ++ show (countGrid g)
