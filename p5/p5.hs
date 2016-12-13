{-# LANGUAGE OverloadedStrings #-}
module P5 where

import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)

input :: String
input = "uqwqemis"

inputs :: [String]
inputs = map (\(n,i) -> n ++ show i) $ zip (repeat input) [0..]

hasZeros :: String -> Bool
hasZeros s = take 5 s == "00000"

hasPref :: String -> String -> Bool
hasPref p s = take (length p) s == p

main = mapM_ putStrLn
     . map (\(i,h) -> i ++ "    " ++ h)
     . take 20
     . filter (hasZeros . snd)
     . map (\i -> (i, show . md5 . BS.pack $ i))
     $ inputs

--  λ mapM_ putStrLn . take 20 . filter hasZeros. map (show . md5) $ inputs
--  00000191970e97b86ecd2220e76d86b2
--  00000a1568b97dfc4736c4248df549b3
--  00000312234ca27718d52476a44c257c
--  00000064ec7123bedfc9ff00cc4f55f2
--  0000091c9c2cd243304328869af7bab2
--  0000096753dd21d352853f1d97e19d01
--  00000a220003ca08164ab5fbe0b7c08f
--  00000aaa1e7e216d6fb95a53fde7a594
--  00000a66c43cd6fa9980223accdf2cde
--  00000ac0e22f994640b38d250d1ee9c6
--  000002457920bc00c2bd4d769a3da01c
--  000002e49710aff8ed8c7b098a125cb1
--  000005074f875107f82b4ffb39a1fbf0
--  0000049d19713e17d7d93e9b1f02c856
--  000006c0b6e2bfeabd18eb400b3aecf7
--  00000667310fdb96834554e59b39ca90
--  000007d44ea65d0437b810035fec92f2
--  00000a22b84cb73c2ffe23c8bdc3bc41
--  000004b898cd6d83ede4afde86bd1fa7
--  000008969379ca9d415cce2c1a2d2755
--  (208.94 secs, 612,452,552,280 bytes)

ans1 = "1a3099aa"

-- _9______
-- _9_1____
-- 69_1____
-- 6941____
-- 6941_0__
-- 694190__
-- 694190cd

ans2 = "694190cd"

