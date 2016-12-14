module P7 where

import Data.Maybe

-- abc[hoho]xyz |-> [(abc, True), (hoho, False), (xyz, True)]
parse :: String
      -> [(String, Bool)]  -- ^ string segment, polarity
parse [] = []
parse s@(x:rest) | x == '['    = (takeWhile (/= ']') rest, False) :
                                 parse (drop 1 (dropWhile (/= ']') rest))
                 | otherwise   = (takeWhile (/= '[') s, True) :
                                 parse (dropWhile (/= '[') s)

hasABBA :: String -> Bool
hasABBA s | length s < 4  = False
          | otherwise     = isABBA (take 4 s) || hasABBA (drop 1 s)
  where
    isABBA a = ((a !! 0) == (a !! 3)) &&
               ((a !! 1) == (a !! 2)) &&
               ((a !! 0) /= (a !! 1))

getABA :: String -> [String]
getABA s | length s < 3  = []
         | otherwise     = if isABA (take 3 s) then (take 3 s) : getABA (drop 1 s)
                                               else getABA (drop 1 s)
  where
    isABA a = ((a !! 0) == (a !! 2)) &&
              ((a !! 0) /= (a !! 1))

invertABA :: String -> String
invertABA [a,b,c] = [b,a,b]
invertABA s = error $ "can't invert: " ++ s

hasTLS :: String -> Bool
hasTLS s =  (all (not . hasABBA) . map fst . filter (not . snd) . parse $ s)
         && (any (hasABBA) . map fst . filter snd . parse $ s)

--  λ hasTLS "abba[mnop]qrst"
--  True
--  λ hasTLS "abcd[bddb]xyyx"
--  False
--  λ hasTLS "aaaa[qwer]tyui"
--  False
--  λ hasTLS "ioxxoj[asdfgh]zxcvbn"
--  True

hasSSL :: String -> Bool
hasSSL s = (length abas > 0) && any (`elem` babs) abas
  where abas = concatMap getABA . map fst . filter snd $ parse s
        babs = map invertABA . concatMap getABA . map fst . filter (not . snd) $ parse s

--  λ hasSSL "aba[bab]xyz"
--  True
--  λ hasSSL "xyx[xyx]xyx"
--  False
--  λ hasSSL "aaa[kek]eke"
--  True
--  λ hasSSL "zazbz[bzb]cdb"
--  True

main = do
  input <- lines <$> readFile "input7"
  print . length . filter hasTLS $ input
  -- ans1 = 110

  print . length . filter hasSSL $ input
  -- ans2 =

