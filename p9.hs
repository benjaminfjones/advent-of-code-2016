module P9 where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Attoparsec.ByteString.Char8
import Control.Applicative


-- | AST node
data Node = Lit ByteString       -- ^ uncompressed character sequence
          | Mark Int ByteString  -- ^ compressed character sequence, e.g (NxM)abcde
  deriving (Eq, Show)

type AST = [Node]

ws :: Parser ()
ws = skipWhile (inClass " \n\r\t")

litP :: Parser Node
litP = Lit <$> (takeWhile1 (/= '(') <* ws)

markP :: Parser Node
markP = do
  char '('
  n <- decimal
  char 'x'
  r <- decimal
  char ')'
  s <- A.take n  -- read chars, including possible markers
  ws
  return $ Mark r s

ast :: Parser AST
ast = many (litP <|> markP)

astLength :: AST -> Int
astLength = sum . map nodeLength

nodeLength :: Node -> Int
nodeLength (Lit x) = BS.length x
nodeLength (Mark r x) = r * (bsLength x)

bsLength :: ByteString -> Int
bsLength b =
  case parseOnly ast b of
    Left err -> error err
    Right xs -> astLength xs

-- MAIN ------------------------------------------------------------------------

-- 'main' for part 2
main = do
  input <- BS.readFile "input9"
  print (bsLength input)
