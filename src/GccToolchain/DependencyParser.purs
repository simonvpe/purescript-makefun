module GccToolchain.DependencyParser where

import Data.Array (filter, many)
import Data.Either (Either(..))
import Data.List (toUnfoldable)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..))
import Node.Path (FilePath)
import Prelude (bind, pure, show, (#), ($), (*>), (/=), (<$>), (<*>))
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators (many1Till)
import Text.Parsing.Parser.String (anyChar, noneOf, oneOf, string)

colon :: Parser String String
colon = string ":"

whitespace :: Array Char
whitespace = toCharArray " \r\n\t"

target :: Parser String FilePath
target = do
  res <- many1Till anyChar colon
  toUnfoldable res # fromCharArray # pure

dependency :: Parser String FilePath
dependency = do
  res <- (oneOf whitespace) *> (many $ noneOf whitespace)
  res # fromCharArray # pure

parser :: Parser String (Tuple FilePath (Array FilePath))
parser = Tuple <$> target <*> (filter (\y -> y /= "") <$> many dependency)

gccParseDependencies :: String -> Either String (Tuple FilePath (Array FilePath))
gccParseDependencies content = case runParser content $ parser of
  Left err -> show err # Left
  Right res -> res # Right
