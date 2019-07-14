module Toolchain.Gcc.DependencyParser (gccParseDependencies) where

import Data.Array (filter, many)
import Data.Either (Either(..))
import Data.List (toUnfoldable)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..))
import Node.Path (FilePath)
import Prelude (bind, pure, show, (#), ($), (*>), (/=), (<$>))
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.Combinators (many1Till)
import Text.Parsing.Parser.String (anyChar, noneOf, oneOf, string)

gccParseDependencies :: String -> Either String (Array FilePath)
gccParseDependencies content =
  let colon = string ":"
      whitespace = toCharArray " \r\n\t\\"
      target = do
        res <- many1Till anyChar colon
        toUnfoldable res # fromCharArray # pure
      filename = do
        res <- (oneOf whitespace) *> (many $ noneOf whitespace)
        res # fromCharArray # pure
      parser = target *> filename *> (filter (\y -> y /= "") <$> many filename)
  in case runParser content $ parser of
    Left err -> show err # Left
    Right res -> res # Right
