module Toolchain.Gcc.DependencyParser where

import Control.Alt ((<|>))
import Data.Array (filter)
import Data.Either (Either(..))
import Data.Either.Map (mapLeft)
import Data.List (toUnfoldable)
import Data.List.Types (toList)
import Data.Monoid (mempty)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..))
import Node.Path (FilePath)
import Prelude (bind, pure, show, unit, (#), ($), (*>), (<*), (/=), (<$>), (>>=), (>>>))
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.Combinators (many, many1, many1Till, manyTill, lookAhead)
import Text.Parsing.StringParser.CodeUnits (anyChar, noneOf, oneOf, string, char, satisfy)

gccParseDependencies :: String -> Either String (Array FilePath)
gccParseDependencies content = runParser rule content # mapLeft show

rule :: Parser (Array String)
rule = target *> dependency *> many dependency
       >>= toUnfoldable >>> pure

target :: Parser (String)
target = (many $ char ' ')
         *> (manyTill anyChar $ char ':')
         <* (many $ char ' ')
         >>= toUnfoldable >>> fromCharArray >>> pure

dependency :: Parser (String)
dependency = sameLine <|> newLine

sameLine :: Parser (String)
sameLine = (many $ char ' ')
           *> (many1 $ noneOf [' ', '\\', '\n'])
           <* (many $ char ' ')
           >>= toList >>> toUnfoldable >>> fromCharArray >>> pure

newLine :: Parser (String)
newLine = (many $ char ' ')
          *> string "\\\n"
          *> (many $ char ' ')
          *> (many1 $ noneOf [' ', '\\', '\n'])
          >>= toList >>> toUnfoldable >>> fromCharArray >>> pure
