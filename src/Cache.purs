module Cache
       ( Cache
       , CacheRow
       , Hash
       , load
       , store
       ) where

import Data.Array (many)
import Data.Either (Either(..), either)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Traversable (sequence, fold)
import Node.Path (FilePath)
import Prelude
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.String (noneOf, oneOf)
import Node.FS.Sync (exists, readTextFile, appendTextFile, truncate)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Node.Encoding (Encoding(UTF8))
import Data.Formatter.DateTime (Formatter, FormatterCommand(..), unformat, format, parseFormatString)
import Data.DateTime
import Data.List (List, fromFoldable)
import Data.Maybe (Maybe(..))

type Hash = String
type CacheRow = Tuple FilePath DateTime
type Cache = Array CacheRow

fmt :: Formatter
fmt = fromFoldable [UnixTimestamp]

load :: FilePath -> Aff Cache
load path =
  let
    whitespace = toCharArray " \r\n\t"

    object = do
      res <- many $ noneOf whitespace
      res # fromCharArray # pure

    row :: Parser String (Maybe (Tuple FilePath DateTime))
    row = do
      obj <- object
      _ <- oneOf whitespace
      datetime <- object
      _ <- oneOf whitespace
      pure $ case unformat fmt datetime of
        Left _ -> Nothing
        Right dt -> Tuple obj dt # Just

    cache :: Parser String (Maybe (Array (Tuple FilePath DateTime)))
    cache = sequence <$> many row

    parse :: String -> Cache
    parse content = case runParser content cache of
      Left _ -> []
      Right m -> case m of
        Nothing -> []
        Just r -> r

  in do
    fileExists <- liftEffect $ exists path
    if fileExists
       then do
            content <- liftEffect $ readTextFile UTF8 path
            liftEffect $ logShow content
            pure $ parse content
      else
            pure []

store :: Cache -> FilePath -> Aff Unit
store cache path =
  let store' row = appendTextFile UTF8 path $ (fst row) <> " " <> (format fmt $ snd row) <> "\n"
  in do
    exists <- liftEffect $ exists path
    _ <- if exists then do liftEffect $ truncate path 0 else pure unit
    _ <- liftEffect $ store' <$> cache # sequence
    pure unit
