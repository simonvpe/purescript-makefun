module Cache where

import Data.Array (many)
import Data.Either (Either(..))
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Traversable (sequence)
import Node.Path (FilePath)
import Prelude
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.String (noneOf, oneOf)
import Node.FS.Sync (exists, readTextFile, appendTextFile, truncate)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(UTF8))

type Hash = String
type CacheRow = Tuple FilePath Hash
type Cache = Array CacheRow

load :: FilePath -> Aff Cache
load path =
  let
    whitespace = toCharArray " \r\n\t"

    object = do
      res <- many $ noneOf whitespace
      res # fromCharArray # pure

    row = do
      obj <- object
      _ <- oneOf whitespace
      hash <- object
      _ <- oneOf whitespace
      Tuple obj hash # pure

    cache = many row

    parse content = case runParser content cache of
      Left err -> show err # Left
      Right res -> res # Right
  in do
    fileExists <- liftEffect $ exists path
    if fileExists
       then do
            content <- liftEffect $ readTextFile UTF8 path
            pure $ case parse content of
              Left _ -> []
              Right c -> c
      else
            pure []

store :: Cache -> FilePath -> Aff Unit
store cache path =
  let store' row = appendTextFile UTF8 path $ (fst row) <> " " <> (snd row) <> "\n"
  in do
    exists <- liftEffect $ exists path
    _ <- if exists then do liftEffect $ truncate path 0 else pure unit
    _ <- liftEffect $ store' <$> cache # sequence
    pure unit
