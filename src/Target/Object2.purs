module Object2
       ( Object
       , loadChecksum
       , loadChecksum'
       , loadDependencies
       , loadDependencies'
       , loadSource
       , loadSource'
       , needsRebuild
       , needsRebuild'
       , writeChecksum
       , writeChecksum'
       ) where

import Prelude

import Control.Monad.Except.Trans (ExceptT, except, throwError)
import Data.Either (Either)
import Data.Foldable (fold, foldM, or)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence, traverse)
import Effect.Aff (Aff)
import Node.Crypto.Hash as Hash
import Node.Crypto.Hash.Except (hex)
import Node.Encoding(Encoding(UTF8))
import Node.FS.Sync.Except (readTextFile, writeTextFile)
import Node.Path (FilePath)
import Target.Hash (xor)

type Error = String
type Hash = String

newtype Object =
  Object { sourcePath :: FilePath
         , sourceHash :: Hash
         , checksum :: Maybe Hash           
         , dep :: Maybe { path :: FilePath, hash :: Hash, hdr :: Array {path :: FilePath, hash :: Hash} }
         }

loadSource :: FilePath -> ExceptT Error Aff Object
loadSource sourcePath = do
  content <- readTextFile UTF8 sourcePath
  sourceHash <- hex Hash.MD5 content
  pure $ Object {sourcePath, sourceHash, dep: Nothing, checksum: Nothing}

loadSource' :: Array FilePath -> ExceptT Error Aff (Array Object)
loadSource' sourcePaths = traverse loadSource sourcePaths

loadDependencies :: (String -> Either Error (Array FilePath)) -> Object -> FilePath -> ExceptT Error Aff Object
loadDependencies parse (Object object) path = do
  content <- readTextFile UTF8 path
  hash <- hex Hash.MD5 content
  headerPaths <- except $ parse content
  hdr <- loadHeaders headerPaths
  pure $ Object (object {dep = Just {path, hash, hdr}})
  where loadHeaders paths = do
          contents <- traverse (readTextFile UTF8) paths
          hashes <- traverse (hex Hash.MD5) contents
          pure $ (\p h -> {path: p, hash: h}) <$> paths <*> hashes

loadDependencies' :: (String -> Either Error (Array FilePath)) -> Array Object -> Array FilePath -> ExceptT Error Aff (Array Object)
loadDependencies' parse objects paths = loadDependencies parse <$> objects <*> paths # sequence

loadChecksum :: Object -> FilePath -> ExceptT Error Aff Object
loadChecksum (Object object) path = do
  hash <- readTextFile UTF8 path
  pure $ Object (object {checksum = Just hash})

loadChecksum' :: Array Object -> Array FilePath -> ExceptT Error Aff (Array Object)
loadChecksum' objects paths = loadChecksum <$> objects <*> paths # sequence

writeChecksum :: Object -> FilePath -> ExceptT Error Aff Unit
writeChecksum object path = do
  case expectedChecksum object of
    Nothing -> throwError "failed to compute checksum"
    Just cs -> writeTextFile UTF8 path cs

writeChecksum' :: Array Object -> Array FilePath -> ExceptT Error Aff Unit
writeChecksum' objects paths = writeChecksum <$> objects <*> paths # sequence >>= fold >>> pure

needsRebuild :: Object -> Boolean
needsRebuild (Object object) = (==) <$> expectedChecksum (Object object) <*> object.checksum # maybe false (\x -> x)

needsRebuild' :: Array Object -> Boolean
needsRebuild' objects = needsRebuild <$> objects # or

expectedChecksum :: Object -> Maybe Hash
expectedChecksum (Object object) = xor <$> (Just object.sourceHash) <*> (dependencyHash (Object object)) # fold

dependencyHash :: Object -> Maybe Hash
dependencyHash (Object {dep: Nothing}) = Nothing
dependencyHash (Object {dep: Just {hash: depHash, hdr}}) = xor <$> (Just depHash) <*> (headerHash hdr) # fold
  where headerHash a = a # map _.hash # foldM xor ""
