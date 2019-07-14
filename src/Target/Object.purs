module Target.Object
       ( Object
       , Source
       , Depend
       , Header
       , objectPath
       , sourcePath
       , loadObjects
       , sourceHash
       , dependHash
       , checksum
       , checksum'
       , writeChecksum
       ) where

import App (class Newtype, App, Error, ExceptT, ask, cDependPath, cObjectPath, performAff, performError, runExceptT, unwrap, cChecksumPath)
import Control.Monad.Except.Trans (except, lift)
import Data.Array (catMaybes)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Show (class Show)
import Data.Traversable (sequence)
import Effect.Aff (Aff)
import Node.Crypto.Hash as Hash
import Node.Crypto.Hash.Except (hex)
import Node.Encoding(Encoding(UTF8))
import Node.FS.Sync.Except (readTextFile, writeTextFile)
import Node.Path (FilePath)
import Prelude (bind, pure, show, (#), ($), (<$>), (<>), (>>=), (>>>))
import Target (Target, sources)
import Target.Hash (xor')
import Data.Foldable (foldl)

-- | Get the path to the object file
objectPath :: Object -> FilePath
objectPath (Object obj) = obj.path

-- | Get the path to the source file of an object
sourcePath :: Object -> FilePath
sourcePath (Object obj) = (unwrap obj.source).path

-- | Extract the source hash of an object
sourceHash :: Object -> String
sourceHash (Object obj) = (unwrap obj.source).hash

-- | Calculate the hash of all the dependencies for an object
dependHash :: Object -> String
dependHash (Object obj) = (unwrap obj.source).dependencies # maybe "" dependHash'
  where dependHash' (Depend d) = xor' d.hash $ foldl xor' "" $ (\(Header h) -> h.hash) <$> d.headers

-- | Calculate the checksum for a single object
checksum :: Object -> String
checksum (Object obj) = (unwrap obj.source).checksum # maybe "" (\x -> x)

-- | Calculate the cumulative checksum for a bunch of objects
checksum' :: Array Object -> String
checksum' objs = foldl xor' "" $ checksum <$> objs

-- | Write all object checksums to disk
writeChecksum :: Target -> Object -> App Object
writeChecksum target obj =
  do
    config <- ask
    _ <- performError $ writeTextFile UTF8 (cChecksumPath config target (sourcePath obj) (sourceHash obj)) cs'
    pure $ Object { source: source', path: path' }
  where
    cs' = xor' (sourceHash obj) (dependHash obj)
    source' = Source $ (unwrap $ (unwrap obj).source) {checksum = Just cs'}
    path' = objectPath obj

        
-- | Load all objects, given a target
loadObjects :: Target -> App (Array Object)
loadObjects target = loadObject <$> sources target # sequence
  where
    loadObject :: FilePath -> App Object
    loadObject path = loadSource >>= object
      where
        loadSource :: App Source
        loadSource = do
          config <- ask
          contents <- performError $ readTextFile UTF8 path
          hashResult <- performError $ hex Hash.MD5 contents
          deps <- loadDepend (cDependPath config target path hashResult)
          cs <- loadChecksum (cChecksumPath config target path hashResult)
          let result = Source {path: path, hash: hashResult, dependencies: deps, checksum: cs}
          pure result

        loadChecksum :: FilePath -> App (Maybe String)
        loadChecksum path' = performAff $ (readTextFile UTF8 path' # runExceptT) >>= eitherToMaybe >>> pure

        object :: Source -> App Object
        object (Source src) = do
          config <- ask
          pure $ Object { source: Source src, path: cObjectPath config target src.path src.hash }
        
    loadDepend :: FilePath -> App (Maybe Depend)
    loadDepend path =
      do
        config <- ask >>= unwrap >>> pure
        let parser = (unwrap config.toolchain).parseDependencies
        performAff $ (readDepend parser # runExceptT) >>= eitherToMaybe >>> pure
      where
        readDepend :: (String -> Either Error (Array FilePath)) -> ExceptT Error Aff Depend
        readDepend parse = do
          contents <- readTextFile UTF8 path
          hashResult <- hex Hash.MD5 contents
          headerPaths <- except $ parse contents
          headers <- lift $ readHeader >>> runExceptT <$> headerPaths # sequence >>= ((<$>) eitherToMaybe) >>> pure
          pure $ Depend {path: path, hash: hashResult, headers: catMaybes headers }

        readHeader :: FilePath -> ExceptT Error Aff Header
        readHeader path' = do
          contents <- readTextFile UTF8 path'
          hashResult <- hex Hash.MD5 contents
          pure $ Header {path: path', hash: hashResult }

    eitherToMaybe :: forall a e. Either e a -> Maybe a
    eitherToMaybe (Left _)  = Nothing
    eitherToMaybe (Right v) = Just v

newtype Header = Header { path :: FilePath, hash :: String}
newtype Depend = Depend { path :: FilePath, hash :: String, headers :: Array Header }
newtype Source = Source { path :: FilePath, hash :: String, dependencies :: Maybe Depend, checksum :: Maybe String }
newtype Object = Object { source :: Source, path :: FilePath }

derive instance newtypeHeader :: Newtype Header _
derive instance newtypeDepend :: Newtype Depend _
derive instance newtypeSource :: Newtype Source _
derive instance newtypeObject :: Newtype Object _

instance showHeader :: Show Header where
  show (Header h) = "{path: " <> show h.path
                    <> ", hash: " <> show h.hash
                    <> "}"

instance showDepend :: Show Depend where
  show (Depend d) = "{path: " <> show d.path
                    <> ", hash: " <> show d.hash
                    <> ", headers: " <> show d.headers
                    <> "}"

instance showSource :: Show Source where
  show (Source h) = "{path: " <> show h.path
                    <> ", hash: " <> show h.hash
                    <> ", dependencies: " <> show h.dependencies
                    <> ", checksum: " <> show h.checksum
                    <> "}"

instance showObject :: Show Object where
  show (Object o) = "{path: " <> show o.path
                    <> ", source: " <> show o.source
                    <> "}"
