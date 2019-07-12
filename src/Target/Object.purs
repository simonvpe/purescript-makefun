module Target.Object
       ( Object
       , ObjectRecord
       , Source
       , SourceRecord
       , Depend
       , DependRecord
       , Header
       , HeaderRecord
       , objectPath
       , sourcePath
       , loadObjects
       , hash
       ) where

import App (class Newtype, App, Error, ExceptT, ask, cDependPath, cObjectPath, performAff, performError, runExceptT, unwrap)
import Control.Monad.Except.Trans (except)
import Data.Array (fold)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Show (class Show)
import Data.Traversable (sequence)
import Effect.Aff (Aff)
import Node.Crypto.Hash as Hash
import Node.Crypto.Hash.Except (hex)
import Node.Encoding(Encoding(UTF8))
import Node.FS.Sync.Except (readTextFile)
import Node.Path (FilePath)
import Prelude (bind, pure, show, (#), ($), (<$>), (<>), (>>=), (>>>))
import Target (Target, sources)

-- | Get the path to the object file
objectPath :: Object -> FilePath
objectPath (Object obj) = obj.path

-- | Get the path to the compilation unit
sourcePath :: Object -> FilePath
sourcePath (Object obj) = (unwrap obj.source).path

-- | Load all objects, given a target
-- loadObjects :: forall m. Bind m => MonadAsk Config m => MonadEffect m => m Unit
loadObjects :: Target -> App (Array Object)
loadObjects target = loadObject target <$> sources target # sequence

-- | Retrieve the accumulated hash of a set of objects, effectively
-- | uniquely identifying the set
hash :: Array Object -> ExceptT Error Aff String
hash objs = fold hashes # hex Hash.MD5
  where hashes = (unwrap >>> (\x -> x.source) >>> unwrap >>> (\x -> x.hash)) <$> objs

-- | Specifies the path to a header and its hash
type HeaderRecord = { path :: FilePath, hash :: String }

-- | Specifies the path to a bunch of headers, the dependency file itself, and its hash
type DependRecord = { path :: FilePath, hash :: String, headers :: Array Header }

-- | Specifies the path to a source file, its hash, and any headers it depends on
type SourceRecord = { path :: FilePath, hash :: String, dependencies :: Maybe Depend }

-- | Specifies the path to an object and the source it depends on
type ObjectRecord = { source :: Source, path :: FilePath }

newtype Header = Header HeaderRecord
newtype Depend = Depend DependRecord
newtype Source = Source SourceRecord
newtype Object = Object ObjectRecord

unSource :: Source -> SourceRecord
unSource (Source src) = src

object :: Target -> Source -> App Object
object target (Source src) = do
  config <- ask
  pure $ Object { source: Source src, path: cObjectPath config target src.path src.hash }

loadObject :: Target -> FilePath -> App Object
loadObject target path = loadSource target path >>= object target

loadSource :: Target -> FilePath -> App Source
loadSource target path = do
  config <- ask
  contents <- performError $ readTextFile UTF8 path
  hashResult <- performError $ hex Hash.MD5 contents
  deps <- loadDepend (cDependPath config target path hashResult)
  pure $ Source {path: path, hash: hashResult, dependencies: deps}

eitherToMaybe :: forall a e. Either e a -> Maybe a
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right v) = Just v

loadDepend :: FilePath -> App (Maybe Depend)
loadDepend path = do
  config <- ask >>= unwrap >>> pure
  let parser = (unwrap config.toolchain).parseDependencies
  performAff $ (readDepend path parser # runExceptT) >>= eitherToMaybe >>> pure

readDepend :: FilePath -> (String -> Either Error (Array FilePath)) -> ExceptT Error Aff Depend
readDepend path parse = do
  contents <- readTextFile UTF8 path
  hashResult <- hex Hash.MD5 contents
  headerPaths <- except $ parse contents
  headers <- readHeader <$> headerPaths # sequence
  pure $ Depend {path: path, hash: hashResult, headers: headers }

readHeader :: FilePath -> ExceptT Error Aff Header
readHeader path = do
  contents <- readTextFile UTF8 path
  hashResult <- hex Hash.MD5 contents
  pure $ Header {path: path, hash: hashResult }

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
                    <> "}"

instance showObject :: Show Object where
  show (Object o) = "{path: " <> show o.path
                    <> ", source: " <> show o.source
                    <> "}"
