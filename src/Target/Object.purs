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

import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array (fold)
import Data.Either (Either(..))
import Data.Either.Map (mapRight)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Show (class Show)
import Data.Traversable (sequence)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Node.Crypto.Hash as Hash
import Node.Crypto.Hash.Except (hex)
import Node.Encoding(Encoding(UTF8))
import Node.FS.Sync.Except (readTextFile)
import Node.Path (FilePath, concat, parse)
import Prelude (bind, discard, pure, show, (#), ($), (<$>), (<>), (>>=), (>>>))
import Target

import Effect.Console (logShow)

-- | Get the path to the object file
objectPath :: Object -> FilePath
objectPath (Object obj) = obj.path

-- | Get the path to the compilation unit
sourcePath :: Object -> FilePath
sourcePath (Object obj) = (unwrap obj.source).path

-- | Load all objects, given a target
-- loadObjects :: forall m. Bind m => MonadAsk Config m => MonadEffect m => m Unit
loadObjects :: FilePath -> Target -> ExceptT Error Aff (Array Object)
loadObjects builddir target = loadObject builddir <$> sources target # sequence

-- | Retrieve the accumulated hash of a set of objects, effectively
-- | uniquely identifying the set
hash :: Array Object -> ExceptT Error Aff String
hash objs = fold hashes # hex Hash.MD5
  where hashes = (unwrap >>> (\x -> x.source) >>> unwrap >>> (\x -> x.hash)) <$> objs

-- | Specifies the path to a header and its hash
type HeaderRecord = { path :: FilePath, hash :: String }

-- | Specifies the path to a bunch of headers, the dependency file itself, and its hash
type DependRecord = { path :: FilePath, hash :: String }

-- | Specifies the path to a source file, its hash, and any headers it depends on
type SourceRecord = { path :: FilePath, hash :: String, dependencies :: Maybe Depend }

-- | Specifies the path to an object and the source it depends on
type ObjectRecord = { source :: Source, path :: FilePath }

newtype Header = Header HeaderRecord
newtype Depend = Depend DependRecord
newtype Source = Source SourceRecord
newtype Object = Object ObjectRecord

type Error = String

unSource :: Source -> SourceRecord
unSource (Source src) = src

object :: FilePath -> Source -> Object
object builddir (Source src) =
  Object { source: Source src, path: concat [ builddir, src.path <> ".o", src.hash <> ".o" ]}

loadObject :: FilePath -> FilePath -> ExceptT Error Aff Object
loadObject builddir path = loadSource builddir path >>= object builddir >>> pure

loadSource :: FilePath -> FilePath -> ExceptT Error Aff Source
loadSource builddir path = do
  contents   <- readTextFile UTF8 path
  hashResult <- hex Hash.MD5 contents
  deps       <- lift $ loadDepend (dependPath builddir path hashResult)
  pure $ Source {path: path, hash: hashResult, dependencies: deps}

dependPath :: FilePath -> FilePath -> String -> FilePath
dependPath builddir srcPath srcHash = concat [builddir, srcPath <> ".o", srcHash <> ".d" ]

eitherToMaybe :: forall a e. Either e a -> Maybe a
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right v) = Just v

loadDepend :: FilePath -> Aff (Maybe Depend)
loadDepend path = (readDepend path # runExceptT) >>= eitherToMaybe >>> pure

readDepend :: FilePath -> ExceptT Error Aff Depend
readDepend path = do
  contents <- readTextFile UTF8 path
  hashResult <- hex Hash.MD5 contents
  pure $ Depend {path: path, hash: hashResult }

derive instance newtypeHeader :: Newtype Header _
derive instance newtypeDepend :: Newtype Depend _
derive instance newtypeSource :: Newtype Source _
derive instance newtypeObject :: Newtype Object _

instance showDepend :: Show Depend where
  show (Depend d) = "{path: " <> show d.path
                    <> ", hash: " <> show d.hash
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
