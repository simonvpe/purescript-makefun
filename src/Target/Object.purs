module Target.Object
       ( Object
       , ObjectRecord
       , Source
       , SourceRecord
       , Header
       , HeaderRecord
       , objectPath
       , sourcePath
       , loadObjects
       , hash
       ) where

import Control.Monad.Except.Trans (ExceptT)
import Data.Array (fold)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (sequence)
import Effect.Aff (Aff)
import Node.Crypto.Hash as Hash
import Node.Crypto.Hash.Except (hex)
import Node.Encoding(Encoding(UTF8))
import Node.FS.Sync.Except (readTextFile)
import Node.Path (FilePath, concat)
import Prelude (bind, pure, (#), ($), (<$>), (<>), (>>=), (>>>))
import Target

-- | Get the path to the object file
objectPath :: Object -> FilePath
objectPath (Object obj) = obj.path

-- | Get the path to the compilation unit
sourcePath :: Object -> FilePath
sourcePath (Object obj) = (unwrap obj.source).path

-- | Load all objects, given a target
loadObjects :: FilePath -> Target -> Array FilePath -> ExceptT Error Aff (Array Object)
loadObjects builddir target headers = loadObject builddir headers <$> sources target # sequence

-- | Retrieve the accumulated hash of a set of objects, effectively
-- | uniquely identifying the set
hash :: Array Object -> ExceptT Error Aff String
hash objs = fold hashes # hex Hash.MD5
  where hashes = (unwrap >>> (\x -> x.source) >>> unwrap >>> (\x -> x.hash)) <$> objs

-- | Specifies the path to a header and its hash
type HeaderRecord = { path :: FilePath, hash :: String }

-- | Specifies the path to a source file, its hash, and any headers it depends on
type SourceRecord = { path :: FilePath, hash :: String, headers :: Array (Maybe Header) }

-- | Specifies the path to an object and the source it depends on
type ObjectRecord = { source :: Source, path :: FilePath }

newtype Header = Header HeaderRecord
newtype Source = Source SourceRecord
newtype Object = Object ObjectRecord

derive instance newtypeHeader :: Newtype Header _
derive instance newtypeSource :: Newtype Source _
derive instance newtypeObject :: Newtype Object _

type Error = String

unSource :: Source -> SourceRecord
unSource (Source src) = src

object :: FilePath -> Source -> Object
object builddir (Source src) =
  Object { source: Source { path: src.path, hash: src.hash, headers: src.headers }
         , path: concat [ builddir, src.path <> ".o", src.hash <> ".o"] }

loadSource :: Array FilePath -> FilePath -> ExceptT Error Aff Source
loadSource _ path = do
  contents <- readTextFile UTF8 path
  hashResult <- hex Hash.MD5 contents
  pure $ Source {path: path, hash: hashResult, headers: []}

loadObject :: FilePath -> Array FilePath -> FilePath -> ExceptT Error Aff Object
loadObject builddir headers path = loadSource headers path >>= object builddir >>> pure
