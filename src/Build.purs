module Build
       ( build
       , link
       )
       where

import Control.Monad.Except.Trans (ExceptT(..), throwError, runExceptT, mapExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array (zip, filter)
import Data.Either(Either(..))
import Data.Either.Map (mapRight)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Crypto.Hash (hex, Algorithm(MD5))
import Node.Encoding(Encoding(UTF8))
import Node.FS.Sync (exists, readTextFile)
import Node.Path (FilePath, concat)
import Prelude
import Target (Target, sources, compilerConfig, linkerConfig)
import Toolchain.Link as Link
import Toolchain.Compile as Compile
import Toolchain (Toolchain)

type HeaderRecord = { path :: FilePath, hash :: String }
type SourceRecord = { path :: FilePath, hash :: String, headers :: Array (Maybe Header) }
type ObjectRecord = { source :: Source, path :: FilePath }

newtype Header = Header HeaderRecord
newtype Source = Source SourceRecord
newtype Object = Object ObjectRecord

type Error = String

loadSource :: Array FilePath -> FilePath -> ExceptT Error Aff Source
loadSource _ path = do
  pathExists <- lift $ liftEffect $ exists path
  if pathExists
     then do
          contents <- lift $ liftEffect $ readTextFile UTF8 path
          hash <- lift $ liftEffect $ hex MD5 contents
          pure $ Source {path: path, hash: hash, headers: []}
     else
          throwError("Error: file not found (" <> path <> ")")

unSrc :: Source -> SourceRecord
unSrc (Source x) = x

loadObject :: FilePath -> Array FilePath -> FilePath -> ExceptT Error Aff Object
loadObject builddir headers path =
  let un (Source src) = src
  in do
    esrc <- loadSource headers path
    let src = un esrc
    pure $ Object { source: Source { path: src.path, hash: src.hash, headers: src.headers }
                  , path: concat [ builddir, src.path <> ".o", src.hash <> ".o"] }

unObj :: Object -> ObjectRecord
unObj (Object x) = x

loadObjects :: FilePath -> Target -> Array FilePath -> ExceptT Error Aff (Array Object)
loadObjects builddir target headers = loadObject builddir headers <$> sources target # sequence
  -- (((runExceptT $ loadObject builddir headers) <$> sources target) # sequence) >>= (sequence >>> pure)

needsRebuild :: FilePath -> Array Object -> Aff (Array Object)
needsRebuild builddir objs =
  let needsRebuild'' obj = (liftEffect $ exists obj.path) >>= (not >>> pure)
  in do
    rebuild <- (needsRebuild'' <<< unObj) <$> objs # sequence
    pure $ zip objs rebuild # (filter snd >>> map fst)

toCompileSpec :: Object -> Compile.CompileSpec
toCompileSpec (Object obj) = Compile.CompileSpec { input: obj.path, output: (unSrc obj.source).path }

build :: Toolchain -> FilePath -> Int -> Target -> ExceptT Error Aff Unit
build tc builddir nofThreads target =
  do
    objs <- loadObjects builddir target []
    rebuild <- lift $ needsRebuild builddir objs
    let spec = toCompileSpec <$> rebuild
    let compiler = Compile.mkCompiler tc $ compilerConfig target
    Compile.parCompileN compiler nofThreads spec

xxx :: FilePath -> Aff (Either String Unit) -> Aff (Either String FilePath)
xxx path aff = aff >>= mapRight (\_ -> path) >>> pure

link :: Toolchain -> FilePath -> Target -> ExceptT Error Aff FilePath
link tc builddir target =
  do
    objs <- loadObjects builddir target []
    let output = "exec"
    let spec = Link.LinkSpec { inputs: (((\x -> x.path) <<< unObj)) <$> objs, output: output}
    mapExceptT (xxx output) $ Link.link tc (linkerConfig target) spec
