module Build
       ( build
       , link
       )
       where

import Cache (Cache)
import Control.Monad.Except.Trans (ExceptT(..), throwError, runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array (zip, filter)
import Data.Either(Either(..))
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
import Toolchain as TC

type HeaderRecord = { ptah :: FilePath, hash :: String }
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
    -- let k = esrc :: Int
    -- k
    -- esrc >>= (\(Source src) ->
    --            Object { source: Source { path: src.path, hash: src.hash, headers: src.headers }
    --                   , path: concat [ builddir, src.path <> ".o", src.hash <> ".o"] }) >>> pure
    --   # pure

unObj :: Object -> ObjectRecord
unObj (Object x) = x

loadObjects :: FilePath -> Target -> Array FilePath -> ExceptT Error Aff (Array Object)
loadObjects builddir target headers = loadObject builddir headers <$> sources target # sequence
  -- (((runExceptT $ loadObject builddir headers) <$> sources target) # sequence) >>= (sequence >>> pure)

needsRebuild :: FilePath -> Cache -> Array Object -> Aff (Array Object)
needsRebuild builddir cache objs =
  let needsRebuild'' obj = (liftEffect $ exists obj.path) >>= (not >>> pure)
  in do
    rebuild <- (needsRebuild'' <<< unObj) <$> objs # sequence
    pure $ zip objs rebuild # (filter snd >>> map fst)

build :: forall r. TC.Toolchain r -> FilePath -> Int -> Target -> Aff (Either Error (Array (Tuple String String)))
build toolchain builddir nofThreads target =
  let compiler = TC.mkCompiler toolchain $ compilerConfig target
      compile inputs = TC.parCompile compiler nofThreads inputs
      needsRebuild' cache objs = needsRebuild builddir cache objs >>=
                                 map (\(Object x) -> Tuple (unSrc x.source).path x.path) >>> pure
  in do
    o <- runExceptT $ loadObjects builddir target []
    case o of
      Left err -> pure $ Left err
      Right objs -> do
        rebuild <- needsRebuild' [] objs
        compile rebuild

link :: forall r. TC.Toolchain r -> FilePath -> Target -> Aff(Either Error FilePath)
link toolchain builddir target = do
  o <- runExceptT $ loadObjects builddir target []
  case o of
    Left err -> pure $ Left err
    Right objs -> do
      TC.link toolchain (linkerConfig target) (((\x -> x.path) <<< unObj) <$> objs) "exe"
