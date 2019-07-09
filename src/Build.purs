module Build where

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Path (FilePath)
import Prelude
import Target (Target, sources, compilerConfig)
import Toolchain (Toolchain, parCompile, mkCompiler)
import Node.FS.Sync (exists, readTextFile)
import Node.Path (concat)
import Data.Traversable (sequence)
import Data.Either(Either(..))
import Node.Crypto.Hash (hex, Algorithm(MD5))
import Node.Encoding(Encoding(UTF8))
import Data.Array (zip, filter)
import Data.Tuple (Tuple(..), fst, snd)
import Cache (Cache)

type Source = { path :: FilePath, hash :: String }
type Object = { source :: Source, path :: FilePath }
type Error = String

source :: FilePath -> Aff (Either Error Source)
source path = do
  pathExists <- liftEffect $ exists path :: Aff Boolean
  if pathExists
    then do
         contents <- liftEffect $ readTextFile UTF8 path
         hash <- liftEffect $ hex MD5 contents
         pure $ Right {path: path, hash: hash}
    else
         pure $ Left ("Error: file not found (" <> path <> ")")

object :: FilePath -> FilePath -> Aff (Either Error Object)
object builddir path =
  let objPath src = concat [builddir, src.path <> ".o", src.hash <> ".o"]
      obj src = {source: src, path: objPath src }
  in source path >>= (\e -> e >>= obj >>> pure) >>> pure

objects :: FilePath -> Target -> Aff (Either Error (Array Object))
objects builddir target =
  ((object builddir <$> sources target) # sequence) >>= (sequence >>> pure)

needsRebuild :: FilePath -> Cache -> Array Object -> Aff (Array Object)
needsRebuild builddir cache objs =
  let needsRebuild'' obj = (liftEffect $ exists obj.path) >>= (not >>> pure)
  in do
    rebuild <- needsRebuild'' <$> objs # sequence
    pure $ zip objs rebuild # (filter snd >>> map fst)

build :: forall r. Toolchain r -> FilePath -> Int -> Target -> Aff (Either String (Array (Tuple String String)))
build toolchain builddir nofThreads target =
  let compiler = mkCompiler toolchain $ compilerConfig target
      compile inputs = parCompile compiler nofThreads inputs
      needsRebuild' builddir cache objs = needsRebuild builddir cache objs >>=
                                          map (\x -> Tuple x.source.path x.path) >>> pure
  in do
    o <- objects builddir target
    case o of
      Left err -> pure $ Left err
      Right objs -> do
        rebuild <- needsRebuild' builddir [] objs
        compile rebuild
