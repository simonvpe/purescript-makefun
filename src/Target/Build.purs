module Target.Build (build) where

import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Error.Class (class MonadError)
import Data.Array (zip, filter)
import Data.Either(Either)
import Data.Either.Map (mapRight)
import Data.Traversable (sequence)
import Data.Tuple (fst, snd)
import Effect.Aff (Aff)
import Node.FS (SymlinkType(..))
import Node.FS.Sync.Except (exists, existsOrMkdir, symlink, unlink)
import Node.Path (FilePath, concat, dirname, relative)
import Prelude (Unit, bind, discard, show, map, pure, unit, not, ($), (<$>), (>>=), (>>>), (#), (<>))
import Target (Target, compilerConfig, linkerConfig, name)
import Target.Object (Object, loadObjects, objectPath, sourcePath, hash)
import Toolchain (Toolchain)
import Toolchain.Compiler as Compiler
import Toolchain.Linker as Linker
import Control.Monad.Except.Trans
import Control.Monad.Reader.Trans
import Effect.Class
import Control.Bind (class Bind)
import Data.Newtype (unwrap)
import App
import Effect.Console (logShow)
import Effect.Class (liftEffect)

-- |
-- | Compile the sources of a target, generating the object files.
-- |
build :: forall m. Bind m => MonadAsk Config m => MonadEffect m => MonadError Error m => m Unit
build = do
  config <- ask >>= unwrap >>> pure
  let target = config.target
  _ <- throwError("HELP")
  artifactPath' <- artifactPath target
  -- objs <- loadObjects artifactPath' target
  -- lift $ liftEffect $ logShow objs
  -- let compiler = Compiler.mkCompiler tc $ compilerConfig target
  -- _ <- makeCompileSpec objs >>= Compiler.parCompileN compiler nofThreads

  -- hash' <- hash objs
  -- let binaryPath' = binaryPath builddir target hash'
  -- binaryExists <- exists $ binaryPath'
  -- if binaryExists then pure unit
  --   else do
  --        _ <- existsOrMkdir $ dirname binaryPath'
  --        Linker.link tc (linkerConfig target) (linkSpec binaryPath' objs) >>= (\_ -> pure unit)
  -- -- if binaryExists
  -- --   then pure symlinkPath'
  -- --   else do _ <- existsOrMkdir $ dirname binaryPath'

  -- let symlinkPath' = symlinkPath builddir target
  -- symlinkExists <- exists $ symlinkPath'
  -- if symlinkExists then unlink symlinkPath' else pure unit
  -- let symlinkTo = relative (dirname (symlinkPath builddir target)) binaryPath'
  -- _ <- existsOrMkdir $ dirname symlinkPath'
  -- _ <- symlink symlinkTo symlinkPath' FileLink
  pure unit

compileSpec :: Object -> Compiler.CompileSpec
compileSpec obj = Compiler.CompileSpec { input: sourcePath obj, output: objectPath obj }

makeCompileSpec :: Array Object -> ExceptT Error Aff (Array Compiler.CompileSpec)
makeCompileSpec objects = needsRebuild objects >>= map compileSpec >>> pure

unitToFilePath :: FilePath -> Aff (Either String Unit) -> Aff (Either String FilePath)
unitToFilePath path aff = aff >>= mapRight (\_ -> path) >>> pure

linkSpec :: FilePath -> Array Object -> Linker.LinkSpec
linkSpec output objs = Linker.LinkSpec { inputs: objectPath <$> objs, output: output}

needsRebuild :: Array Object -> ExceptT Error Aff (Array Object)
needsRebuild objs = do
  rebuild <- needsRebuild'' <$> objs # sequence
  pure $ zip objs rebuild # (filter snd >>> map fst)
  where
    needsRebuild'' obj = (exists $ objectPath obj) >>= (not >>> pure)

artifactPath :: forall m. Bind m => MonadAsk Config m => Target -> m FilePath
artifactPath target = do
  config <- ask >>= unwrap >>> pure
  pure $ concat [config.buildDir, (name target) <> ".o"]

binaryPath :: FilePath -> Target -> String -> FilePath
binaryPath builddir target hash' = concat [ builddir
                                          , (name target) <> ".o"
                                          , (name target) <> ".o"
                                          , hash']

symlinkPath :: FilePath -> Target -> FilePath
symlinkPath builddir target = concat [builddir, name target]
