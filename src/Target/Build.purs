module Target.Build (build) where

import App (App, Config(..), Error, ExceptT, ask, cBinaryPath, performError, cSymlinkPath)
import Data.Array (zip, filter)
import Data.Either(Either)
import Data.Either.Map (mapRight)
import Data.Eq ((/=))
import Data.HeytingAlgebra ((||))
import Data.Newtype (unwrap)
import Data.Traversable (sequence)
import Data.Tuple (fst, snd)
import Effect.Aff (Aff)
import Node.FS (SymlinkType(..))
import Node.FS.Sync.Except (exists, existsOrMkdir, symlink, unlink)
import Node.Path (FilePath, dirname, relative)
import Prelude (Unit, bind, discard, map, pure, unit, not, ($), (<$>), (>>=), (>>>), (#))
import Target (Target,  compilerConfig, linkerConfig, buildType)
import Target.Hash (xor')
import Target.Object (Object, loadObjects, objectPath, sourcePath, writeChecksum, sourceHash, dependHash, checksum, checksum')
import Toolchain.Compiler as Compiler
import Toolchain.Linker as Linker

-- |
-- | Compile the sources of a target, generating the object files.
-- |
build :: Target -> App Unit
build target = do
  let buildType' = buildType target
  config <- ask >>= unwrap >>> pure

  objs <- loadObjects target

  _ <- runCompiler target objs
  withDependencies <- loadObjects target
  withChecksums <- updateChecksums target withDependencies
  _ <- runLinker target withChecksums

  pure unit

runCompiler :: Target -> Array Object -> App Unit
runCompiler target objs = do
  let buildType' = buildType target
  config <- ask >>= unwrap >>> pure
  compileSpec' <- performError $ makeCompileSpec objs
  let compileConfig = Compiler.Config { toolchain: config.toolchain
                                      , configuration: compilerConfig target
                                      , buildType: buildType target
                                      , nofCores: config.nofCores }
  performError $ Compiler.parCompileN compileConfig compileSpec'

runLinker :: Target -> Array Object -> App Unit
runLinker target objs = do
  let buildType' = buildType target
  config <- ask >>= unwrap >>> pure
  let binaryPath = cBinaryPath (Config config) target (checksum' objs)
  binaryExists <- performError $ exists binaryPath
  if binaryExists then pure unit else do
    _ <- performError $ existsOrMkdir $ (dirname binaryPath)
    Linker.link buildType' (linkerConfig target) (linkSpec binaryPath objs) >>= (\_ -> pure unit)
  performError $ createSymlink (cSymlinkPath (Config config) target) binaryPath

updateChecksums :: Target -> Array Object -> App (Array Object)
updateChecksums target objs = writeChecksum target <$> objs # sequence

createSymlink :: FilePath -> FilePath -> ExceptT Error Aff Unit
createSymlink symlinkPath binaryPath = do
  symlinkExists <- exists symlinkPath
  if symlinkExists then unlink symlinkPath else pure unit
  let symlinkTo = relative (dirname symlinkPath) binaryPath
  _ <- existsOrMkdir $ dirname symlinkPath
  _ <- symlink symlinkTo symlinkPath FileLink
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
    needsRebuild'' obj = do
      objExists <- exists $ objectPath obj
      pure $ not objExists || ((xor' (sourceHash obj) (dependHash obj)) /= checksum obj)
