module Toolchain.Compile (CompileSpec(..), CompileSpecRecord, mkCompiler, parCompileN) where

import Control.Apply (lift2)
import Control.Monad.Error.Class (try)
import Control.Monad.Except.Trans (ExceptT, throwError, mapExceptT, runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parSequence)
import Control.Parallel.Class (parallel)
import Data.Array (take, drop)
import Data.Either (Either(..))
import Data.Either.Map (mapRight)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff (Aff, forkAff, joinFiber, Fiber)
import Effect.Aff.LaunchProcess (launchProcess)
import Effect.Class (liftEffect)
import Node.ChildProcess (pipe, defaultSpawnOptions)
import Node.FS.Sync (exists)
import Node.FS.Sync.Mkdirp (mkdirp)
import Node.Path (FilePath, dirname)
import Prelude
import Toolchain (Toolchain(..), ToolchainRecord)
import Toolchain.CompilerConfiguration (CompilerConfiguration)

type CompileSpecRecord =
  { input :: FilePath
  , output :: FilePath
  }
newtype CompileSpec = CompileSpec CompileSpecRecord

unCompileSpec :: CompileSpec -> CompileSpecRecord
unCompileSpec (CompileSpec compileSpec) = compileSpec

genCompileArgs :: ToolchainRecord -> Array CompilerConfiguration -> CompileSpecRecord -> Array String
genCompileArgs tc cfg spec =
  tc.generateCompilerFlags (tc.defaultCompilerConfiguration <> cfg) spec.input spec.output

createOutputDir :: FilePath -> Aff Unit
createOutputDir dir =
  do
    dirExists <- liftEffect $ exists dir
    if not dirExists then liftEffect $ mkdirp dir else pure unit

compile :: Toolchain -> Array CompilerConfiguration -> CompileSpec -> ExceptT String Aff Unit
compile (Toolchain tc) cfg (CompileSpec spec) =
  do
    _ <- lift $ createOutputDir (dirname spec.output)
    launchProcess tc.compiler args' options'
  where
    args' = genCompileArgs tc cfg spec
    options' = defaultSpawnOptions { stdio = pipe }

mkCompiler :: Toolchain -> Array CompilerConfiguration -> (CompileSpec -> ExceptT String Aff Unit)
mkCompiler tc cfg = compile tc cfg

checkThreadCount :: Int -> ExceptT String Aff Unit
checkThreadCount n = if n < 1 then throwError "not enough threads (minimum 1)" else pure unit

joinJobs :: Aff (Either String (Array Unit)) -> Aff (Either String Unit)
joinJobs aff = aff >>= mapRight fold >>> pure

parCompile :: (CompileSpec -> ExceptT String Aff Unit) -> Array CompileSpec -> ExceptT String Aff Unit
parCompile compile' specs = mapExceptT joinJobs $ compile' <$> specs # parSequence

parCompileN :: (CompileSpec -> ExceptT String Aff Unit) -> Int -> Array CompileSpec -> ExceptT String Aff Unit
parCompileN compile' n specs =
  do
    _ <- checkThreadCount n
    _ <- specs # take n # parCompile compile'
    specs # drop n # parCompileN compile' n
