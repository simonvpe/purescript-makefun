module Toolchain.Compiler (CompileSpec(..), CompileSpecRecord, mkCompiler, parCompileN) where

import Control.Monad.Except.Trans (ExceptT, throwError, mapExceptT)
import Control.Parallel (parSequence)
import Data.Array (take, drop)
import Data.Either (Either)
import Data.Either.Map (mapRight)
import Data.Foldable (fold)
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Effect.Aff.LaunchProcess (launchProcess)
import Node.ChildProcess (pipe, defaultSpawnOptions)
import Node.FS.Sync.Except (existsOrMkdir)
import Node.Path (FilePath, dirname)
import Prelude
import Toolchain (Toolchain(..), ToolchainRecord)
import Toolchain.CompilerConfiguration (CompilerConfiguration)

-- |
-- | Create a compiler that can be passed along to `parCompileN`
-- |
mkCompiler :: Toolchain -> Array CompilerConfiguration -> (CompileSpec -> ExceptT String Aff Unit)
mkCompiler tc cfg = compile tc cfg

-- |
-- | Compile a bunch of objects given the compiler specifications and compiler passed. The parallelism
-- | can be configured.
parCompileN :: (CompileSpec -> ExceptT String Aff Unit) -> Int -> Array CompileSpec -> ExceptT String Aff Unit
parCompileN _ _ [] = pure unit
parCompileN compile' n specs =
  checkThreadCount n
  *> (specs # take n # parCompile compile')
  *> (specs # drop n # parCompileN compile' n)

-- |
-- | Specifies what to compile
-- |
newtype CompileSpec = CompileSpec CompileSpecRecord
type CompileSpecRecord = { input :: FilePath, output :: FilePath }
derive instance newtypeCompileSpec :: Newtype CompileSpec _

genCompileArgs :: ToolchainRecord -> Array CompilerConfiguration -> CompileSpecRecord -> Array String
genCompileArgs tc cfg spec =
  tc.generateCompilerFlags (tc.defaultCompilerConfiguration <> cfg) spec.input spec.output

compile :: Toolchain -> Array CompilerConfiguration -> CompileSpec -> ExceptT String Aff Unit
compile (Toolchain tc) cfg (CompileSpec spec) =
  let launch' = launchProcess tc.compiler (genCompileArgs tc cfg spec) (defaultSpawnOptions { stdio = pipe })
  in existsOrMkdir (dirname spec.output) *> launch'

checkThreadCount :: Int -> ExceptT String Aff Unit
checkThreadCount n = if n < 1 then throwError "not enough threads (minimum 1)" else pure unit

joinJobs :: Aff (Either String (Array Unit)) -> Aff (Either String Unit)
joinJobs aff = aff >>= mapRight fold >>> pure

parCompile :: (CompileSpec -> ExceptT String Aff Unit) -> Array CompileSpec -> ExceptT String Aff Unit
parCompile compile' specs = mapExceptT joinJobs $ compile' <$> specs # parSequence
