module Toolchain.Compiler (CompileSpec(..), Config(..), Compiler(..), CompilerMonad, parCompileN) where

import App (Error, unwrap)
import Control.Monad.Except.Trans (ExceptT, class MonadError, class MonadThrow, throwError)
import Control.Monad.Reader.Trans (ReaderT, class MonadReader, class MonadAsk, runReaderT, ask)
import Control.Monad.Trans.Class (lift)
import Data.Array (take, drop)
import Data.Foldable (fold)
import Data.Newtype (class Newtype)
import Data.Traversable (sequence)
import Effect.Aff (Aff)
import Effect.Aff.LaunchProcess (launchProcess)
import Effect.Class (class MonadEffect)
import Node.ChildProcess (pipe, defaultSpawnOptions)
import Node.FS.Sync.Except (existsOrMkdir)
import Node.Path (FilePath, dirname)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, ($), (>>>), Unit, bind, pure, unit, (#), (*>), (<), (<$>), (<>), (>>=))
import Toolchain (BuildType, Toolchain)
import Toolchain.CompilerConfiguration (CompilerConfiguration)

newtype CompileSpec = CompileSpec { input :: FilePath, output :: FilePath }
derive instance newtypeCompileSpec :: Newtype CompileSpec _

newtype Config = Config { toolchain :: Toolchain, configuration :: Array CompilerConfiguration, buildType :: BuildType, nofCores :: Int}
derive instance newtypeConfigRecord :: Newtype Config _

type CompilerMonad a = ReaderT Config (ExceptT Error Aff) a
newtype Compiler a = Compiler (CompilerMonad a)

parCompileN :: Config -> Array CompileSpec -> ExceptT Error Aff Unit
parCompileN config spec = runCompiler (parCompileN' spec) config
  where
    runCompiler :: forall a. Compiler a -> Config -> ExceptT Error Aff a
    runCompiler (Compiler c) config' = runReaderT c config'

parCompileN' :: Array CompileSpec -> Compiler Unit
parCompileN' [] = pure unit
parCompileN' specs = do
  nofCores' <- nofCores
  (specs # take nofCores' # parCompile) *> (specs # drop nofCores' # parCompileN')
  where
    nofCores :: Compiler (Int)
    nofCores = do
      config <- ask >>= unwrap >>> pure
      if config.nofCores < 1 then throwError "not enough threads (minimum 1)" else pure config.nofCores

    -- TODO: This is not parallel, its sequential!
parCompile :: Array CompileSpec -> Compiler Unit
parCompile specs' = compile <$> specs' # sequence >>= fold >>> pure

compile :: CompileSpec -> Compiler Unit
compile (CompileSpec spec) = do
  compileArgs' <- compileArgs (CompileSpec spec)
  compiler' <- compiler
  performError $ existsOrMkdir (dirname spec.output) *> launchProcess compiler' compileArgs' (defaultSpawnOptions {stdio = pipe})

compiler :: Compiler FilePath
compiler = do
  config <- ask >>= unwrap >>> pure
  pure $ (unwrap config.toolchain).compiler config.buildType

performError :: forall a. ExceptT Error Aff a -> Compiler a
performError = lift >>> Compiler

compileArgs :: CompileSpec -> Compiler (Array String)
compileArgs (CompileSpec spec) = do
  generateCompilerFlags' <- generateCompilerFlags
  configuration' <- configuration
  pure $ generateCompilerFlags' configuration' spec.input spec.output
  where
    generateCompilerFlags :: Compiler (Array CompilerConfiguration -> FilePath -> FilePath -> Array String)
    generateCompilerFlags = do
      config <- ask >>= unwrap >>> pure
      defaultCompilerConfiguration' <- defaultCompilerConfiguration
      pure $ \cfg input output -> (unwrap config.toolchain).generateCompilerFlags (defaultCompilerConfiguration' <> cfg) input output

    defaultCompilerConfiguration :: Compiler (Array CompilerConfiguration)
    defaultCompilerConfiguration = do
      config <- ask >>= unwrap >>> pure
      pure $ (unwrap config.toolchain).defaultCompilerConfiguration config.buildType

    configuration :: Compiler (Array CompilerConfiguration)
    configuration = do
      config <- ask >>= unwrap >>> pure
      pure $ config.configuration

derive newtype instance bindCompiler :: Bind Compiler
derive newtype instance applyCompiler :: Apply Compiler
derive newtype instance functorCompiler :: Functor Compiler
derive newtype instance applicativeCompiler :: Applicative Compiler
derive newtype instance monadCompiler :: Monad Compiler
derive newtype instance monadEffectCompiler :: MonadEffect Compiler
derive newtype instance monadAskCompiler :: MonadAsk Config Compiler
derive newtype instance monadeReaderCompiler :: MonadReader Config Compiler
derive newtype instance monadThrowCompiler :: MonadThrow String Compiler
derive newtype instance monadErrorCompiler :: MonadError String Compiler

