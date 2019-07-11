module Toolchain where

import Control.Monad.Except.Trans (ExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff (Aff)
import Effect.Aff.LaunchProcess (Error, launchProcess)
import Effect.Class (liftEffect)
import Node.ChildProcess (Exit(..), pipe, spawn, defaultSpawnOptions, onExit, stdout, stderr, onError, toStandardError, kill)
import Node.FS.Sync (exists)
import Node.FS.Sync.Mkdirp (mkdirp)
import Node.Path (FilePath, parse, dirname, concat, sep)
import Prelude
import Toolchain.CompilerConfiguration (CompilerConfiguration)
import Toolchain.LinkerConfiguration (LinkerConfiguration)

type ToolchainRecord =
  { compiler :: FilePath
  , linker :: FilePath
  , defaultCompilerConfiguration :: Array CompilerConfiguration
  , defaultLinkerConfiguration :: Array LinkerConfiguration
  , generateCompilerFlags :: Array CompilerConfiguration -> FilePath -> FilePath -> Array String
  , generateLinkerFlags :: Array LinkerConfiguration -> Array FilePath -> FilePath -> Array String
    --  , parseDependencies :: DependencyParser
  , extraObjects :: Array FilePath
  }
newtype Toolchain = Toolchain ToolchainRecord

unToolchain :: Toolchain -> ToolchainRecord
unToolchain (Toolchain toolchain) = toolchain
