module Toolchain where

import Data.Either (Either)
import Data.Newtype (class Newtype)
import Node.Path (FilePath)
import Toolchain.CompilerConfiguration (CompilerConfiguration)
import Toolchain.LinkerConfiguration (LinkerConfiguration)

type ToolchainRecord =
  { compiler :: FilePath
  , linker :: FilePath
  , defaultCompilerConfiguration :: Array CompilerConfiguration
  , defaultLinkerConfiguration :: Array LinkerConfiguration
  , generateCompilerFlags :: Array CompilerConfiguration -> FilePath -> FilePath -> Array String
  , generateLinkerFlags :: Array LinkerConfiguration -> Array FilePath -> FilePath -> Array String
  , parseDependencies :: String -> Either String (Array FilePath)
  , extraObjects :: Array FilePath
  }
newtype Toolchain = Toolchain ToolchainRecord
derive instance newtypeToolchain :: Newtype Toolchain _
