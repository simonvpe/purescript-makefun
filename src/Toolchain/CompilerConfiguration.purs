module Toolchain.CompilerConfiguration (CompilerConfiguration(..)) where

import Node.Path (FilePath)

data CompilerConfiguration
  = DontLink
  | IncludeDirectory FilePath
  | GenerateDependencyInformation
  | NoCompilerConfiguration
