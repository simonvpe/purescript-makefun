module Target
       ( Target(..)
       , TargetName
       , name
       , sources
       , compilerConfig
       , linkerConfig
       , buildType
       ) where

import Node.Path (FilePath)
import Toolchain.CompilerConfiguration (CompilerConfiguration)
import Toolchain.LinkerConfiguration (LinkerConfiguration)
import Toolchain as Toolchain

type TargetName = String

data Target =
  Executable
  { name :: TargetName
  , sources :: Array FilePath
  , compilerConfig :: Array CompilerConfiguration
  , linkerConfig :: Array LinkerConfiguration
  }

name :: Target -> TargetName
name target = case target of
  Executable e -> e.name

sources :: Target -> Array FilePath
sources target = case target of
  Executable e -> e.sources

compilerConfig :: Target -> Array CompilerConfiguration
compilerConfig target = case target of
  Executable e -> e.compilerConfig

linkerConfig :: Target -> Array LinkerConfiguration
linkerConfig target = case target of
  Executable e -> e.linkerConfig

buildType :: Target -> Toolchain.BuildType
buildType target = case target of
  Executable _ -> Toolchain.Executable

-- dependencies :: forall r. Tc.Toolchain r -> Target -> Aff (Array Tc.Dependency)
-- dependencies toolchain target =
--   let src = sources target
--   in do traverse (Tc.dependencies toolchain) src
