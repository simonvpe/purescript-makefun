module Target where

import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Node.Path (FilePath)
import Toolchain as Tc
import Toolchain.CompilerConfiguration (CompilerConfiguration(..))
import Toolchain.LinkerConfiguration (LinkerConfiguration(..))

type TargetName = String

data Target =
  Executable
  { name :: TargetName
  , sources :: Array FilePath
  , compilerConfig :: Array CompilerConfiguration
  , linkerConfig :: Array LinkerConfiguration
  }

sources :: Target -> Array FilePath
sources target = case target of
  Executable e -> e.sources

compilerConfig :: Target -> Array CompilerConfiguration
compilerConfig target = case target of
  Executable e -> e.compilerConfig

linkerConfig :: Target -> Array LinkerConfiguration
linkerConfig target = case target of
  Executable e -> e.linkerConfig

-- dependencies :: forall r. Tc.Toolchain r -> Target -> Aff (Array Tc.Dependency)
-- dependencies toolchain target =
--   let src = sources target
--   in do traverse (Tc.dependencies toolchain) src
