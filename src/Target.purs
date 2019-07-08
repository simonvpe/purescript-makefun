module Target where

import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Node.Path (FilePath)
import Toolchain as Tc

type TargetName = String

data Target =
  Executable
  { name :: TargetName
  , sources :: Array FilePath
  , compilerConfig :: Array Tc.CompilerConfiguration
  }

sources :: Target -> Array FilePath
sources target = case target of
  Executable e -> e.sources

compilerConfig :: Target -> Array Tc.CompilerConfiguration
compilerConfig target = case target of
  Executable e -> e.compilerConfig

-- dependencies :: forall r. Tc.Toolchain r -> Target -> Aff (Array Tc.Dependency)
-- dependencies toolchain target =
--   let src = sources target
--   in do traverse (Tc.dependencies toolchain) src
