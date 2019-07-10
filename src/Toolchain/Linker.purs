module Toolchain.Linker (LinkSpecRecord, LinkSpec(..), link) where

import Control.Monad.Except.Trans (ExceptT)
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Effect.Aff.LaunchProcess (launchProcess)
import Node.ChildProcess (defaultSpawnOptions, pipe)
import Node.FS.Sync.Except (existsOrMkdir)
import Node.Path (FilePath, dirname)
import Prelude
import Toolchain (Toolchain(..), ToolchainRecord)
import Toolchain.LinkerConfiguration (LinkerConfiguration)

-- |
-- | Link objects into a binary.
-- |
link :: Toolchain -> Array LinkerConfiguration -> LinkSpec -> ExceptT String Aff Unit
link (Toolchain tc) cfg (LinkSpec spec) =
  let launch' = launchProcess tc.linker (genLinkArgs tc cfg spec) (defaultSpawnOptions { stdio = pipe })
  in existsOrMkdir (dirname spec.output) *> launch'

-- |
-- | Specifies what to link
-- |
newtype LinkSpec = LinkSpec LinkSpecRecord
type LinkSpecRecord = { inputs :: Array FilePath, output :: FilePath }
derive instance newtypeLinkSpec :: Newtype LinkSpec _

genLinkArgs :: ToolchainRecord -> Array LinkerConfiguration -> LinkSpecRecord -> Array String
genLinkArgs tc cfg spec =
  tc.generateLinkerFlags (tc.defaultLinkerConfiguration <> cfg) (spec.inputs <> tc.extraObjects) spec.output
