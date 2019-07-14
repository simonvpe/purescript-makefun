module Toolchain.Linker (LinkSpecRecord, LinkSpec(..), link) where

import App (App, ask, unwrap, performError)
import Data.Newtype (class Newtype)
import Effect.Aff.LaunchProcess (launchProcess)
import Node.ChildProcess (defaultSpawnOptions, pipe)
import Node.FS.Sync.Except (existsOrMkdir)
import Node.Path (FilePath, dirname)
import Prelude
import Toolchain (Toolchain(..), BuildType)
import Toolchain.LinkerConfiguration (LinkerConfiguration)

-- |
-- | Link objects into a binary.
-- |
link :: BuildType -> Array LinkerConfiguration -> LinkSpec -> App Unit
link buildType  cfg (LinkSpec spec) = do
  config <- ask >>= unwrap >>> pure
  let tc = unwrap config.toolchain
      launch' = launchProcess (tc.linker buildType) (genLinkArgs config.toolchain buildType cfg spec) (defaultSpawnOptions { stdio = pipe })
  performError $ existsOrMkdir (dirname spec.output) *> launch'

-- |
-- | Specifies what to link
-- |
newtype LinkSpec = LinkSpec LinkSpecRecord
type LinkSpecRecord = { inputs :: Array FilePath, output :: FilePath }
derive instance newtypeLinkSpec :: Newtype LinkSpec _

genLinkArgs :: Toolchain -> BuildType -> Array LinkerConfiguration -> LinkSpecRecord -> Array String
genLinkArgs (Toolchain tc) buildType cfg spec =
  tc.generateLinkerFlags cfg' (spec.inputs <> tc.extraObjects) spec.output
  where cfg' = (tc.defaultLinkerConfiguration buildType) <> cfg
