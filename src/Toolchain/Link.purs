module Toolchain.Link (LinkSpecRecord, LinkSpec(..), unLinkSpec, link) where

import Toolchain (Toolchain(..), ToolchainRecord)
import Node.Path (FilePath, dirname)
import Effect.Aff (Aff)
import Prelude
import Effect.Class (liftEffect)
import Node.FS.Sync (exists)
import Node.FS.Sync.Mkdirp (mkdirp)
import Toolchain.LinkerConfiguration (LinkerConfiguration)
import Node.ChildProcess (defaultSpawnOptions, pipe)
import Effect.Aff.LaunchProcess (launchProcess)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Trans.Class (lift)

type LinkSpecRecord =
  { inputs :: Array FilePath
  , output :: FilePath
  }

newtype LinkSpec = LinkSpec LinkSpecRecord

unLinkSpec :: LinkSpec -> LinkSpecRecord
unLinkSpec (LinkSpec x) = x

createOutputDir :: FilePath -> Aff Unit
createOutputDir dir =
  do
    dirExists <- liftEffect $ exists dir
    if not dirExists then liftEffect $ mkdirp dir else pure unit

genLinkArgs :: ToolchainRecord -> Array LinkerConfiguration -> LinkSpecRecord -> Array String
genLinkArgs tc cfg spec =
  tc.generateLinkerFlags (tc.defaultLinkerConfiguration <> cfg) (spec.inputs <> tc.extraObjects) spec.output

link :: Toolchain -> Array LinkerConfiguration -> LinkSpec -> ExceptT String Aff Unit
link (Toolchain tc) cfg (LinkSpec spec) =
  do
    _ <- lift $ createOutputDir (dirname spec.output)
    launchProcess tc.linker args' options'
  where
    args' = genLinkArgs tc cfg spec
    options' = defaultSpawnOptions { stdio = pipe }
