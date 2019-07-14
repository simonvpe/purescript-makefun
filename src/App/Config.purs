module App.Config where

import Data.Newtype (class Newtype)
import Node.Path (FilePath, concat, dirname, basename)
import Prelude ((<>))
import Target (Target(..), name)
import Toolchain (Toolchain)

newtype Config =
  Config { toolchain :: Toolchain
         , buildDir :: FilePath
         , nofCores :: Int }

derive instance newtypeConfig :: Newtype Config _

cArtifactsPath :: Config -> Target -> FilePath
cArtifactsPath (Config config) target =
  concat [config.buildDir, name target <> ".o"]

cBinaryPath :: Config -> Target -> String -> FilePath
cBinaryPath (Config config) target hash =
  concat [root, kind, hash <> ".bin"]
  where root = cArtifactsPath (Config config) target
        kind = case target of Executable _ -> "bin"

cObjectPath :: Config -> Target -> FilePath -> String -> FilePath
cObjectPath (Config config) target srcPath srcHash =
  concat [root, subdir, srcHash <> ".o"]
  where root = cArtifactsPath (Config config) target
        subdir = concat [dirname srcPath, basename srcPath <> ".o"]

cDependPath :: Config -> Target -> FilePath -> String -> FilePath
cDependPath (Config config) target srcPath srcHash =
  concat [root, subdir, srcHash <> ".d"]
  where root = cArtifactsPath (Config config) target
        subdir = concat [dirname srcPath, basename srcPath <> ".o"]

cSymlinkPath :: Config -> Target -> FilePath
cSymlinkPath (Config config) target =
  concat [root, name target]
  where root = cArtifactsPath (Config config) target

cChecksumPath :: Config -> Target -> FilePath -> String -> FilePath
cChecksumPath (Config config) target srcPath srcHash =
  concat [root, subdir, srcHash <> ".k"]
  where root = cArtifactsPath (Config config) target
        subdir = concat [dirname srcPath, basename srcPath <> ".o"]
