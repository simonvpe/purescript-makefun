module Toolchain.Gcc where

import Data.Array (concatMap)
import Data.Semigroup ((<>))
import Toolchain.Gcc.DependencyParser (gccParseDependencies)
import Toolchain (CompilerConfiguration(..), CompilerFlagGenerator, LinkerConfiguration(..), LinkerFlagGenerator, Toolchain)

gccCompilerFlagGenerator :: CompilerFlagGenerator
gccCompilerFlagGenerator config input output =
  let xform x = case x of
        (DontLink)                      -> ["-c"]
        (IncludeDirectory path)         -> ["-I", path]
        (LibraryDirectory path)         -> ["-L", path]
        (GenerateDependencyInformation) -> ["-MMD"]
        (NoCompilerConfiguration)       -> []
  in (concatMap xform config) <> ["-o", output] <> [input]

gccLinkerFlagGenerator :: LinkerFlagGenerator
gccLinkerFlagGenerator config input output =
  let xform x = case x of
        (NoLinkerConfiguration) -> []
  in (concatMap xform config) <> ["-o", output] <> [input]

gccToolchain :: Toolchain ()
gccToolchain =
  { compiler: "/sbin/g++"
  , linker: "/sbin/ld"
  , defaultCompilerConfiguration: [DontLink, GenerateDependencyInformation]
  , defaultLinkerConfiguration: []
  , generateCompilerFlags: gccCompilerFlagGenerator
  , generateLinkerFlags: gccLinkerFlagGenerator
  , parseDependencies: gccParseDependencies
  }
