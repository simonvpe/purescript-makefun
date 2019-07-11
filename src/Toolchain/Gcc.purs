module Toolchain.Gcc (gccToolchain) where

import Data.Array (concatMap)
import Data.Semigroup ((<>))
import Toolchain.Gcc.DependencyParser (gccParseDependencies)
import Toolchain (Toolchain(..))
import Toolchain.CompilerConfiguration (CompilerConfiguration(..))
import Toolchain.LinkerConfiguration (LinkerConfiguration(..))
import Node.Path (FilePath)

gccCompilerFlagGenerator :: Array CompilerConfiguration -> FilePath -> FilePath -> Array String
gccCompilerFlagGenerator config input output =
  let xform x = case x of
        (DontLink)                      -> ["-c"]
        (IncludeDirectory path)         -> ["-I", path]
        (GenerateDependencyInformation) -> ["-MMD"]
        (NoCompilerConfiguration)       -> []
  in (concatMap xform config) <> ["-o", output] <> [input]

gccLinkerFlagGenerator :: Array LinkerConfiguration -> Array FilePath -> FilePath -> Array String
gccLinkerFlagGenerator config inputs output =
  let xform x = case x of
        (LinkLibrary lib) -> ["-l" <> lib]
        (Entry entry) -> ["--entry", entry]
        (DynamicLinker linker) -> ["-dynamic-linker", linker]
        (LibraryDirectory path)         -> ["-L", path]
        (NoLinkerConfiguration) -> []
  in inputs <> ["-o", output] <> (concatMap xform config)

gccToolchain :: Toolchain
gccToolchain =
  Toolchain { compiler: "/usr/bin/g++"

  , linker: "/usr/bin/ld"

  , defaultCompilerConfiguration:
    [ DontLink
    , GenerateDependencyInformation ]

  , defaultLinkerConfiguration:
    [ DynamicLinker "/lib/ld-linux-x86-64.so.2"
    , LibraryDirectory "/usr/lib"
    , LibraryDirectory "/usr/lib/gcc/x86_64-pc-linux-gnu/8.3.0"
    , LinkLibrary "c"
    , LinkLibrary "stdc++" ]

  , generateCompilerFlags: gccCompilerFlagGenerator

  , generateLinkerFlags: gccLinkerFlagGenerator

  -- , parseDependencies: gccParseDependencies

  , extraObjects:
    [ "/lib/crt1.o"
    , "/lib/crti.o"
    , "/lib/crtn.o"
    , "/usr/lib/gcc/x86_64-pc-linux-gnu/8.3.0/crtbegin.o"
    , "/usr/lib/gcc/x86_64-pc-linux-gnu/8.3.0/crtend.o" ]
  }
