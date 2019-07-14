module Toolchain.Gcc (gccToolchain) where

import Data.Array (concatMap)
import Data.Semigroup ((<>))
import Toolchain.Gcc.DependencyParser (gccParseDependencies)
import Toolchain as Toolchain
import Toolchain.CompilerConfiguration (CompilerConfiguration(..))
import Toolchain.LinkerConfiguration (LinkerConfiguration(..))
import Node.Path (FilePath)

gccCompilerFlagGenerator :: Array CompilerConfiguration -> FilePath -> FilePath -> Array String
gccCompilerFlagGenerator config input output =
  let xform x = case x of
        CRaw args                     -> args        
        DontLink                      -> ["-c"]
        IncludeDirectory path         -> ["-I", path]
        GenerateDependencyInformation -> ["-MMD"]
        NoCompilerConfiguration       -> []
  in (concatMap xform config) <> ["-o", output] <> [input]

gccLinkerFlagGenerator :: Array LinkerConfiguration -> Array FilePath -> FilePath -> Array String
gccLinkerFlagGenerator config inputs output =
  let xform x = case x of
        LRaw args             -> args        
        LinkLibrary lib       -> ["-l" <> lib]
        Entry entry           -> ["--entry", entry]
        DynamicLinker linker  -> ["-dynamic-linker", linker]
        LibraryDirectory path -> ["-L", path]
        NoLinkerConfiguration -> []
  in (concatMap xform config) <> inputs <> ["-o", output]

gccCompiler :: Toolchain.BuildType -> FilePath
gccCompiler buildType = case buildType of
  Toolchain.Executable    -> "libtool"
  Toolchain.SharedLibrary -> "libtool"
  Toolchain.StaticLibrary -> "libtool"

gccLinker :: Toolchain.BuildType -> FilePath
gccLinker buildType = case buildType of
  Toolchain.Executable    -> "libtool"
  Toolchain.SharedLibrary -> "libtool"
  Toolchain.StaticLibrary -> "libtool"

gccDefaultCompilerConfiguration :: Toolchain.BuildType -> Array CompilerConfiguration
gccDefaultCompilerConfiguration buildType = case buildType of
  Toolchain.Executable    -> [CRaw ["--mode=compile", "--tag=CXX", "--silent", "g++"], DontLink, GenerateDependencyInformation]
  Toolchain.SharedLibrary -> []
  Toolchain.StaticLibrary -> []

gccDefaultLinkerConfiguration :: Toolchain.BuildType -> Array LinkerConfiguration
gccDefaultLinkerConfiguration buildType = case buildType of
  Toolchain.Executable    -> [LRaw ["--mode=link", "--tag=CXX", "--silent", "g++"]]
  Toolchain.SharedLibrary -> []
  Toolchain.StaticLibrary -> []

gccToolchain :: Toolchain.Toolchain
gccToolchain =
  Toolchain.Toolchain { compiler: gccCompiler
                      , linker: gccLinker
                      , defaultCompilerConfiguration: gccDefaultCompilerConfiguration
                      , defaultLinkerConfiguration: gccDefaultLinkerConfiguration
                      , generateCompilerFlags: gccCompilerFlagGenerator
                      , generateLinkerFlags: gccLinkerFlagGenerator
                      , parseDependencies: gccParseDependencies
                      , extraObjects: []
                      }
