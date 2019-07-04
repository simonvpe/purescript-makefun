module Toolchain where

import Prelude

import Data.Either (Either(..))
import Data.Posix.Signal (Signal(..))
import Data.JSDate
import Effect.Aff (Aff, effectCanceler, makeAff)
import Effect.Console (log, error, logShow)
import Effect (Effect)
import Node.Buffer as Buffer
import Node.ChildProcess (Exit(..), SpawnOptions, pipe, spawn, defaultSpawnOptions, onExit, stdout, stderr, onError, toStandardError, kill)
import Node.Encoding (Encoding(UTF8))
import Node.Path (FilePath, parse)
import Node.Stream (onData)
import Data.Array (zip, filter)
import Node.Path (FilePath, concat)
import Data.Traversable (sequence)
import Data.Tuple
import Node.FS.Sync (stat, exists)
import Node.FS.Stats (modifiedTime)

data CompilerConfiguration
  = DontLink
  | IncludeDirectory FilePath
  | LibraryDirectory FilePath
  | NoCompilerConfiguration

type CompilerOutput = FilePath

type CompilerInput = FilePath

type CompilerFlagGenerator = Array CompilerConfiguration -> CompilerInput -> CompilerOutput -> Array String

data LinkerConfiguration
  = NoLinkerConfiguration

type LinkerOutput = FilePath

type LinkerInput = FilePath

type LinkerFlagGenerator = Array LinkerConfiguration -> LinkerInput -> LinkerOutput -> Array String

type Toolchain r =
  { compiler :: FilePath
  , linker :: FilePath
  , defaultCompilerConfiguration :: Array CompilerConfiguration
  , defaultLinkerConfiguration :: Array LinkerConfiguration
  , generateCompilerFlags :: CompilerFlagGenerator
  , generateLinkerFlags :: LinkerFlagGenerator
  | r}

compile :: forall r. Toolchain r -> Array CompilerConfiguration -> CompilerInput -> CompilerOutput -> Aff Exit
compile toolchain extraArgs input output =
  let
    spawnAff :: String -> Array String -> SpawnOptions -> Aff Exit
    spawnAff cmd arguments opts = makeAff \cb -> do
      process <- spawn cmd arguments opts
      onData (stdout process) (Buffer.toString UTF8 >=> log)
      onData (stderr process) (Buffer.toString UTF8 >=> error)
      onError process $ cb <<< Left <<< toStandardError
      onExit process \exit -> do
        cb <<< pure $ exit
      pure <<< effectCanceler <<< void $ kill SIGTERM process
    args = toolchain.generateCompilerFlags (toolchain.defaultCompilerConfiguration <> extraArgs) input output
    options = defaultSpawnOptions { stdio = pipe }
  in do
    spawnAff toolchain.compiler args options

outputPath :: FilePath -> String -> FilePath -> FilePath
outputPath directory extension source = concat [directory, (parse source).name <> extension]

type CompileSpec r =
  { sources :: Array FilePath
  , buildDir :: FilePath
  , buildExtension :: FilePath
  , compilerConfiguration :: Array CompilerConfiguration
  | r}

compileAll :: forall t u. Toolchain t -> CompileSpec u -> Aff (Array Exit)
compileAll toolchain spec =
  let inout = zip spec.sources $ map (outputPath spec.buildDir spec.buildExtension) spec.sources
  in sequence $ map (\x -> compile toolchain spec.compilerConfiguration (fst x) (snd x)) inout

needsRecompile' :: FilePath -> String -> FilePath -> Effect Boolean
needsRecompile' directory extension source =
  let output = outputPath directory extension source
  in do
    sourceStats <- stat source
    outputExists <- exists output
    res <- if outputExists then
             do
               outputStats <- stat output
               pure $ modifiedTime outputStats < modifiedTime sourceStats
           else
             do
               pure $ true
    pure $ res

needsRecompile :: FilePath -> String -> Array FilePath -> Effect (Array FilePath)
needsRecompile directory extension sources = do
  indicators <- sequence $ map (needsRecompile' directory extension) sources
  pure $ map fst $ filter snd $ zip sources indicators
