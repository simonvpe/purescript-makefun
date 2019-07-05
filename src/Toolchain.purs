module Toolchain where

import Prelude

import Data.Either (Either(..))
import Data.Posix.Signal (Signal(..))
import Data.JSDate
import Effect.Aff (Aff, effectCanceler, makeAff)
import Effect.Class (liftEffect)
import Effect.Console (log, error)
import Effect (Effect)
import Node.Buffer as Buffer
import Node.ChildProcess (Exit(..), SpawnOptions, pipe, spawn, defaultSpawnOptions, onExit, stdout, stderr, onError, toStandardError, kill)
import Node.Encoding (Encoding(UTF8))
import Node.Path (FilePath, parse, dirname)
import Node.Stream (onData)
import Data.Array (zip, filter)
import Node.Path (FilePath, concat)
import Data.Traversable (sequence, fold, intercalate)
import Data.Tuple
import Node.FS.Sync (stat, exists, mkdir)
import Node.FS.Stats (modifiedTime)
import Ansi.Output (withGraphics, foreground)
import Ansi.Codes (Color(..))

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
    spawnAff cmd arguments opts = makeAff \cb -> do
      log $ withGraphics (foreground Green) (intercalate " " $ [cmd] <> args)
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
    outputDirExists <- liftEffect $ exists (dirname output)
    if not outputDirExists then liftEffect $ mkdir (dirname output) else pure unit
    spawnAff toolchain.compiler args options

outputPath :: String -> FilePath -> FilePath
outputPath extension source =
  let parsed = parse source
  in concat [parsed.dir, parsed.name] <> extension

type CompileSpec r =
  { sources :: Array FilePath
  , buildExtension :: FilePath
  , compilerConfiguration :: Array CompilerConfiguration
  | r}

compileAll :: forall t u. Toolchain t -> CompileSpec u -> Aff (Array Exit)
compileAll toolchain spec =
  let inout = zip spec.sources $ map (outputPath spec.buildExtension) spec.sources
  in sequence $ map (\x -> compile toolchain spec.compilerConfiguration (fst x) (snd x)) inout

needsRecompile' :: String -> FilePath -> Effect Boolean
needsRecompile' extension source =
  let output = outputPath extension source
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

needsRecompile :: String -> Array FilePath -> Effect (Array FilePath)
needsRecompile extension sources = do
  indicators <- sequence $ map (needsRecompile' extension) sources
  pure $ map fst $ filter snd $ zip sources indicators
