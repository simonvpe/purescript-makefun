module Toolchain
       ( Compiler
       , CompilerConfiguration(..)
       , CompilerFlagGenerator
       , CompilerInput
       , CompilerOutput
       , Dependency
       , DependencyParser
       , LinkerConfiguration(..)
       , LinkerFlagGenerator
       , LinkerInput
       , LinkerOutput
       , Toolchain
       , compile
       , dependencies
       , parCompile
       ) where

-- import Node.FS.Stats (modifiedTime)
import Ansi.Codes (Color(..))
import Ansi.Output (withGraphics, foreground)
import Control.Monad.Error.Class (try)
import Control.Parallel (parSequence)
import Data.Array (zip, take, drop)
import Data.Either (Either(..), either)
import Data.Posix.Signal (Signal(..))
import Data.Traversable (intercalate, sequence)
import Data.Tuple (Tuple, fst, snd)
import Effect.Aff (Aff, effectCanceler, forkAff, joinFiber, makeAff)
import Effect.Class (liftEffect)
import Effect.Console (log, error)
import Node.Buffer as Buffer
import Node.ChildProcess (Exit(..), pipe, spawn, defaultSpawnOptions, onExit, stdout, stderr, onError, toStandardError, kill)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (exists, mkdir, readTextFile)
import Node.Path (FilePath, parse, dirname, concat)
import Node.Stream (onData)
import Prelude (bind, discard, map, not, pure, show, unit, void, (#), ($), (<), (<$>), (<<<), (<>), (>=>))

data CompilerConfiguration
  = DontLink
  | IncludeDirectory FilePath
  | LibraryDirectory FilePath
  | GenerateDependencyInformation
  | NoCompilerConfiguration

type CompilerOutput = FilePath

type CompilerInput = FilePath

type CompilerFlagGenerator = Array CompilerConfiguration -> CompilerInput -> CompilerOutput -> Array String

data LinkerConfiguration
  = NoLinkerConfiguration

type LinkerOutput = FilePath

type LinkerInput = FilePath

type LinkerFlagGenerator = Array LinkerConfiguration -> LinkerInput -> LinkerOutput -> Array String

type Dependency = Tuple CompilerInput (Array FilePath)

type DependencyParser = String -> Either String Dependency

type Toolchain r =
  { compiler :: FilePath
  , linker :: FilePath
  , defaultCompilerConfiguration :: Array CompilerConfiguration
  , defaultLinkerConfiguration :: Array LinkerConfiguration
  , generateCompilerFlags :: CompilerFlagGenerator
  , generateLinkerFlags :: LinkerFlagGenerator
  , parseDependencies :: DependencyParser
  | r}

outputPath :: FilePath -> FilePath
outputPath source =
  let parsed = parse source
  in concat [parsed.dir, parsed.name] <> ".o"

dependencyListPath :: CompilerInput -> FilePath
dependencyListPath source =
  let parsed = parse source
  in concat [parsed.dir, parsed.name] <> ".d"

compile :: forall r. Toolchain r -> Array CompilerConfiguration -> CompilerInput  -> Aff Exit
compile toolchain extraArgs input =
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
    output = outputPath input
    options = defaultSpawnOptions { stdio = pipe }
  in do
    outputDirExists <- liftEffect $ exists (dirname output)
    if not outputDirExists then liftEffect $ mkdir (dirname output) else pure unit
    spawnAff toolchain.compiler args options

type Compiler = CompilerInput -> Aff Exit

parCompile' :: Compiler -> Array FilePath -> Aff(Either String (Array FilePath))
parCompile' compiler files =
  let
    exitToString file signal = case signal of
      Normally 0 -> true # Right
      Normally r -> "return code " <> show r <> " (" <> file <> ")" # Left
      BySignal x -> "signal " <> show x <> " (" <> file <> ")" # Left
  in do
    fibers <- files
              # map compiler
              # map forkAff
              # parSequence

    results <- fibers
               # map (\x -> try $ joinFiber x)
               # sequence

    pure $ case sequence results of
      Left err ->
        err # show # Left

      Right sigs -> do
        success <- (\x -> fst x # exitToString $ snd x) <$> zip files sigs # sequence
        Right files

parCompile :: Compiler -> Int -> Array FilePath -> Aff(Either String (Array FilePath))
parCompile _ _ [] = do pure $ Right []
parCompile compiler nofThreads files =
  if nofThreads < 1 then do
    "too few threads (" <> show nofThreads <> ")" # Left # pure
  else do
    left <- files # take nofThreads # parCompile' compiler
    right <- files # drop nofThreads # parCompile compiler nofThreads
    pure $ left <> right

dependencies :: forall r. Toolchain r -> FilePath -> Aff(Array FilePath)
dependencies toolchain source =
  let path = dependencyListPath source
  in do
    dependencyListExists <- liftEffect $ exists path
    if dependencyListExists
      then do
           content <- liftEffect $ readTextFile UTF8 path
           content # toolchain.parseDependencies # either (\_ -> []) snd # pure
      else [] # pure

-- needsRecompile :: FilePath -> Aff Boolean
-- needsRecompile source =
--   let output = outputPath source
--   in liftEffect do
--     sourceStats <- stat source
--     outputExists <- exists output
--     res <- if outputExists then
--              do
--                outputStats <- stat output
--                pure $ modifiedTime outputStats < modifiedTime sourceStats
--            else
--              do
--                pure $ true
--     pure $ res
