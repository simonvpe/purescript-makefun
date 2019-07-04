module Main where

import Prelude

import Effect.Aff (Aff(..), makeAff, launchAff_, effectCanceler)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Array (concatMap, sort)
import Data.Posix.Signal (Signal(..))
import Effect (Effect)
import Effect.Console (logShow, log, error)
import Node.Buffer as Buffer
import Node.ChildProcess (Exit(..), pipe, spawn, SpawnOptions(..), defaultSpawnOptions, onExit, stdout, stderr, onError, toStandardError, kill)
import Node.Encoding (Encoding(UTF8))
import Node.Path (FilePath)
import Node.Stream (onData)
-- import Node.Yargs.Applicative (yarg, runY)
-- import Node.Yargs.Setup (example, usage)

--|
--| LIBRARY
--|

type Name = String

data Target =
  Executable
  { name :: Name
  , sources :: Array FilePath
  }

executable :: Name -> Array FilePath -> Maybe Target
executable name sources = Just $ Executable { name: name, sources: sources }

--|
--| LIBRARY
--|

class CompilerArg a where
  expand :: a -> Array String
  expandAll :: Array a -> Array String

type Toolchain c r = CompilerArg c =>
                     { executable :: FilePath
                     , defaultArgs :: Array c | r}

newtype CompileDefinition = CompileDefinition
                         { dummy :: Int
                         }

compile :: forall c r. CompilerArg c => Toolchain c r -> Array c -> Aff (Either String String)
compile toolchain extraArgs =
  let
    spawnAff :: String -> Array String -> SpawnOptions -> Aff Exit
    spawnAff cmd args opts = makeAff \cb -> do
      process <- spawn cmd args opts
      onData (stdout process) (Buffer.toString UTF8 >=> log)
      onData (stderr process) (Buffer.toString UTF8 >=> error)
      onError process $ cb <<< Left <<< toStandardError
      onExit process \exit -> do
        cb <<< pure $ exit
      pure <<< effectCanceler <<< void $ kill SIGTERM process
    args = expandAll (toolchain.defaultArgs <> extraArgs)
    options = defaultSpawnOptions { stdio = pipe }
  in do
    result <- spawnAff toolchain.executable args options
    pure case result of
      Normally 0 -> Right "SUCCESS"
      _ -> Left "FAILURE"
--|
--| Gcc
--|

data GccArg = Output FilePath | IncludeDirectory FilePath | LibraryDirectory FilePath | DontLink | Source FilePath
derive instance instanceEqGccArg :: Eq GccArg
derive instance instanceOrdGccArg :: Ord GccArg

instance expandGccArg :: CompilerArg GccArg where
  expand arg = case arg of
    (Output path)           -> ["-o", path]
    (Source path)           -> [path]
    (IncludeDirectory path) -> ["-I", path]
    (LibraryDirectory path) -> ["-L", path]
    (DontLink)              -> ["-c"]
  expandAll args = concatMap expand (sort args)

gccDefaultExecutable :: FilePath
gccDefaultExecutable = "/sbin/g++"

gccDefaultArgs :: Array GccArg
gccDefaultArgs = [DontLink]

gccToolchain :: { executable :: FilePath, defaultArgs :: Array GccArg}
gccToolchain = { executable: gccDefaultExecutable, defaultArgs: gccDefaultArgs}


-- gccDefaultArgs = expandAll [DontLink]

-- type GccToolchain = Toolchain { test :: Int }
-- gcc :: GccToolchain
-- gcc = { executable: "/sbin/g++", args: expandAll [DontLink], test: 1 }

--|
--| APPLICATION
--|

myapp = executable "myapp" ["test.cpp"]

app :: Array String -> String -> Effect Unit
app [] _ = pure unit
app files _ = logShow files
app files _ = logShow files

main :: Effect Unit
main = do
  -- let setup = usage "$0 -f makefun-file" <> example "$0 -f myproject.makefun" "Say hello!"
  -- runY setup
  --   $ app
  --   <$> yarg "f" ["file"] (Just "Path to a makefun file") (Right "At least one makefun file is required") true
  --   <*> yarg "t" ["toolchain"] (Just "A toolchain") (Right "At least one toolchain is required") true
  launchAff_ (compile gccToolchain [Output "test.o", Source "test.cpp"])

