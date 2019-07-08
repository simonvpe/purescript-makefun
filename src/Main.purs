module Main where

import Control.Applicative (pure)
import Data.Either (Either)
import Data.Array (zip)
import Effect (Effect)
import Effect.Aff(Aff, launchAff)
import Effect.Class (liftEffect)
import Effect.Console (logShow, log)
import Toolchain.Gcc (gccToolchain)
import Node.Path (FilePath)
import Prelude (Unit, bind, void, ($), discard, (<$>), (#), (<>), show)
import Toolchain (Dependency, Toolchain, parCompile, compile, CompilerConfiguration(..), dependencies)
import Cache (load, store)
import Data.Traversable (traverse)

type TargetName = String

data Target =
  Executable
  { name :: TargetName
  , sources :: Array FilePath
  , compilerConfig :: Array CompilerConfiguration
  }

exe :: Target
exe = Executable { name: "myapp"
                 , sources:
                   [ "test-src/a.cpp"
                   , "test-src/b.cpp"
                   , "test-src/c.cpp"
                   ]
                 , compilerConfig:
                   [ IncludeDirectory "/usr/include"
                   ]
                 }

sources :: Target -> Array FilePath
sources target = case target of
  Executable e -> e.sources

compilerConfig :: Target -> Array CompilerConfiguration
compilerConfig target = case target of
  Executable e -> e.compilerConfig

dependencies' :: forall r. Toolchain r -> Target -> Aff (Array Dependency)
dependencies' toolchain target =
  let src = sources target
  in do
    deps <- traverse (dependencies toolchain) src
    zip src deps # pure

compileTarget :: forall r. Toolchain r -> Int -> Target -> Aff (Either String (Array FilePath))
compileTarget toolchain nofThreads target =
  let compiler = compile toolchain $ compilerConfig target
  in parCompile compiler nofThreads $ sources target

app :: Aff Unit
app =
  let src = sources exe
  in do
    deps <- dependencies' gccToolchain exe
    liftEffect $ (log $ "deps = " <> show (zip src deps))
    r <- compileTarget gccToolchain 8 exe
    cache <- load "cache"
    _ <- store cache "cache2"
    liftEffect $ logShow cache

main :: Effect Unit
main = do
  void $ launchAff $ app
