module Main where

import Data.Either (Either)
import Effect (Effect)
import Effect.Aff(Aff, launchAff)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import GccToolchain (gccToolchain)
import Node.Path (FilePath)
import Prelude (Unit, bind, void, ($), discard)
import Toolchain (Toolchain, parCompile, compile, CompilerConfiguration(..), dependencies)

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

compileTarget :: forall r. Toolchain r -> Int -> Target -> Aff (Either String (Array FilePath))
compileTarget toolchain nofThreads target =
  let compiler = compile toolchain $ compilerConfig target
  in parCompile compiler nofThreads $ sources target

app :: Aff Unit
app = do
  deps <- dependencies gccToolchain "test-src/a.cpp"
  liftEffect $ logShow deps
  r <- compileTarget gccToolchain 8 exe
  liftEffect $ logShow r

main :: Effect Unit
main = do
  void $ launchAff $ app
