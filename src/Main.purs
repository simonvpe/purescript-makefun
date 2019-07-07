module Main where

import Effect (Effect)
import Effect.Aff(Aff, launchAff)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import GccToolchain (gccCompile)
import Node.Path (FilePath)
import Prelude (Unit, bind, void, ($))
import Toolchain (parCompile)

type TargetName = String

data Target =
  Executable
  { name :: TargetName
  , sources :: Array FilePath
  }

exe :: Target
exe = Executable { name: "myapp"
                 , sources: [ "test-src/a.cpp"
                            , "test-src/b.cpp"
                            , "test-src/c.cpp"
                            ]
                 }

sources :: Target -> Array FilePath
sources target = case target of
  Executable e -> e.sources

app :: Aff Unit
app = do
  r <- parCompile gccCompile (sources exe)
  liftEffect $ logShow r

main :: Effect Unit
main = do
  void $ launchAff $ app
