module Main where

import Prelude (Unit, pure, unit, ($), (<>), map, flip, discard)

import Toolchain (Toolchain(..), CompilerConfiguration, compileAll, compile)
import Data.Either (Either)
import GccToolchain (gccToolchain)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Console (logShow)
import Node.Path (FilePath)

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
                            ]
                 }

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
  -- launchAff_ (compile gccToolchain [Output "test.o", Source "test.cpp"])
  -- logShow (compileAll gccToolchain ["test-src/a.cpp"] "/tmp" [])
  launchAff_ $ compileAll gccToolchain { sources: ["test-src/a.cpp", "test-src/b.cpp"]
                                       , buildDir: "/tmp"
                                       , buildExtension: ".o"
                                       , compilerConfiguration: []
                                       }
  --logShow "Hello"

