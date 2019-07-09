module Main where

import Build
import Effect (Effect)
import Effect.Aff(Aff, launchAff)
import Effect.Console(log, logShow)
import Prelude (Unit, void, ($), bind, pure, unit)
import Target (Target(..))
import Toolchain (CompilerConfiguration(..))
import Toolchain.Gcc (gccToolchain)
import Data.Either(Either(..))
import Effect.Class (liftEffect)

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

app :: Aff Unit
app = do
  let builddir = "b"
  buildRes <- build gccToolchain builddir 8 exe 
  case buildRes of
    Left err -> liftEffect $ log err
    Right ok -> do
      linkRes <- link gccToolchain builddir exe
      case linkRes of
        Left err -> liftEffect $ log err
        Right ok -> pure unit

main :: Effect Unit
main = do
  void $ launchAff $ app
