module Main where

import Effect (Effect)
import Effect.Aff(Aff, launchAff)
import Effect.Console(log)
import Effect.Class (liftEffect)
import Control.Monad.Except.Trans (runExceptT)
import Prelude (Unit, void, ($), bind, discard)
import Target (Target(..))
import Toolchain.CompilerConfiguration (CompilerConfiguration(..))
import Toolchain.Gcc (gccToolchain)
import Toolchain.Build (build)
import Data.Either(Either(..))

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
                 , linkerConfig:
                   [
                   ]
                 }

app :: Aff Unit
app = do
  let builddir = "b"
  liftEffect $ log "Compiling"
  buildRes <- runExceptT $ build gccToolchain builddir 8 exe
  case buildRes of
    Left err -> liftEffect $ log err
    Right _ -> liftEffect $ log "ok"

main :: Effect Unit
main = do
  void $ launchAff $ app
