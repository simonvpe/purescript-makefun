module Main where

import Build
import Effect (Effect)
import Effect.Aff(Aff, launchAff)
import Effect.Console(log, logShow)
import Control.Monad.Except.Trans (runExceptT)
import Prelude (Unit, void, ($), bind)
import Target (Target(..))
import Toolchain.CompilerConfiguration (CompilerConfiguration(..))
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
                 , linkerConfig:
                   [
                   ]
                 }

app :: Aff Unit
app = do
  let builddir = "b"
  buildRes <- runExceptT $ build gccToolchain builddir 8 exe
  case buildRes of
    Left err -> liftEffect $ log err
    Right _ -> do
      linkRes <- runExceptT $ link gccToolchain builddir exe
      case linkRes of
        Left err -> liftEffect $ log err
        Right ok -> liftEffect $ logShow ok

main :: Effect Unit
main = do
  void $ launchAff $ app
