module Main where

import Effect (Effect)
import Effect.Aff(Aff, launchAff)
import Effect.Console(log)
import Control.Monad.Except.Trans (runExceptT)
import Prelude (Unit, show, void, ($), bind, discard, pure, unit, (<>))
import Target (Target(..))
import Target.Build (build)
import Toolchain (Toolchain)
import Toolchain.CompilerConfiguration (CompilerConfiguration(..))
import Toolchain.Gcc (gccToolchain)
import Data.Either(Either(..))
import App

exe :: Target
exe = Executable { name: "myapp"
                 , sources:
                   [ "test-src/a.cpp"
                   , "test-src/b.cpp"
                   , "test-src/c.cpp"
                   , "test-src/subdir/d.cpp"
                   ]
                 , compilerConfig:
                   [ IncludeDirectory "/usr/include"
                   ]
                 , linkerConfig:
                   [
                   ]
                 }

config :: Config
config = Config { toolchain: gccToolchain, buildDir: "b", nofCores: 2, target: exe }

entrypoint :: forall m. Bind m => MonadReader Config m => MonadEffect m => MonadError String m => m Unit
entrypoint = do
  cfg <- ask
  buildRes <- build
  pure unit

-- app :: Aff Unit
-- app = do
--   _ <- App.run entrypoint config
--   let builddir = "b"
--   liftEffect $ log "Compiling"
--   buildRes <- runExceptT $ build gccToolchain builddir 8 exe
--   case buildRes of
--     Left err -> liftEffect $ log err
--     Right _ -> liftEffect $ log "ok"

main :: Effect Unit
main = do
  result <- launchAff $ run entrypoint config
  pure unit
