module Main where

import Effect.Aff(Fiber, launchAff)
import Effect.Console(log)
import Prelude (Unit, ($), bind, discard, pure, unit)
import Target (Target(..))
import Target.Build (build)
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
                   ]
                 , compilerConfig:
                   [ IncludeDirectory "/usr/include"
                   ]
                 , linkerConfig:
                   [
                   ]
                 }

config :: Config
config = Config { toolchain: gccToolchain, buildDir: "b", nofCores: 2 }

entrypoint :: App Unit
entrypoint = do
  cfg <- ask
  buildRes <- build exe
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

main :: Effect (Fiber Unit)
main = launchAff do
  result <- run entrypoint config
  case result of
    Left err -> liftEffect $ log err
    Right _ -> liftEffect $ log "ok"

  pure unit
