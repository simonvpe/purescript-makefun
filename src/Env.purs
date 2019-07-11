module Env (Env) where

import Effect (Effect)
import Node.Path (FilePath)
import Prelude (Unit)

type Env =
  { log :: String -> Effect Unit
  , buildDir :: FilePath
  }
