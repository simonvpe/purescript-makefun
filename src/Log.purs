module Log (log) where

import Control.Monad.Reader (class MonadReader, ask)
import Effect.Class (class MonadEffect, liftEffect)
import Env (Env)
import Prelude (Unit, bind, ($))

log :: forall m. MonadReader Env m => MonadEffect m => String -> m Unit
log message = do
  env <- ask
  liftEffect $ env.log message
