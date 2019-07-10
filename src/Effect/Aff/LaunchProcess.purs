module Effect.Aff.LaunchProcess (launchProcess, Error) where

import Ansi.Codes (Color(..))
import Ansi.Output (withGraphics, foreground)
import Control.Monad.Except.Trans (ExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Posix.Signal (Signal(..))
import Data.Traversable (intercalate)
import Effect.Aff (Aff, Canceler, effectCanceler, makeAff)
import Effect.Class (liftEffect)
import Effect.Console (log, error)
import Effect.Exception as Exception
import Effect (Effect)
import Node.Buffer as Buffer
import Node.ChildProcess (Exit(..), spawn, onExit, stdout, stderr, onError, toStandardError, kill, SpawnOptions)
import Node.Encoding (Encoding(UTF8))
import Node.Stream (onData)
import Prelude

type Error = String

-- TODO: This needs exception handling

aff :: String -> Array String -> SpawnOptions -> (Either Exception.Error Exit -> Effect Unit) -> Effect Canceler
aff cmd args opts callback = do
  liftEffect $ log $ withGraphics (foreground Green) (intercalate " " $ [cmd] <> args)
  process <- spawn cmd args opts
  onData (stdout process) (Buffer.toString UTF8 >=> log)
  onData (stderr process) (Buffer.toString UTF8 >=> error)
  onError process $ callback <<< Left <<< toStandardError
  onExit process $ callback <<< pure
  pure <<< effectCanceler <<< void $ kill SIGTERM process

launchProcess :: String -> Array String -> SpawnOptions -> ExceptT Error Aff Unit
launchProcess cmd args opts = do
  exit <- lift $ makeAff $ aff cmd args opts
  case exit of
    Normally 0 -> pure unit
    Normally r -> throwError $ "exit code " <> show r
    BySignal s -> throwError $ "signal " <> show s
