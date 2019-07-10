module Node.Crypto.Hash.Except (hex) where

import Control.Monad.Except.Trans (ExceptT, except)
import Data.Either(Either(..))
import Data.EitherR (handleEither)
import Effect.Aff(Aff)
import Effect.Class (liftEffect)
import Effect.Exception (try)
import Node.Crypto.Hash as Hash
import Prelude

hex :: Hash.Algorithm -> String -> ExceptT String Aff String
hex algo input = (liftEffect $ try (Hash.hex algo input)) >>= handleEither (show >>> Left) >>> except
