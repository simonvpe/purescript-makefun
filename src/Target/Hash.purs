module Target.Hash where

import Control.Applicative (pure)
import Control.Apply ((<*>))
import Control.Bind ((>>=))
import Control.Semigroupoid ((>>>))
import Data.Functor ((<$>))
import Data.Foldable (length)
import Data.Maybe (Maybe, maybe)
import Data.BigInt as BigInt

xor :: String -> String -> Maybe String
xor a b = BigInt.xor <$> fs a <*> fs b >>= ts >>> pure
  where fs = BigInt.fromBase 16
        ts = BigInt.toBase 16

xor' :: String -> String -> String
xor' a b = maybe "" (\x -> x) (xor a b)
