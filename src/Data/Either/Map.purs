module Data.Either.Map (mapBoth, mapRight, mapLeft) where

import Data.Either (Either(..))

mapBoth :: forall a b c d. (a -> c) -> (b -> d) -> Either a b -> Either c d
mapBoth f _ (Left x)  = Left (f x)
mapBoth _ f (Right x) = Right (f x)

mapRight :: forall a b c. (b -> c) -> Either a b -> Either a c
mapRight = mapBoth (\x -> x)

mapLeft :: forall a b c. (a -> c) -> Either a b -> Either c b
mapLeft f = mapBoth f (\x -> x)
