module App
       ( App(..)
       , AppMonad
       , Error
       , run
       , performError
       , performAff
       , performEffect
       , module App.Config
       , module Control.Bind
       , module Control.Monad.Error.Class
       , module Control.Monad.Except.Trans
       , module Control.Monad.Reader.Class
       , module Control.Monad.Reader.Trans
       , module Control.Monad.Trans.Class
       , module Data.Either
       , module Data.Newtype
       , module Effect.Aff
       , module Effect.Class
       , module Effect
       ) where

-- | https://thoughtbot.com/blog/refactoring-to-a-monad-transformer-stack

import App.Config (Config(..), cArtifactsPath, cBinaryPath, cDependPath, cObjectPath, cSymlinkPath, cChecksumPath)
import Control.Bind (class Bind)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Except.Trans (ExceptT, class MonadError, class MonadThrow, runExceptT, throwError)
import Control.Monad.Reader.Class (class MonadReader)
import Control.Monad.Reader.Trans (ReaderT, class MonadReader, class MonadAsk, runReaderT, ask)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..), either)
import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, ($), (>>>))

type Error = String

type AppMonad a = ReaderT Config (ExceptT Error Aff) a
newtype App a = App (AppMonad a)

run :: forall a. App a -> Config -> Aff (Either Error a)
run (App app) config = runExceptT $ runReaderT app config

performError :: forall a. ExceptT Error Aff a -> App a
performError = lift >>> App

performAff :: forall a. Aff a -> App a
performAff = lift >>> lift >>> App

performEffect :: forall a. Effect a -> App a
performEffect = liftEffect >>> performAff

derive newtype instance bindApp :: Bind App
derive newtype instance applyApp :: Apply App
derive newtype instance functorApp :: Functor App
derive newtype instance applicativeApp :: Applicative App
derive newtype instance monadApp :: Monad App
derive newtype instance monadEffectApp :: MonadEffect App
derive newtype instance monadAskApp :: MonadAsk Config App
derive newtype instance monadeReaderApp :: MonadReader Config App
derive newtype instance monadThrowApp :: MonadThrow String App
derive newtype instance monadErrorApp :: MonadError String App

