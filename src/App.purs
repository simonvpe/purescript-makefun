module App
       ( Config(..)
       , App(..)
       , Error
       , run
       , module Control.Monad.Except.Trans
       , module Control.Monad.Error.Class
       , module Control.Monad.Reader.Trans
       , module Effect.Class
       , module Control.Bind
       , module Data.Newtype
       ) where

-- | https://thoughtbot.com/blog/refactoring-to-a-monad-transformer-stack

import Prelude
import Control.Monad.Except.Trans (ExceptT, class MonadError, runExceptT, throwError)
import Control.Monad.Reader.Trans (ReaderT, class MonadReader, runReaderT, ask)
import Control.Monad.Error.Class (class MonadError)
import Effect.Class (class MonadEffect, liftEffect)
import Control.Bind (class Bind)
import Data.Newtype (unwrap)
import Effect.Aff.Class
import Data.Identity
import Data.Either
import Toolchain (Toolchain)
import Effect.Aff
import Node.Path (FilePath)
import Target (Target)
import Data.Newtype (class Newtype)

type Error = String

newtype Config =
  Config { toolchain :: Toolchain
         , buildDir :: FilePath
         , nofCores :: Int
         , target :: Target }

newtype App a = App (ReaderT Config (ExceptT Error Aff) a)

run :: forall a. App a -> Config -> Aff (Either Error a)
run (App app) config = runExceptT $ runReaderT app config


derive instance newtypeConfig :: Newtype Config _
derive newtype instance bindApp :: Bind App
derive newtype instance monadEffectApp :: MonadEffect App
derive newtype instance monadeReaderApp :: MonadReader Config App
derive newtype instance monadErrorApp :: MonadError String App

