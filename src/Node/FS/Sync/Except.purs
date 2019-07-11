module Node.FS.Sync.Except
       ( exists
       , mkdirp
       , existsOrMkdir
       , readTextFile
       , symlink
       , unlink
       ) where

import Control.Monad.Except.Trans (ExceptT, except)
import Data.Either(Either(..))
import Data.EitherR (handleEither)
import Effect (Effect)
import Effect.Aff(Aff)
import Effect.Class (liftEffect)
import Effect.Exception (try)
import Node.Encoding(Encoding)
import Node.FS (SymlinkType)
import Node.FS.Sync as Fs
import Node.FS.Sync.Mkdirp as Mkdirp
import Node.Path (FilePath)
import Prelude

exists :: FilePath -> ExceptT String Aff Boolean
exists path = mapToExceptT $ Fs.exists path

mkdirp :: FilePath -> ExceptT String Aff Unit
mkdirp path = mapToExceptT $ Mkdirp.mkdirp path

existsOrMkdir :: FilePath -> ExceptT String Aff Unit
existsOrMkdir path = exists path >>= (\e -> if e then pure unit else mkdirp path)

readTextFile :: Encoding -> FilePath -> ExceptT String Aff String
readTextFile enc path = mapToExceptT $ Fs.readTextFile enc path

symlink :: FilePath -> FilePath -> SymlinkType -> ExceptT String Aff Unit
symlink target path t = mapToExceptT $ Fs.symlink target path t

unlink :: FilePath -> ExceptT String Aff Unit
unlink path = mapToExceptT $ Fs.unlink path

mapToExceptT :: forall a. (Effect a) -> ExceptT String Aff a
mapToExceptT f = (liftEffect $ try f) >>= handleEither (show >>> Left) >>> except
