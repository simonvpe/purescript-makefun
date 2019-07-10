module Node.FS.Sync.Except where

import Control.Monad.Except.Trans (ExceptT, except)
import Data.Either(Either(..))
import Data.EitherR (handleEither)
import Effect.Aff(Aff)
import Effect.Class (liftEffect)
import Effect.Exception (try)
import Node.Encoding(Encoding)
import Node.FS.Sync as Fs
import Node.FS.Sync.Mkdirp as Mkdirp
import Node.Path (FilePath)
import Prelude
  
exists :: FilePath -> ExceptT String Aff Boolean
exists path = (liftEffect $ try (Fs.exists path)) >>= handleEither (show >>> Left) >>> except

mkdirp :: FilePath -> ExceptT String Aff Unit
mkdirp path = (liftEffect $ try (Mkdirp.mkdirp path)) >>= handleEither (show >>> Left) >>> except

existsOrMkdir :: FilePath -> ExceptT String Aff Unit
existsOrMkdir path = exists path >>= (\e -> if e then pure unit else  mkdirp path)
       
readTextFile :: Encoding -> FilePath -> ExceptT String Aff String
readTextFile enc path = (liftEffect $ try (Fs.readTextFile enc path)) >>= handleEither (show >>> Left) >>> except
