{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      :  Control.Monad.Trans.Journal
Description :  Journal monad transformer
Copyright   :  (c) Dimitri Sabadie
License     :  GPL-3

Maintainer  :  dimitri.sabadie@gmail.com
Stability   :  stable
Portability :  portable

-}

module Control.Monad.Trans.Journal (
    -- * LoggerT monad transformer
    LoggerT
  , runLoggerT
  , module X
  ) where

import Control.Applicative ( Applicative )
import Control.Monad.Journal.Class as X
import Control.Monad.Trans ( MonadTrans, MonadIO )
import Control.Monad.Trans.State ( StateT, get, modify, put, runStateT )
import Data.Monoid ( Monoid(..) )

newtype LoggerT w m a = LoggerT (StateT w m a) deriving (Applicative,Functor,Monad,MonadTrans,MonadIO)

instance (Monoid w, Monad m) => MonadJournal w (LoggerT w m) where
  journal = LoggerT . modify . flip mappend
  history = LoggerT get
  clear   = LoggerT (put mempty)

runLoggerT :: (Monoid w, Monad m) => LoggerT w m a -> m (a,w)
runLoggerT (LoggerT s) = runStateT s mempty
