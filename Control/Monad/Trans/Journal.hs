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
    -- * JournalT monad transformer
    JournalT
  , runJournalT
  , module X
  ) where

import Control.Applicative ( Applicative )
import Control.Monad.Journal.Class as X
import Control.Monad.Trans ( MonadTrans, MonadIO )
import Control.Monad.Trans.State ( StateT, get, modify, put, runStateT )
import Data.Monoid ( Monoid(..) )

newtype JournalT w m a = JournalT (StateT w m a) deriving (Applicative,Functor,Monad,MonadTrans,MonadIO)

instance (Monoid w, Monad m) => MonadJournal w (JournalT w m) where
  journal !w = JournalT . modify $ flip mappend w
  history = JournalT get
  clear   = JournalT (put mempty)

runJournalT :: (Monoid w, Monad m) => JournalT w m a -> m (a,w)
runJournalT (JournalT s) = runStateT s mempty
