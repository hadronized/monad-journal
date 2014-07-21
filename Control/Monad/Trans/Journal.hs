{-# LANGUAGE GeneralizedNewtypeDeriving, UndecidableInstances #-}

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

import Control.Applicative ( Applicative, Alternative )
import Control.Monad ( MonadPlus )
import Control.Monad.Journal.Class as X
import Control.Monad.Trans ( MonadTrans, MonadIO, lift )
import Control.Monad.Trans.State ( StateT, get, modify, put, runStateT, mapStateT )

import Control.Monad.Reader.Class ( MonadReader(..) )
import Control.Monad.Writer.Class ( MonadWriter(..) )
import Control.Monad.State.Class  ( MonadState )
import qualified Control.Monad.State.Class as MS ( MonadState(..) )

import Data.Monoid ( Monoid(..) )

newtype JournalT w m a = JournalT (StateT w m a)
    deriving ( Applicative
             , Alternative
             , Functor
             , Monad
             , MonadTrans
             , MonadIO
             , MonadPlus
             , MonadReader r
             , MonadWriter w'
             )

instance (Monoid w, Monad m) => MonadJournal w (JournalT w m) where
  journal !w = JournalT . modify $ flip mappend w
  history = JournalT get
  clear   = JournalT (put mempty)

instance MonadState s m => MonadState s (JournalT w m) where
    get = lift MS.get
    put = lift . MS.put
    state = lift . MS.state

runJournalT :: (Monoid w, Monad m) => JournalT w m a -> m (a,w)
runJournalT (JournalT s) = runStateT s mempty
