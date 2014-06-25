{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}

{- |
Module      :  Control.Monad.Journal.Class
Description :  Journal monad typeclass
Copyright   :  (c) Dimitri Sabadie
License     :  GPL-3

Maintainer  :  dimitri.sabadie@gmail.com
Stability   :  stable
Portability :  portable

-}

module Control.Monad.Journal.Class (
    -- * MonadJournal
    MonadJournal(..)
  , sink
  , absorb
  ) where

import Control.Monad ( Monad )
import Control.Monad.Trans ( MonadIO, MonadTrans, lift, liftIO )
import Control.Monad.Trans.Either ( EitherT )
import Control.Monad.Trans.Identity ( IdentityT )
import Control.Monad.Trans.List ( ListT )
import Control.Monad.Trans.Maybe ( MaybeT )
import Control.Monad.Trans.RWS ( RWST )
import Control.Monad.Trans.Reader ( ReaderT )
import Control.Monad.Trans.State ( StateT )
import Control.Monad.Trans.Writer ( WriterT )
import Data.Monoid ( Monoid, mappend, mempty )

class (Monoid w, Monad m) => MonadJournal w m | m -> w where
  -- |Log something.
  journal :: w -> m ()
  -- |Extract the logs history.
  history :: m w
  -- |Clear the logs history.
  clear :: m ()

-- |Sink all logs history through `MonadIO` then clean it.
sink :: (MonadJournal w m, MonadIO m) => (w -> IO ()) -> m ()
sink out = history >>= liftIO . out >> clear

-- |Absorb a logs history and pass around the value.
absorb :: (MonadJournal w m) => (a,w) -> m a
absorb (a,w) = journal w >> return a

instance  (Monad m, Monoid w, MonadJournal w m) => MonadJournal w (IdentityT m) where
  journal !w = lift (journal w)
  history    = lift history
  clear      = lift clear

instance  (Monad m, Monoid w, MonadJournal w m) => MonadJournal w (ListT m) where
  journal !w = lift (journal w)
  history    = lift history
  clear      = lift clear

instance  (Monad m, Monoid w, MonadJournal w m) => MonadJournal w (MaybeT m) where
  journal !w = lift (journal w)
  history    = lift history
  clear      = lift clear

instance  (Monad m, Monoid w, MonadJournal w m) => MonadJournal w (RWST r w s m) where
  journal !w = lift (journal w)
  history    = lift history
  clear      = lift clear

instance  (Monad m, Monoid w, MonadJournal w m) => MonadJournal w (ReaderT r m) where
  journal !w = lift (journal w)
  history    = lift history
  clear      = lift clear

instance  (Monad m, Monoid w, MonadJournal w m) => MonadJournal w (StateT s m) where
  journal !w = lift (journal w)
  history    = lift history
  clear      = lift clear

instance  (Monad m, Monoid w, Monoid q, MonadJournal w m) => MonadJournal w (WriterT q m) where
  journal !w = lift (journal w)
  history    = lift history
  clear      = lift clear

instance  (Monad m, Monoid w, MonadJournal w m) => MonadJournal w (EitherT e m) where
  journal !w = lift (journal w)
  history    = lift history
  clear      = lift clear
