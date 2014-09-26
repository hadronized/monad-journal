{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) Dimitri Sabadie
-- License     :  BSD3
-- 
-- Maintainer  :  dimitri.sabadie@gmail.com
-- Stability   :  stable
-- Portability :  portable
--
-- 'MonadWriter' on steroids.
--
-- 'MonadJournal' is a more controlable version of 'MonadWriter' because it
-- enables you to access the 'Monoid' being computed up. You can then access
-- logs inside the computation itself, whereas you cannot with
-- 'MonadWriter' – unless you use specific functions like 'listen', but that
-- still stacks 'Monoid' in the monad.
--
-- Typically, you can use 'MonadJournal' when you come across the logging
-- problem and you need logs as long as you proceed.
-----------------------------------------------------------------------------

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

-- |This typeclass provides the ability to accumulate 'Monoid' in a monad
-- via the 'journal' function; to get them via the 'history' function and
-- finally, to purge them all with the 'clear' function.
--
-- In most cases, you won’t need 'history' neither 'clear'. There’s a 
-- cool function that combines both and enables you to deal with the
-- 'Monoid': 'sink'.
class (Monoid w, Monad m) => MonadJournal w m | m -> w where
  -- |Log something.
  journal :: w -> m ()
  -- |Extract the logs history.
  history :: m w
  -- |Clear the logs history.
  clear :: m ()

-- |Sink all logs history through 'MonadIO' then clean it.
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
