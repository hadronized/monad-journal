{-# LANGUAGE FunctionalDependencies #-}

{- |
Module      :  Control.Monad.Journal.Class
Description :  `MonadJournal` class
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
import Control.Monad.Trans ( MonadIO, liftIO )
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
