{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Blog
    ( runBlogT
    ) where

import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.IO.Unlift     (MonadUnliftIO(..), wrappedWithRunInIO)
import           Control.Monad.Logger        (MonadLogger, NoLoggingT (..))
import           Control.Monad.Reader
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..),
                                              defaultLiftBaseWith,
                                              defaultRestoreM)
import           Database.Persist.Postgresql (ConnectionString)

newtype BlogT m a = BlogT { unBlogT :: NoLoggingT (ReaderT ConnectionString m) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadLogger
           , MonadReader ConnectionString
           , MonadIO
           )

instance MonadUnliftIO m => MonadUnliftIO (BlogT m) where
  withRunInIO = wrappedWithRunInIO BlogT unBlogT

instance MonadTrans BlogT where
  lift = BlogT . lift . lift


deriving instance (MonadBase b m) => MonadBase b (BlogT m)


instance MonadBaseControl b m => MonadBaseControl b (BlogT m) where
  type StM (BlogT m) a = ComposeSt BlogT m a
  liftBaseWith     = defaultLiftBaseWith
  restoreM         = defaultRestoreM


instance MonadTransControl BlogT where
  type StT BlogT a = StT NoLoggingT (StT (ReaderT ConnectionString) a)
  liftWith f = BlogT $ liftWith $ \run ->
                                    liftWith $ \run' ->
                                                f (run' . run . unBlogT)
  restoreT = BlogT . restoreT . restoreT


runBlogT :: ConnectionString -> BlogT m a -> m a
runBlogT backend (BlogT m) =
  runReaderT (runNoLoggingT m) backend
