{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.Test.Import
    ( module Common.Test.Import
    , module X
    ) where

import Test.Hspec as X
import Database.Esqueleto.Experimental as X
import Control.Monad.Catch (MonadCatch)
import UnliftIO as X
import qualified UnliftIO.Resource as R
import Control.Monad.Logger (MonadLoggerIO(..), MonadLogger(..))
import Common.Test.Models as X

type RunDbMonad m =
    ( MonadUnliftIO m
    , MonadIO m
    , MonadLoggerIO m
    , MonadLogger m
    , MonadCatch m
    )

type Run = forall a. (forall m. (RunDbMonad m, MonadFail m) => SqlPersistT (R.ResourceT m) a) -> IO a

type WithConn m a = RunDbMonad m => (SqlBackend -> R.ResourceT m a) -> m a
