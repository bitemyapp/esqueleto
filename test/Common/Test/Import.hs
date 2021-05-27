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

import Control.Monad.Fail
import Common.Test.Models as X
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Logger (MonadLogger(..), MonadLoggerIO(..))
import Database.Esqueleto.Experimental as X
import Test.Hspec as X
import UnliftIO as X
import qualified UnliftIO.Resource as R

type RunDbMonad m =
    ( MonadUnliftIO m
    , MonadIO m
    , MonadLoggerIO m
    , MonadLogger m
    , MonadCatch m
    )

type Run = forall a. (forall m. (RunDbMonad m, MonadFail m) => SqlPersistT (R.ResourceT m) a) -> IO a

type WithConn m a = RunDbMonad m => (SqlBackend -> R.ResourceT m a) -> m a
