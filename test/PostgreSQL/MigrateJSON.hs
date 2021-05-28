{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module PostgreSQL.MigrateJSON where

import Common.Test.Import hiding (Value, from, on)

import Data.Aeson (Value)
import Database.Esqueleto.Legacy (from)
import Database.Esqueleto.PostgreSQL.JSON (JSONB)
import Database.Persist.TH

-- JSON Table for PostgreSQL
share [mkPersist sqlSettings, mkMigrate "migrateJSON"] [persistUpperCase|
Json
  value (JSONB Value)
  deriving Show
|]

cleanJSON
    :: forall m. MonadIO m
    => SqlPersistT m ()
cleanJSON = delete $ from $ \(_ :: SqlExpr (Entity Json)) -> return ()
