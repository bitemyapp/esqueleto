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

import Control.Monad.Trans.Resource (ResourceT)
import Data.Aeson (Value)
import Database.Esqueleto (SqlExpr, delete, from)
import Database.Esqueleto.PostgreSQL.JSON (JSONB)
import Database.Persist (Entity)
import Database.Persist.Sql (SqlPersistT)
import Database.Persist.TH

import Common.Test (RunDbMonad)

-- JSON Table for PostgreSQL
share [mkPersist sqlSettings, mkMigrate "migrateJSON"] [persistUpperCase|
Json
  value (JSONB Value)
|]

cleanJSON
  :: (forall m. RunDbMonad m
  => SqlPersistT (ResourceT m) ())
cleanJSON = delete $ from $ \(_ :: SqlExpr (Entity Json)) -> return ()
