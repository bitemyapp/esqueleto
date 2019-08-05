{-# LANGUAGE FlexibleContexts
           , GADTs
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , OverloadedStrings
           , QuasiQuotes
           , RankNTypes
           , ScopedTypeVariables
           , TemplateHaskell
           , TypeFamilies
           , UndecidableInstances
 #-}
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
