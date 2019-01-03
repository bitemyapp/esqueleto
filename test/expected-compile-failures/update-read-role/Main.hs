{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Main where

import           Control.Monad.IO.Class               (MonadIO)
import           Database.Esqueleto
import           Database.Esqueleto.Internal.Language (Insertion)
import           Database.Persist.Sql                 (SqlWriteT)
import           Database.Persist.TH                  (mkDeleteCascade,
                                                       mkMigrate, mkPersist,
                                                       persistLowerCase, share,
                                                       sqlSettings)

import           Lib

main :: IO ()
main = pure ()

updateQuery :: SqlExpr (Entity Person) -> SqlQuery ()
updateQuery = \p -> do
    set p [ PersonAge =. just (val 123) -. p ^. PersonBorn ]
    where_ $ isNothing (p ^. PersonAge)

shouldFail :: MonadIO m => SqlReadT m ()
shouldFail = update updateQuery
