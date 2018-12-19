{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Main where

import           Control.Monad.IO.Class               (MonadIO)
import           Database.Esqueleto                   (SqlExpr, SqlQuery, from,
                                                       insertSelect, val, (<#),
                                                       (<&>), (^.))
import           Database.Esqueleto.Internal.Language (Insertion)
import           Database.Persist.Sql                 (SqlReadT)
import           Database.Persist.TH                  (mkDeleteCascade,
                                                       mkMigrate, mkPersist,
                                                       persistLowerCase, share,
                                                       sqlSettings)

import           Lib

main :: IO ()
main = pure ()

insertQuery :: SqlQuery (SqlExpr (Insertion BlogPost))
insertQuery =
  from $ \p ->
  return $ BlogPost <# (val "Group Blog Post") <&> (p ^. PersonId)

shouldFail :: MonadIO m => SqlReadT m ()
shouldFail = insertSelect insertQuery
