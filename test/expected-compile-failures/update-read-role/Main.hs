{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Main where

import Control.Monad.IO.Class (MonadIO)
import Database.Persist.Sql (SqlWriteT)
import           Database.Esqueleto

import           Database.Esqueleto.Internal.Language (Insertion)
import           Database.Persist.TH                  (mkDeleteCascade,
                                                       mkMigrate, mkPersist,
                                                       persistLowerCase, share,
                                                       sqlSettings)

import Lib

main :: IO ()
main = pure ()

updateQuery :: SqlExpr (Entity Person) -> SqlQuery ()
updateQuery = \p -> do
    set p [ PersonAge =. just (val 123) -. p ^. PersonBorn ]
    where_ $ isNothing (p ^. PersonAge)

-- Currently gives the error:
--
--     /home/matt/Projects/esqueleto/test/expected-compile-failures/update-read-role/Main.hs:26:14
-- : error:
--         • Couldn't match type ‘backend’ with ‘SqlBackend’
--             arising from a use of ‘update’
--           ‘backend’ is a rigid type variable bound by
--             the type signature for:
--               shouldFail :: SqlReadT m ()
--             at update-read-role/Main.hs:26:1-31
--         • In the expression: update updateQuery
--           In an equation for ‘shouldFail’: shouldFail = update updateQuery
--        |
--     26 | shouldFail = update updateQuery
--        |              ^^^^^^^^^^^^^^^^^^
shouldFail :: MonadIO m => SqlReadT m ()
shouldFail = update updateQuery
