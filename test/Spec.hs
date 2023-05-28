module Main where

import Test.Hspec
import Test.Hspec.Core.Spec

import qualified MySQL.LegacyTest as LegacyMySQL
import qualified MySQL.Test as MySQL
import qualified PostgreSQL.LegacyTest as LegacyPostgres
import qualified PostgreSQL.Test as Postgres
import qualified SQLite.LegacyTest as LegacySQLite
import qualified SQLite.Test as SQLite

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    parallel $ describe "Esqueleto" $ do
        describe "Legacy SQLite" $ do
            sequential $ LegacySQLite.spec
        describe "SQLite" $ do
            sequential $ SQLite.spec
        describe "Legacy MySQL" $ do
            sequential $ LegacyMySQL.spec
        describe "MySQL" $ do
            sequential $ MySQL.spec
        describe "Legacy Postgresql" $ do
            sequential $ LegacyPostgres.spec
        describe "Postgresql" $ do
            sequential $ Postgres.spec

