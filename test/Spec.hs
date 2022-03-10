module Main where

import Test.Hspec
import Test.Hspec.Core.Spec

import qualified SQLite.Test as SQLite
import qualified MySQL.Test as MySQL
import qualified PostgreSQL.Test as Postgres

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    parallel $ describe "Esqueleto" $ do
        describe "SQLite" $ do
            sequential $ SQLite.spec
        describe "MySQL" $ do
            sequential $ MySQL.spec
        describe "Postgresql" $ do
            sequential $ Postgres.spec
