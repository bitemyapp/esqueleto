{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module SQLite.Test where

import Common.Test.Import hiding (from, on)

import Control.Monad (void)
import Control.Monad.Logger (runNoLoggingT, runStderrLoggingT)
import Database.Esqueleto
import Database.Esqueleto.SQLite (random_)
import Database.Persist.Sqlite (createSqlitePool)
import Database.Sqlite (SqliteException)

import Common.Test

testSqliteRandom :: SpecDb
testSqliteRandom = do
    itDb "works with random_" $ do
        _ <- select $ return (random_ :: SqlExpr (Value Int))
        asserting noExceptions

testSqliteSum :: SpecDb
testSqliteSum = do
    itDb "works with sum_" $ do
        _ <- insert' p1
        _ <- insert' p2
        _ <- insert' p3
        _ <- insert' p4
        ret <- select $ do
               p <- from $ table @Person
               return $ joinV $ sum_ (p ^. PersonAge)
        asserting $ ret `shouldBe` [ Value $ Just (36 + 17 + 17 :: Int) ]





testSqliteTwoAscFields :: SpecDb
testSqliteTwoAscFields = do
    itDb "works with two ASC fields (one call)" $ do
        p1e <- insert' p1
        p2e <- insert' p2
        p3e <- insert' p3
        p4e <- insert' p4
        ret <- select $ do
               p <- from $ table @Person
               orderBy [asc (p ^. PersonAge), asc (p ^. PersonName)]
               return p
        -- in SQLite and MySQL, its the reverse
        asserting $ ret `shouldBe` [ p2e, p4e, p3e, p1e ]

testSqliteOneAscOneDesc :: SpecDb
testSqliteOneAscOneDesc = do
    itDb "works with one ASC and one DESC field (two calls)" $ do
        p1e <- insert' p1
        p2e <- insert' p2
        p3e <- insert' p3
        p4e <- insert' p4
        ret <- select $ do
               p <- from $ table @Person
               orderBy [desc (p ^. PersonAge)]
               orderBy [asc (p ^. PersonName)]
               return p
        asserting $ ret `shouldBe` [ p1e, p4e, p3e, p2e ]

testSqliteCoalesce :: SpecDb
testSqliteCoalesce = do
    itDb "throws an exception on SQLite with <2 arguments" $ do
        eres <- try $ select $ do
                p <- from $ table @Person
                return (coalesce [p ^. PersonAge]) :: SqlQuery (SqlExpr (Value (Maybe Int)))
        asserting $ case eres of
            Left (_ :: SqliteException) ->
                pure ()
            Right _ ->
                expectationFailure "Expected SqliteException with <2 args to coalesce"

testSqliteUpdate :: SpecDb
testSqliteUpdate = do
    itDb "works on a simple example" $ do
        p1k <- insert p1
        p2k <- insert p2
        p3k <- insert p3
        let anon = "Anonymous" :: String
        ()  <- update $ \p -> do
               set p [ PersonName =. val anon
                     , PersonAge *=. just (val 2) ]
               where_ (p ^. PersonName !=. val "Mike")
        n   <- updateCount $ \p -> do
               set p [ PersonAge +=. just (val 1) ]
               where_ (p ^. PersonName !=. val "Mike")
        ret <- select $ do
               p <- from $ table @Person
               orderBy [ asc (p ^. PersonName), asc (p ^. PersonAge) ]
               return p
        -- SQLite: nulls appear first, update returns matched rows.
        asserting $ do
            n `shouldBe` 2
            ret `shouldMatchList`
                [ Entity p2k (Person anon Nothing (Just 37) 2)
                , Entity p1k (Person anon (Just 73) Nothing 1)
                , Entity p3k p3
                ]

testSqliteTextFunctions :: SpecDb
testSqliteTextFunctions = do
    describe "text functions" $ do
        itDb "like, (%) and (++.) work on a simple example" $ do
            let query :: String -> SqlPersistT IO [Entity Person]
                query t =
                    select $ do
                    p <- from $ table @Person
                    where_ (like
                             (p ^. PersonName)
                             ((%) ++. val t ++. (%)))
                    orderBy [asc (p ^. PersonName)]
                    return p
            [p1e, p2e, p3e, p4e] <- mapM insert' [p1, p2, p3, p4]
            r0 <- query "h"
            r1 <- query "i"
            r2 <- query "iv"
            asserting $ do
                r0 `shouldBe` [p1e, p2e]
                r1 `shouldBe` [p4e, p3e]
                r2 `shouldBe` [p4e]

spec :: HasCallStack => Spec
spec = beforeAll mkConnectionPool $ do
    tests

    describe "SQLite specific tests" $ do
      testAscRandom random_
      testSqliteRandom
      testSqliteSum
      testSqliteTwoAscFields
      testSqliteOneAscOneDesc
      testSqliteCoalesce
      testSqliteUpdate
      testSqliteTextFunctions

mkConnectionPool :: IO ConnectionPool
mkConnectionPool = do
    conn <-
        if verbose
        then runStderrLoggingT $
            createSqlitePool ".esqueleto-test.sqlite" 4
        else runNoLoggingT $
            createSqlitePool ".esqueleto-test.sqlite" 4
    flip runSqlPool conn $ do
        migrateIt

    pure conn

verbose :: Bool
verbose = False

migrateIt :: MonadUnliftIO m => SqlPersistT m ()
migrateIt = do
  void $ runMigrationSilent migrateAll
  cleanDB
