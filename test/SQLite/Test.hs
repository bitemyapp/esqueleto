{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module SQLite.Test where

import Common.Test.Import hiding (from, on)

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger (runNoLoggingT, runStderrLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Resource as R
import Database.Esqueleto hiding (random_)
import Database.Esqueleto.SQLite (random_)
import Database.Persist.Sqlite (withSqliteConn)
import Database.Sqlite (SqliteException)
import Test.Hspec

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
        ret <- select $
               from $ \p->
               return $ joinV $ sum_ (p ^. PersonAge)
        asserting $ ret `shouldBe` [ Value $ Just (36 + 17 + 17 :: Int) ]





testSqliteTwoAscFields :: SpecDb
testSqliteTwoAscFields = do
    itDb "works with two ASC fields (one call)" $ do
        p1e <- insert' p1
        p2e <- insert' p2
        p3e <- insert' p3
        p4e <- insert' p4
        ret <- select $
               from $ \p -> do
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
        ret <- select $
               from $ \p -> do
               orderBy [desc (p ^. PersonAge)]
               orderBy [asc (p ^. PersonName)]
               return p
        asserting $ ret `shouldBe` [ p1e, p4e, p3e, p2e ]

testSqliteCoalesce :: SpecDb
testSqliteCoalesce = do
    itDb "throws an exception on SQLite with <2 arguments" $ do
        eres <- try $ select $
               from $ \p -> do
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
        let anon = "Anonymous"
        ()  <- update $ \p -> do
               set p [ PersonName =. val anon
                     , PersonAge *=. just (val 2) ]
               where_ (p ^. PersonName !=. val "Mike")
        n   <- updateCount $ \p -> do
               set p [ PersonAge +=. just (val 1) ]
               where_ (p ^. PersonName !=. val "Mike")
        ret <- select $
               from $ \p -> do
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
                    select $
                    from $ \p -> do
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

spec :: Spec
spec = beforeAll undefined $ do
    tests

    describe "SQLite specific tests" $ do
      testAscRandom random_
      testRandomMath
      testSqliteRandom
      testSqliteSum
      testSqliteTwoAscFields
      testSqliteOneAscOneDesc
      testSqliteCoalesce
      testSqliteUpdate
      testSqliteTextFunctions

run, runSilent, runVerbose :: Run
runSilent  act = runNoLoggingT     $ run_worker act
runVerbose act = runStderrLoggingT $ run_worker act
run =
  if verbose
  then runVerbose
  else runSilent

verbose :: Bool
verbose = False

run_worker :: RunDbMonad m => SqlPersistT (R.ResourceT m) a -> m a
run_worker act = withConn $ runSqlConn (migrateIt >> act)

migrateIt :: RunDbMonad m => SqlPersistT (R.ResourceT m) ()
migrateIt = do
  void $ runMigrationSilent migrateAll

withConn :: RunDbMonad m => (SqlBackend -> R.ResourceT m a) -> m a
withConn =
  R.runResourceT . withSqliteConn ":memory:"
