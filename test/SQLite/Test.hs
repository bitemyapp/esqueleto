{-# LANGUAGE ScopedTypeVariables
           , FlexibleContexts
           , RankNTypes
           , TypeFamilies
           , OverloadedStrings
#-}

module Main (main) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger (runStderrLoggingT, runNoLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist.Sqlite (withSqliteConn)
import Database.Sqlite (SqliteException)
import Database.Esqueleto hiding (random_)
import Database.Esqueleto.SQLite (random_)
import qualified Control.Monad.Trans.Resource as R
import Test.Hspec

import Common.Test




testSqliteRandom :: Spec
testSqliteRandom = do
  it "works with random_" $
    run $ do
      _ <- select $ return (random_ :: SqlExpr (Value Int))
      return ()





testSqliteSum :: Spec
testSqliteSum = do
  it "works with sum_" $
    run $ do
      _ <- insert' p1
      _ <- insert' p2
      _ <- insert' p3
      _ <- insert' p4
      ret <- select $
             from $ \p->
             return $ joinV $ sum_ (p ^. PersonAge)
      liftIO $ ret `shouldBe` [ Value $ Just (36 + 17 + 17 :: Int) ]





testSqliteTwoAscFields :: Spec
testSqliteTwoAscFields = do
  it "works with two ASC fields (one call)" $
    run $ do
      p1e <- insert' p1
      p2e <- insert' p2
      p3e <- insert' p3
      p4e <- insert' p4
      ret <- select $
             from $ \p -> do
             orderBy [asc (p ^. PersonAge), asc (p ^. PersonName)]
             return p
      -- in SQLite and MySQL, its the reverse
      liftIO $ ret `shouldBe` [ p2e, p4e, p3e, p1e ]





testSqliteOneAscOneDesc :: Spec
testSqliteOneAscOneDesc = do
  it "works with one ASC and one DESC field (two calls)" $
    run $ do
      p1e <- insert' p1
      p2e <- insert' p2
      p3e <- insert' p3
      p4e <- insert' p4
      ret <- select $
             from $ \p -> do
             orderBy [desc (p ^. PersonAge)]
             orderBy [asc (p ^. PersonName)]
             return p
      liftIO $ ret `shouldBe` [ p1e, p4e, p3e, p2e ]





testSqliteCoalesce :: Spec
testSqliteCoalesce = do
  it "throws an exception on SQLite with <2 arguments" $
    run (select $
         from $ \p -> do
         return (coalesce [p ^. PersonAge]) :: SqlQuery (SqlExpr (Value (Maybe Int))))
    `shouldThrow` (\(_ :: SqliteException) -> True)





testSqliteUpdate :: Spec
testSqliteUpdate = do
  it "works on a simple example" $
    run $ do
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
      liftIO $ n `shouldBe` 2
      liftIO $ ret `shouldBe` [ Entity p2k (Person anon Nothing (Just 37) 2)
                              , Entity p1k (Person anon (Just 73) Nothing 1)
                              , Entity p3k p3 ]





nameContains :: (BaseBackend backend ~ SqlBackend,
                 BackendCompatible SqlBackend backend,
                 Esqueleto query expr backend, MonadIO m, SqlString s,
                 IsPersistBackend backend, PersistQueryRead backend,
                 PersistUniqueRead backend)
             => (SqlExpr (Value [Char])
             -> expr (Value s)
             -> SqlExpr (Value Bool))
             -> s
             -> [Entity Person]
             -> ReaderT backend m ()
nameContains f t expected = do
  ret <- select $
         from $ \p -> do
         where_ (f
                  (p ^. PersonName)
                  ((%) ++. val t ++. (%)))
         orderBy [asc (p ^. PersonName)]
         return p
  liftIO $ ret `shouldBe` expected

testSqliteTextFunctions :: Spec
testSqliteTextFunctions = do
  describe "text functions" $ do
    it "like, (%) and (++.) work on a simple example" $
       run $ do
         [p1e, p2e, p3e, p4e] <- mapM insert' [p1, p2, p3, p4]
         nameContains like "h"  [p1e, p2e]
         nameContains like "i"  [p4e, p3e]
         nameContains like "iv" [p4e]





main :: IO ()
main = do
  hspec $ do
    tests run

    describe "Test SQLite locking" $ do
      testLocking withConn

    describe "SQLite specific tests" $ do
      testAscRandom random_ run
      testRandomMath run
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
