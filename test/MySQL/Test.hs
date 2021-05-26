{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Control.Applicative
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger (runNoLoggingT, runStderrLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Resource as R
import Database.Esqueleto
import Database.Esqueleto.Experimental hiding (from, on)
import qualified Database.Esqueleto.Experimental as Experimental
import Database.Persist.MySQL
       ( connectDatabase
       , connectHost
       , connectPassword
       , connectPort
       , connectUser
       , defaultConnectInfo
       , withMySQLConn
       )

import System.Environment
import Test.Hspec

import Common.Test


-- testMysqlRandom :: Spec
-- testMysqlRandom = do
--   -- This is known not to work until
--   -- we can differentiate behavior by database
--   it "works with random_" $
--     run $ do
--       _ <- select $ return (random_ :: SqlExpr (Value Double))
--       return ()




testMysqlSum :: Spec
testMysqlSum = do
  it "works with sum_" $
    run $ do
      _ <- insert' p1
      _ <- insert' p2
      _ <- insert' p3
      _ <- insert' p4
      ret <- select $
             from $ \p->
             return $ joinV $ sum_ (p ^. PersonAge)
      liftIO $ ret `shouldBe` [ Value $ Just (36 + 17 + 17 :: Double ) ]




testMysqlTwoAscFields :: Spec
testMysqlTwoAscFields = do
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
      liftIO $ ret `shouldBe` [ p2e, p4e, p3e, p1e ]




testMysqlOneAscOneDesc :: Spec
testMysqlOneAscOneDesc = do
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




testMysqlCoalesce :: Spec
testMysqlCoalesce = do
  it "works on PostgreSQL and MySQL with <2 arguments" $
    run $ do
      _ :: [Value (Maybe Int)] <-
        select $
        from $ \p -> do
        return (coalesce [p ^. PersonAge])
      return ()




testMysqlUpdate :: Spec
testMysqlUpdate = do
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
      -- MySQL: nulls appear first, and update returns actual number
      --        of changed rows
      liftIO $ n `shouldBe` 1
      liftIO $ ret `shouldBe` [ Entity p2k (Person anon Nothing (Just 37) 2)
                              , Entity p1k (Person anon (Just 73) Nothing 1)
                              , Entity p3k p3 ]




nameContains :: (BaseBackend backend ~ SqlBackend,
                 BackendCompatible SqlBackend backend,
                 MonadIO m, SqlString s,
                 IsPersistBackend backend, PersistQueryRead backend,
                 PersistUniqueRead backend)
             => (SqlExpr (Value [Char])
             -> SqlExpr (Value s)
             -> SqlExpr (Value Bool))
             -> s
             -> [Entity Person]
             -> ReaderT backend m ()
nameContains f t expected = do
  ret <- select $
         from $ \p -> do
         where_ (f
                  (p ^. PersonName)
                  (concat_ [(%), val t, (%)]))
         orderBy [asc (p ^. PersonName)]
         return p
  liftIO $ ret `shouldBe` expected


testMysqlTextFunctions :: Spec
testMysqlTextFunctions = do
  describe "text functions" $ do
    it "like, (%) and (++.) work on a simple example" $
       run $ do
         [p1e, p2e, p3e, p4e] <- mapM insert' [p1, p2, p3, p4]
         nameContains like "h"  [p1e, p2e]
         nameContains like "i"  [p4e, p3e]
         nameContains like "iv" [p4e]


testMysqlUnionWithLimits :: Spec
testMysqlUnionWithLimits = do
  describe "MySQL Union" $ do
    it "supports limit/orderBy by parenthesizing" $ do
      run $ do
        mapM_ (insert . Foo) [1..6]

        let q1 = do
              foo <- Experimental.from $ Table @Foo
              where_ $ foo ^. FooName <=. val 3
              orderBy [asc $ foo ^. FooName]
              limit 2
              pure $ foo ^. FooName

        let q2 = do
              foo <- Experimental.from $ Table @Foo
              where_ $ foo ^. FooName >. val 3
              orderBy [asc $ foo ^. FooName]
              limit 2
              pure $ foo ^. FooName


        ret <- select $ Experimental.from $ q1 `union_` q2
        liftIO $ ret `shouldMatchList` [Value 1, Value 2, Value 4, Value 5]


main :: IO ()
main = do
  hspec $ do
    tests run

    describe "Test MySQL locking" $ do
      testLocking withConn

    describe "MySQL specific tests" $ do
      -- definitely doesn't work at the moment
      -- testMysqlRandom
      testMysqlSum
      testMysqlTwoAscFields
      testMysqlOneAscOneDesc
      testMysqlCoalesce
      testMysqlUpdate
      testMysqlTextFunctions
      testMysqlUnionWithLimits




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
  cleanDB


withConn :: RunDbMonad m => (SqlBackend -> R.ResourceT m a) -> m a
withConn f = do
    ci <- liftIO isCI
    let connInfo
            | ci =
                defaultConnectInfo
                    { connectHost     = "127.0.0.1"
                    , connectUser     = "travis"
                    , connectPassword = "esqutest"
                    , connectDatabase = "esqutest"
                    , connectPort     = 33306
                    }
            | otherwise =
                defaultConnectInfo
                    { connectHost     = "localhost"
                    , connectUser     = "travis"
                    , connectPassword = "esqutest"
                    , connectDatabase = "esqutest"
                    , connectPort     = 3306
                    }
    R.runResourceT $ withMySQLConn connInfo f

isCI :: IO Bool
isCI =  do
    env <- getEnvironment
    return $ case lookup "TRAVIS" env <|> lookup "CI" env of
        Just "true" -> True
        _ -> False

