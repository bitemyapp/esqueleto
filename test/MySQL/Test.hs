{-# LANGUAGE ScopedTypeVariables
           , FlexibleContexts
           , RankNTypes
#-}

module Main (main) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger (MonadLogger(..), runStderrLoggingT, runNoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Database.Persist.MySQL ( withMySQLConn
                              , connectHost
                              , connectDatabase
                              , connectUser
                              , connectPassword
                              , defaultConnectInfo)
import Database.Esqueleto
import qualified Control.Monad.Trans.Resource as R
import Test.Hspec

import Common.Test

-------------------------------------------------------------------------------


testMysqlRandom :: Spec
testMysqlRandom = do
  it "works with random_" $
    run $ do
      _ <- select $ return (random_ :: SqlExpr (Value Double))
      return ()


-------------------------------------------------------------------------------


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


-------------------------------------------------------------------------------


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


-------------------------------------------------------------------------------


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


-------------------------------------------------------------------------------


testMysqlCoalesce :: Spec
testMysqlCoalesce = do
  it "works on PostgreSQL and MySQL with <2 arguments" $
    run $ do
      _ :: [Value (Maybe Int)] <-
        select $
        from $ \p -> do
        return (coalesce [p ^. PersonAge])
      return ()


-------------------------------------------------------------------------------


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


-------------------------------------------------------------------------------


main :: IO ()
main = do
  hspec $ do
    tests run

    describe "Test MySQL locking" $ do
      testLocking withConn

    describe "MySQL specific tests" $ do
      testMysqlRandom
      testMysqlSum
      testMysqlTwoAscFields
      testMysqlOneAscOneDesc
      testMysqlCoalesce
      testMysqlUpdate


-------------------------------------------------------------------------------


run, runSilent, runVerbose :: Run
runSilent  act = runNoLoggingT     $ run_worker act
runVerbose act = runStderrLoggingT $ run_worker act
run =
  if verbose
  then runVerbose
  else runSilent


verbose :: Bool
verbose = True


run_worker :: RunDbMonad m => SqlPersistT (R.ResourceT m) a -> m a
run_worker act = withConn $ runSqlConn (migrateIt >> act)


migrateIt :: RunDbMonad m => SqlPersistT (R.ResourceT m) ()
migrateIt = do
  void $ runMigrationSilent migrateAll
  cleanDB


withConn :: RunDbMonad m => (SqlBackend -> R.ResourceT m a) -> m a
withConn =
  R.runResourceT .
  withMySQLConn defaultConnectInfo
    { connectHost     = "localhost"
    , connectUser     = "esqutest"
    , connectPassword = "esqutest"
    , connectDatabase = "esqutest"
    }
