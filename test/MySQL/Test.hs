{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module MySQL.Test where

import Common.Test.Import hiding (from, on)

import Control.Applicative
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger (runNoLoggingT, runStderrLoggingT)
import Control.Monad.Trans.Reader (ReaderT, mapReaderT)
import qualified Control.Monad.Trans.Resource as R
import Database.Esqueleto hiding (from, on)
import qualified Database.Esqueleto as Experimental
import Database.Esqueleto.Legacy
import Database.Persist.MySQL
       ( connectDatabase
       , connectHost
       , connectPassword
       , connectPort
       , connectUser
       , createMySQLPool
       , defaultConnectInfo
       , withMySQLConn
       )

import Test.Hspec

import Common.Test

testMysqlSum :: SpecDb
testMysqlSum = do
  itDb "works with sum_" $ do
      _ <- insert' p1
      _ <- insert' p2
      _ <- insert' p3
      _ <- insert' p4
      ret <- select $
             from $ \p->
             return $ joinV $ sum_ (p ^. PersonAge)
      liftIO $ ret `shouldBe` [ Value $ Just (36 + 17 + 17 :: Double ) ]

testMysqlTwoAscFields :: SpecDb
testMysqlTwoAscFields = do
  itDb "works with two ASC fields (one call)" $ do
      p1e <- insert' p1
      p2e <- insert' p2
      p3e <- insert' p3
      p4e <- insert' p4
      ret <- select $
             from $ \p -> do
             orderBy [asc (p ^. PersonAge), asc (p ^. PersonName)]
             return p
      liftIO $ ret `shouldBe` [ p2e, p4e, p3e, p1e ]

testMysqlOneAscOneDesc :: SpecDb
testMysqlOneAscOneDesc = do
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
      liftIO $ ret `shouldBe` [ p1e, p4e, p3e, p2e ]




testMysqlCoalesce :: SpecDb
testMysqlCoalesce = do
  itDb "works on PostgreSQL and MySQL with <2 arguments" $ do
      _ :: [Value (Maybe Int)] <-
        select $
        from $ \p -> do
        return (coalesce [p ^. PersonAge])
      return ()




testMysqlUpdate :: SpecDb
testMysqlUpdate = do
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
      -- MySQL: nulls appear first, and update returns actual number
      --        of changed rows
      liftIO $ n `shouldBe` 1
      liftIO $ ret `shouldBe` [ Entity p2k (Person anon Nothing (Just 37) 2)
                              , Entity p1k (Person anon (Just 73) Nothing 1)
                              , Entity p3k p3 ]

nameContains :: (SqlString s)
             => (SqlExpr (Value [Char])
             -> SqlExpr (Value s)
             -> SqlExpr (Value Bool))
             -> s
             -> [Entity Person]
             -> SqlPersistT IO ()
nameContains f t expected = do
  ret <- select $
         from $ \p -> do
         where_ (f
                  (p ^. PersonName)
                  (concat_ [(%), val t, (%)]))
         orderBy [asc (p ^. PersonName)]
         return p
  liftIO $ ret `shouldBe` expected


testMysqlTextFunctions :: SpecDb
testMysqlTextFunctions = do
  describe "text functions" $ do
    itDb "like, (%) and (++.) work on a simple example" $ do
         [p1e, p2e, p3e, p4e] <- mapM insert' [p1, p2, p3, p4]
         nameContains like "h"  [p1e, p2e]
         nameContains like "i"  [p4e, p3e]
         nameContains like "iv" [p4e]


testMysqlUnionWithLimits :: SpecDb
testMysqlUnionWithLimits = do
  describe "MySQL Union" $ do
    itDb "supports limit/orderBy by parenthesizing" $ do
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

spec :: Spec
spec = beforeAll mkConnectionPool $ do
    tests

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

verbose :: Bool
verbose = False

migrateIt :: R.MonadUnliftIO m => SqlPersistT m ()
migrateIt = do
  mapReaderT R.runResourceT $ void $ runMigrationSilent migrateAll
  cleanDB

mkConnectionPool :: IO ConnectionPool
mkConnectionPool = do
    ci <- isCI
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
    pool <-
        if verbose
        then
            runStderrLoggingT $
                createMySQLPool connInfo 4
        else
            runNoLoggingT $
                createMySQLPool connInfo 4


    flip runSqlPool pool $ do
        migrateIt
        cleanDB

    pure pool
