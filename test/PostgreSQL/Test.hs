{-# OPTIONS_GHC -fno-warn-unused-binds  #-}
{-# LANGUAGE ConstraintKinds
           , EmptyDataDecls
           , FlexibleContexts
           , FlexibleInstances
           , DeriveGeneric
           , GADTs
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , OverloadedStrings
           , QuasiQuotes
           , Rank2Types
           , TemplateHaskell
           , TypeFamilies
           , ScopedTypeVariables
           , TypeSynonymInstances
 #-}
module Main (main) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger (runStderrLoggingT, runNoLoggingT)
import Database.Esqueleto
import Database.Persist.Postgresql (withPostgresqlConn)
import Data.Ord (comparing)
import Control.Arrow ((&&&))
import qualified Database.Esqueleto.PostgreSQL as EP
import Test.Hspec
import qualified Control.Monad.Trans.Resource as R
import qualified Data.List as L
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Common.Test

-------------------------------------------------------------------------------


testPostgresqlCoalesce :: Spec
testPostgresqlCoalesce = do
  it "works on PostgreSQL and MySQL with <2 arguments" $
    run $ do
      _ :: [Value (Maybe Int)] <-
        select $
        from $ \p -> do
        return (coalesce [p ^. PersonAge])
      return ()


-------------------------------------------------------------------------------


testPostgresqlTextFunction :: Spec
testPostgresqlTextFunction = do
  it "ilike, (%) and (++.) work on a simple example on PostgreSQL" $
    run $ do
      [p1e, _, p3e, _, p5e] <- mapM insert' [p1, p2, p3, p4, p5]
      let nameContains t expected = do
            ret <- select $
                   from $ \p -> do
                   where_ (p ^. PersonName `ilike` (%) ++. val t ++. (%))
                   orderBy [asc (p ^. PersonName)]
                   return p
            liftIO $ ret `shouldBe` expected
      nameContains "mi" [p3e, p5e]
      nameContains "JOHN" [p1e]


-------------------------------------------------------------------------------


testPostgresqlUpdate :: Spec
testPostgresqlUpdate = do
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
      -- PostgreSQL: nulls are bigger than data, and update returns
      --             matched rows, not actually changed rows.
      liftIO $ n `shouldBe` 2
      liftIO $ ret `shouldBe` [ Entity p1k (Person anon (Just 73) Nothing 1)
                              , Entity p2k (Person anon Nothing (Just 37) 2)
                              , Entity p3k p3 ]


-------------------------------------------------------------------------------


testPostgresqlRandom :: Spec
testPostgresqlRandom = do
  it "works with random_" $
    run $ do
      _ <- select $ return (random_ :: SqlExpr (Value Double))
      return ()


-------------------------------------------------------------------------------


testPostgresqlSum :: Spec
testPostgresqlSum = do
  it "works with sum_" $
    run $ do
      _ <- insert' p1
      _ <- insert' p2
      _ <- insert' p3
      _ <- insert' p4
      ret <- select $
             from $ \p->
             return $ joinV $ sum_ (p ^. PersonAge)
      liftIO $ ret `shouldBe` [ Value $ Just (36 + 17 + 17 :: Rational ) ]


-------------------------------------------------------------------------------


testPostgresqlTwoAscFields :: Spec
testPostgresqlTwoAscFields = do
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
      -- in PostgreSQL nulls are bigger than everything
      liftIO $ ret `shouldBe` [ p4e, p3e, p1e , p2e ]


-------------------------------------------------------------------------------


testPostgresqlOneAscOneDesc :: Spec
testPostgresqlOneAscOneDesc = do
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
      liftIO $ ret `shouldBe` [ p2e, p1e, p4e, p3e ]


-------------------------------------------------------------------------------


testSelectDistinctOn :: Spec
testSelectDistinctOn = do
  describe "SELECT DISTINCT ON" $ do
    it "works on a simple example" $ do
      run $ do
        [p1k, p2k, _] <- mapM insert [p1, p2, p3]
        [_, bpB, bpC] <- mapM insert'
          [ BlogPost "A" p1k
          , BlogPost "B" p1k
          , BlogPost "C" p2k ]
        ret <- select $
               from $ \bp ->
               distinctOn [don (bp ^. BlogPostAuthorId)] $ do
               orderBy [asc (bp ^. BlogPostAuthorId), desc (bp ^. BlogPostTitle)]
               return bp
        liftIO $ ret `shouldBe` L.sortBy (comparing (blogPostAuthorId . entityVal)) [bpB, bpC]

    let slightlyLessSimpleTest q =
          run $ do
            [p1k, p2k, _] <- mapM insert [p1, p2, p3]
            [bpA, bpB, bpC] <- mapM insert'
              [ BlogPost "A" p1k
              , BlogPost "B" p1k
              , BlogPost "C" p2k ]
            ret <- select $
                   from $ \bp ->
                   q bp $ return bp
            let cmp = (blogPostAuthorId &&& blogPostTitle) . entityVal
            liftIO $ ret `shouldBe` L.sortBy (comparing cmp) [bpA, bpB, bpC]

    it "works on a slightly less simple example (two distinctOn calls, orderBy)" $
      slightlyLessSimpleTest $ \bp act ->
        distinctOn [don (bp ^. BlogPostAuthorId)] $
        distinctOn [don (bp ^. BlogPostTitle)] $ do
          orderBy [asc (bp ^. BlogPostAuthorId), asc (bp ^. BlogPostTitle)]
          act

    it "works on a slightly less simple example (one distinctOn call, orderBy)" $ do
      slightlyLessSimpleTest $ \bp act ->
        distinctOn [don (bp ^. BlogPostAuthorId), don (bp ^. BlogPostTitle)] $ do
          orderBy [asc (bp ^. BlogPostAuthorId), asc (bp ^. BlogPostTitle)]
          act

    it "works on a slightly less simple example (distinctOnOrderBy)" $ do
      slightlyLessSimpleTest $ \bp ->
        distinctOnOrderBy [asc (bp ^. BlogPostAuthorId), asc (bp ^. BlogPostTitle)]


-------------------------------------------------------------------------------


testPostgresModule :: Spec
testPostgresModule = do
  describe "PostgreSQL module" $ do
    it "arrayAgg looks sane" $
      run $ do
        let people = [p1, p2, p3, p4, p5]
        mapM_ insert people
        [Value ret] <-
          select . from $ \p -> return (EP.arrayAgg (p ^. PersonName))
        liftIO $ L.sort ret `shouldBe` L.sort (map personName people)

    it "stringAgg looks sane" $
      run $ do
        let people = [p1, p2, p3, p4, p5]
        mapM_ insert people
        [Value ret] <-
          select $
          from $ \p -> do
          return (EP.stringAgg (p ^. PersonName) (val " "))
        liftIO $ L.sort (words ret) `shouldBe` L.sort (map personName people)

    it "chr looks sane" $
      run $ do
        [Value (ret :: String)] <- select $ return (EP.chr (val 65))
        liftIO $ ret `shouldBe` "A"

    it "works with now" $
      run $ do
        nowDb <- select $ return EP.now_
        nowUtc <- liftIO getCurrentTime
        let halfSecond = realToFrac (0.5 :: Double)

        -- | Check the result is not null
        liftIO $ nowDb `shouldSatisfy` (not . null)

        -- | Unpack the now value
        let (Value now: _) = nowDb

        -- | Get the time diff and check it's less than half a second
        liftIO $ diffUTCTime nowUtc now `shouldSatisfy` (< halfSecond)


-------------------------------------------------------------------------------


main :: IO ()
main = do
  hspec $ do
    tests run

    describe "Test PostgreSQL locking" $ do
      testLocking withConn

    describe "PostgreSQL specific tests" $ do
      testSelectDistinctOn
      testPostgresModule
      testPostgresqlOneAscOneDesc
      testPostgresqlTwoAscFields
      testPostgresqlSum
      testPostgresqlRandom
      testPostgresqlUpdate
      testPostgresqlTextFunction
      testPostgresqlCoalesce


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

migrateIt :: RunDbMonad m => SqlPersistT (R.ResourceT m) ()
migrateIt = do
  void $ runMigrationSilent migrateAll
  cleanDB

run_worker :: RunDbMonad m => SqlPersistT (R.ResourceT m) a -> m a
run_worker act = withConn $ runSqlConn (migrateIt >> act)

withConn :: RunDbMonad m => (SqlBackend -> R.ResourceT m a) -> m a
withConn =
  R.runResourceT . withPostgresqlConn "host=localhost port=5432 user=esqutest password=esqutest dbname=esqutest"
