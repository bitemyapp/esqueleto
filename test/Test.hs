{-# LANGUAGE ConstraintKinds
           , EmptyDataDecls
           , FlexibleContexts
           , GADTs
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , OverloadedStrings
           , QuasiQuotes
           , Rank2Types
           , TemplateHaskell
           , TypeFamilies
 #-}
module Main (main) where

import Control.Applicative ((<$>))
import Control.Monad (replicateM_)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger (MonadLogger(..), runStderrLoggingT, runNoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Database.Esqueleto
import Database.Persist.Sqlite (withSqliteConn)
import Database.Persist.TH
import Test.Hspec

import qualified Data.Conduit as C


-- Test schema
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
  Person
    name String
    age Int Maybe
    deriving Eq Show
  BlogPost
    title String
    authorId PersonId
    deriving Eq Show
  Follow
    follower PersonId
    followed PersonId
    deriving Eq Show
|]


main :: IO ()
main = do
  let p1 = Person "John"  (Just 36)
      p2 = Person "Rachel" Nothing
      p3 = Person "Mike"  (Just 17)
      p4 = Person "Livia" (Just 17)
  hspec $ do
    describe "select" $ do
      it "works for a single value" $
        run $ do
          ret <- select $ return $ val (3 :: Int)
          liftIO $ ret `shouldBe` [ Value 3 ]

      it "works for a single NULL value" $
        run $ do
          ret <- select $ return $ nothing
          liftIO $ ret `shouldBe` [ Value (Nothing :: Maybe Int) ]

    describe "select/from" $ do
      it "works for a simple example" $
        run $ do
          p1e <- insert' p1
          ret <- select $
                 from $ \person ->
                 return person
          liftIO $ ret `shouldBe` [ p1e ]

      it "works for a simple self-join (one entity)" $
        run $ do
          p1e <- insert' p1
          ret <- select $
                 from $ \(person1, person2) ->
                 return (person1, person2)
          liftIO $ ret `shouldBe` [ (p1e, p1e) ]

      it "works for a simple self-join (two entities)" $
        run $ do
          p1e <- insert' p1
          p2e <- insert' p2
          ret <- select $
                 from $ \(person1, person2) ->
                 return (person1, person2)
          liftIO $ ret `shouldBe` [ (p1e, p1e)
                                  , (p1e, p2e)
                                  , (p2e, p1e)
                                  , (p2e, p2e) ]

      it "works for a simple projection" $
        run $ do
          p1k <- insert p1
          p2k <- insert p2
          ret <- select $
                 from $ \p ->
                 return (p ^. PersonId, p ^. PersonName)
          liftIO $ ret `shouldBe` [ (Value p1k, Value (personName p1))
                                  , (Value p2k, Value (personName p2)) ]

      it "works for a simple projection with a simple implicit self-join" $
        run $ do
          _ <- insert p1
          _ <- insert p2
          ret <- select $
                 from $ \(pa, pb) ->
                 return (pa ^. PersonName, pb ^. PersonName)
          liftIO $ ret `shouldBe` [ (Value (personName p1), Value (personName p1))
                                  , (Value (personName p1), Value (personName p2))
                                  , (Value (personName p2), Value (personName p1))
                                  , (Value (personName p2), Value (personName p2)) ]

      it "works with many kinds of LIMITs and OFFSETs" $
        run $ do
          [p1e, p2e, p3e, p4e] <- mapM insert' [p1, p2, p3, p4]
          let people = from $ \p -> do
                       orderBy [asc (p ^. PersonName)]
                       return p
          ret1 <- select $ do
                  p <- people
                  limit 2
                  limit 1
                  return p
          liftIO $ ret1 `shouldBe` [ p1e ]
          ret2 <- select $ do
                  p <- people
                  limit 1
                  limit 2
                  return p
          liftIO $ ret2 `shouldBe` [ p1e, p4e ]
          ret3 <- select $ do
                  p <- people
                  offset 3
                  offset 2
                  return p
          liftIO $ ret3 `shouldBe` [ p3e, p2e ]
          ret4 <- select $ do
                  p <- people
                  offset 3
                  limit 5
                  offset 2
                  limit 3
                  offset 1
                  limit 2
                  return p
          liftIO $ ret4 `shouldBe` [ p4e, p3e ]
          ret5 <- select $ do
                  p <- people
                  offset 1000
                  limit  1
                  limit  1000
                  offset 0
                  return p
          liftIO $ ret5 `shouldBe` [ p1e, p4e, p3e, p2e ]


    describe "select/JOIN" $ do
      it "works with a LEFT OUTER JOIN" $
        run $ do
          p1e <- insert' p1
          p2e <- insert' p2
          p3e <- insert' p3
          p4e <- insert' p4
          b12e <- insert' $ BlogPost "b" (entityKey p1e)
          b11e <- insert' $ BlogPost "a" (entityKey p1e)
          b31e <- insert' $ BlogPost "c" (entityKey p3e)
          ret <- select $
                 from $ \(p `LeftOuterJoin` mb) -> do
                 on (just (p ^. PersonId) ==. mb ?. BlogPostAuthorId)
                 orderBy [ asc (p ^. PersonName), asc (mb ?. BlogPostTitle) ]
                 return (p, mb)
          liftIO $ ret `shouldBe` [ (p1e, Just b11e)
                                  , (p1e, Just b12e)
                                  , (p4e, Nothing)
                                  , (p3e, Just b31e)
                                  , (p2e, Nothing) ]

      it "typechecks (A LEFT OUTER JOIN (B LEFT OUTER JOIN C))" $
        let _ = run $
                select $
                from $ \(a `LeftOuterJoin` (b `LeftOuterJoin` c)) ->
                let _ = [a, b, c] :: [ SqlExpr (Entity Person) ]
                in return a
        in return () :: IO ()

      it "typechecks ((A LEFT OUTER JOIN B) LEFT OUTER JOIN C)" $
        let _ = run $
                select $
                from $ \((a `LeftOuterJoin` b) `LeftOuterJoin` c) ->
                let _ = [a, b, c] :: [ SqlExpr (Entity Person) ]
                in return a
        in return () :: IO ()

      it "throws an error for using on without joins" $
        run (select $
             from $ \(p, mb) -> do
             on (just (p ^. PersonId) ==. mb ?. BlogPostAuthorId)
             orderBy [ asc (p ^. PersonName), asc (mb ?. BlogPostTitle) ]
             return (p, mb)
        ) `shouldThrow` (\(OnClauseWithoutMatchingJoinException _) -> True)

      it "throws an error for using too many ons" $
        run (select $
             from $ \(p `FullOuterJoin` mb) -> do
             on (just (p ^. PersonId) ==. mb ?. BlogPostAuthorId)
             on (just (p ^. PersonId) ==. mb ?. BlogPostAuthorId)
             orderBy [ asc (p ^. PersonName), asc (mb ?. BlogPostTitle) ]
             return (p, mb)
        ) `shouldThrow` (\(OnClauseWithoutMatchingJoinException _) -> True)

    describe "select/where_" $ do
      it "works for a simple example with (==.)" $
        run $ do
          p1e <- insert' p1
          _   <- insert' p2
          _   <- insert' p3
          ret <- select $
                 from $ \p -> do
                 where_ (p ^. PersonName ==. val "John")
                 return p
          liftIO $ ret `shouldBe` [ p1e ]

      it "works for a simple example with (==.) and (||.)" $
        run $ do
          p1e <- insert' p1
          p2e <- insert' p2
          _   <- insert' p3
          ret <- select $
                 from $ \p -> do
                 where_ (p ^. PersonName ==. val "John" ||. p ^. PersonName ==. val "Rachel")
                 return p
          liftIO $ ret `shouldBe` [ p1e, p2e ]

      it "works for a simple example with (>.) [uses val . Just]" $
        run $ do
          p1e <- insert' p1
          _   <- insert' p2
          _   <- insert' p3
          ret <- select $
                 from $ \p -> do
                 where_ (p ^. PersonAge >. val (Just 17))
                 return p
          liftIO $ ret `shouldBe` [ p1e ]

      it "works for a simple example with (>.) and not_ [uses just . val]" $
        run $ do
          _   <- insert' p1
          _   <- insert' p2
          p3e <- insert' p3
          ret <- select $
                 from $ \p -> do
                 where_ (not_ $ p ^. PersonAge >. just (val 17))
                 return p
          liftIO $ ret `shouldBe` [ p3e ]

      it "works with isNothing" $
        run $ do
          _   <- insert' p1
          p2e <- insert' p2
          _   <- insert' p3
          ret <- select $
                 from $ \p -> do
                 where_ $ isNothing (p ^. PersonAge)
                 return p
          liftIO $ ret `shouldBe` [ p2e ]

      it "works with not_ . isNothing" $
        run $ do
          p1e <- insert' p1
          _   <- insert' p2
          ret <- select $
                 from $ \p -> do
                 where_ $ not_ (isNothing (p ^. PersonAge))
                 return p
          liftIO $ ret `shouldBe` [ p1e ]

      it "works for a many-to-many implicit join" $
        run $ do
          p1e@(Entity p1k _) <- insert' p1
          p2e@(Entity p2k _) <- insert' p2
          _                  <- insert' p3
          p4e@(Entity p4k _) <- insert' p4
          f12 <- insert' (Follow p1k p2k)
          f21 <- insert' (Follow p2k p1k)
          f42 <- insert' (Follow p4k p2k)
          f11 <- insert' (Follow p1k p1k)
          ret <- select $
                 from $ \(follower, follows, followed) -> do
                 where_ $ follower ^. PersonId ==. follows ^. FollowFollower &&.
                          followed ^. PersonId ==. follows ^. FollowFollowed
                 orderBy [ asc (follower ^. PersonName)
                         , asc (followed ^. PersonName) ]
                 return (follower, follows, followed)
          liftIO $ ret `shouldBe` [ (p1e, f11, p1e)
                                  , (p1e, f12, p2e)
                                  , (p4e, f42, p2e)
                                  , (p2e, f21, p1e) ]

      it "works for a many-to-many explicit join" $
        run $ do
          p1e@(Entity p1k _) <- insert' p1
          p2e@(Entity p2k _) <- insert' p2
          _                  <- insert' p3
          p4e@(Entity p4k _) <- insert' p4
          f12 <- insert' (Follow p1k p2k)
          f21 <- insert' (Follow p2k p1k)
          f42 <- insert' (Follow p4k p2k)
          f11 <- insert' (Follow p1k p1k)
          ret <- select $
                 from $ \(follower `InnerJoin` follows `InnerJoin` followed) -> do
                 on $ followed ^. PersonId ==. follows ^. FollowFollowed
                 on $ follower ^. PersonId ==. follows ^. FollowFollower
                 orderBy [ asc (follower ^. PersonName)
                         , asc (followed ^. PersonName) ]
                 return (follower, follows, followed)
          liftIO $ ret `shouldBe` [ (p1e, f11, p1e)
                                  , (p1e, f12, p2e)
                                  , (p4e, f42, p2e)
                                  , (p2e, f21, p1e) ]

      it "works for a many-to-many explicit join with LEFT OUTER JOINs" $
        run $ do
          p1e@(Entity p1k _) <- insert' p1
          p2e@(Entity p2k _) <- insert' p2
          p3e                <- insert' p3
          p4e@(Entity p4k _) <- insert' p4
          f12 <- insert' (Follow p1k p2k)
          f21 <- insert' (Follow p2k p1k)
          f42 <- insert' (Follow p4k p2k)
          f11 <- insert' (Follow p1k p1k)
          ret <- select $
                 from $ \(follower `LeftOuterJoin` mfollows `LeftOuterJoin` mfollowed) -> do
                 on $      mfollowed ?. PersonId  ==. mfollows ?. FollowFollowed
                 on $ just (follower ^. PersonId) ==. mfollows ?. FollowFollower
                 orderBy [ asc ( follower ^. PersonName)
                         , asc (mfollowed ?. PersonName) ]
                 return (follower, mfollows, mfollowed)
          liftIO $ ret `shouldBe` [ (p1e, Just f11, Just p1e)
                                  , (p1e, Just f12, Just p2e)
                                  , (p4e, Just f42, Just p2e)
                                  , (p3e, Nothing,  Nothing)
                                  , (p2e, Just f21, Just p1e) ]


    describe "select/orderBy" $ do
      it "works with a single ASC field" $
        run $ do
          p1e <- insert' p1
          p2e <- insert' p2
          p3e <- insert' p3
          ret <- select $
                 from $ \p -> do
                 orderBy [asc $ p ^. PersonName]
                 return p
          liftIO $ ret `shouldBe` [ p1e, p3e, p2e ]

      it "works with two ASC fields" $
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

      it "works with one ASC and one DESC field" $
        run $ do
          p1e <- insert' p1
          p2e <- insert' p2
          p3e <- insert' p3
          p4e <- insert' p4
          ret <- select $
                 from $ \p -> do
                 orderBy [desc (p ^. PersonAge), asc (p ^. PersonName)]
                 return p
          liftIO $ ret `shouldBe` [ p1e, p4e, p3e, p2e ]

      it "works with a sub_select" $
        run $ do
          [p1k, p2k, p3k, p4k] <- mapM insert [p1, p2, p3, p4]
          [b1k, b2k, b3k, b4k] <- mapM (insert . BlogPost "") [p1k, p2k, p3k, p4k]
          ret <- select $
                 from $ \b -> do
                 orderBy [desc $ sub_select $
                                 from $ \p -> do
                                 where_ (p ^. PersonId ==. b ^. BlogPostAuthorId)
                                 return (p ^. PersonName)
                         ]
                 return (b ^. BlogPostId)
          liftIO $ ret `shouldBe` (Value <$> [b2k, b3k, b4k, b1k])


    describe "selectDistinct" $
      it "works on a simple example" $
        run $ do
          p1k <- insert p1
          let (t1, t2, t3) = ("a", "b", "c")
          mapM_ (insert . flip BlogPost p1k) [t1, t3, t2, t2, t1]
          ret <- selectDistinct $
                 from $ \b -> do
                 let title = b ^. BlogPostTitle
                 orderBy [asc title]
                 return title
          liftIO $ ret `shouldBe` [ Value t1, Value t2, Value t3 ]

    describe "text functions" $
      it "like, (%) and (++.) work on a simple example" $
         run $ do
           [p1e, p2e, p3e, p4e] <- mapM insert' [p1, p2, p3, p4]
           let nameContains t expected = do
                 ret <- select $
                        from $ \p -> do
                        where_ (p ^. PersonName `like` (%) ++. val t ++. (%))
                        orderBy [asc (p ^. PersonName)]
                        return p
                 liftIO $ ret `shouldBe` expected
           nameContains "h"  [p1e, p2e]
           nameContains "i"  [p4e, p3e]
           nameContains "iv" [p4e]

    describe "delete" $
      it "works on a simple example" $
        run $ do
          p1e <- insert' p1
          p2e <- insert' p2
          p3e <- insert' p3
          let getAll = select $
                       from $ \p -> do
                       orderBy [asc (p ^. PersonName)]
                       return p
          ret1 <- getAll
          liftIO $ ret1 `shouldBe` [ p1e, p3e, p2e ]
          ()   <- delete $
                  from $ \p ->
                  where_ (p ^. PersonName ==. val (personName p1))
          ret2 <- getAll
          liftIO $ ret2 `shouldBe` [ p3e, p2e ]
          n    <- deleteCount $
                  from $ \p ->
                  return ((p :: SqlExpr (Entity Person)) `seq` ())
          ret3 <- getAll
          liftIO $ (n, ret3) `shouldBe` (2, [])

    describe "update" $ do
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
          liftIO $ n `shouldBe` 2
          liftIO $ ret `shouldBe` [ Entity p2k (Person anon Nothing)
                                  , Entity p1k (Person anon (Just 73))
                                  , Entity p3k p3 ]

      it "works with a subexpression having COUNT(*)" $
        run $ do
          p1k <- insert p1
          p2k <- insert p2
          p3k <- insert p3
          replicateM_ 3 (insert $ BlogPost "" p1k)
          replicateM_ 7 (insert $ BlogPost "" p3k)
          let blogPostsBy p =
                from $ \b -> do
                where_ (b ^. BlogPostAuthorId ==. p ^. PersonId)
                return countRows
          ()  <- update $ \p -> do
                 set p [ PersonAge =. just (sub_select (blogPostsBy p)) ]
          ret <- select $
                 from $ \p -> do
                 orderBy [ asc (p ^. PersonName) ]
                 return p
          liftIO $ ret `shouldBe` [ Entity p1k p1 { personAge = Just 3 }
                                  , Entity p3k p3 { personAge = Just 7 }
                                  , Entity p2k p2 { personAge = Just 0 } ]

      it "GROUP BY works with COUNT" $
        run $ do
          p1k <- insert p1
          p2k <- insert p2
          p3k <- insert p3
          replicateM_ 3 (insert $ BlogPost "" p1k)
          replicateM_ 7 (insert $ BlogPost "" p3k)
          ret <- select $
                 from $ \(p `LeftOuterJoin` b) -> do
                 on (p ^. PersonId ==. b ^. BlogPostAuthorId)
                 groupBy (p ^. PersonId)
                 let cnt = count (b ^. BlogPostId)
                 orderBy [ asc cnt ]
                 return (p, cnt)
          liftIO $ ret `shouldBe` [ (Entity p2k p2, Value (0 :: Int))
                                  , (Entity p1k p1, Value 3)
                                  , (Entity p3k p3, Value 7) ]

    describe "lists of values" $ do
      it "IN works for valList" $
        run $ do
          p1k <- insert p1
          p2k <- insert p2
          p3k <- insert p3
          ret <- select $
                 from $ \p -> do
                 where_ (p ^. PersonName `in_` valList (personName <$> [p1, p2]))
                 return p
          liftIO $ ret `shouldBe` [ Entity p1k p1
                                  , Entity p2k p2 ]

      it "IN works for valList (null list)" $
        run $ do
          p1k <- insert p1
          p2k <- insert p2
          p3k <- insert p3
          ret <- select $
                 from $ \p -> do
                 where_ (p ^. PersonName `in_` valList [])
                 return p
          liftIO $ ret `shouldBe` []

      it "IN works for subList_select" $
        run $ do
          p1k <- insert p1
          p2k <- insert p2
          p3k <- insert p3
          _ <- insert (BlogPost "" p1k)
          _ <- insert (BlogPost "" p3k)
          ret <- select $
                 from $ \p -> do
                 let subquery =
                       from $ \bp ->
                       return (bp ^. BlogPostAuthorId)
                 where_ (p ^. PersonId `in_` subList_select subquery)
                 return p
          liftIO $ ret `shouldBe` [ Entity p1k p1
                                  , Entity p3k p3 ]

      it "NOT IN works for subList_select" $
        run $ do
          p1k <- insert p1
          p2k <- insert p2
          p3k <- insert p3
          _ <- insert (BlogPost "" p1k)
          _ <- insert (BlogPost "" p3k)
          ret <- select $
                 from $ \p -> do
                 let subquery =
                       from $ \bp ->
                       return (bp ^. BlogPostAuthorId)
                 where_ (p ^. PersonId `notIn` subList_select subquery)
                 return p
          liftIO $ ret `shouldBe` [ Entity p2k p2 ]

      it "EXISTS works for subList_select" $
        run $ do
          p1k <- insert p1
          p2k <- insert p2
          p3k <- insert p3
          _ <- insert (BlogPost "" p1k)
          _ <- insert (BlogPost "" p3k)
          ret <- select $
                 from $ \p -> do
                 where_ $ exists $
                          from $ \bp -> do
                          where_ (bp ^. BlogPostAuthorId ==. p ^. PersonId)
                 return p
          liftIO $ ret `shouldBe` [ Entity p1k p1
                                  , Entity p3k p3 ]

      it "EXISTS works for subList_select" $
        run $ do
          p1k <- insert p1
          p2k <- insert p2
          p3k <- insert p3
          _ <- insert (BlogPost "" p1k)
          _ <- insert (BlogPost "" p3k)
          ret <- select $
                 from $ \p -> do
                 where_ $ notExists $
                          from $ \bp -> do
                          where_ (bp ^. BlogPostAuthorId ==. p ^. PersonId)
                 return p
          liftIO $ ret `shouldBe` [ Entity p2k p2 ]


----------------------------------------------------------------------


insert' :: ( Functor m
           , PersistStore m
           , PersistMonadBackend m ~ PersistEntityBackend val
           , PersistEntity val )
        => val -> m (Entity val)
insert' v = flip Entity v <$> insert v


type RunDbMonad m = ( MonadBaseControl IO m, MonadIO m, MonadLogger m
                    , C.MonadUnsafeIO m, C.MonadThrow m )


run, runSilent, runVerbose :: (forall m. RunDbMonad m => SqlPersist (C.ResourceT m) a) -> IO a
runSilent  act = runNoLoggingT     $ run_worker act
runVerbose act = runStderrLoggingT $ run_worker act
run =
  if verbose
  then runVerbose
  else runSilent


verbose :: Bool
verbose = True


run_worker :: RunDbMonad m => SqlPersist (C.ResourceT m) a -> m a
run_worker =
  C.runResourceT .
  withSqliteConn ":memory:" .
  runSqlConn .
  (runMigrationSilent migrateAll >>)
