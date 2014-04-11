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
           , ScopedTypeVariables
           , CPP
 #-}
module Main (main) where

import Control.Applicative ((<$>))
import Control.Monad (replicateM, replicateM_)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger (MonadLogger(..), runStderrLoggingT, runNoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Database.Esqueleto
#if   defined (WITH_POSTGRESQL)
import Database.Persist.Postgresql (withPostgresqlConn)
#elif defined (WITH_MYSQL)
import Database.Persist.MySQL ( withMySQLConn
                              , connectHost
                              , connectDatabase
                              , connectUser
                              , connectPassword
                              , defaultConnectInfo)
#else
import Database.Persist.Sqlite (withSqliteConn)
#endif
import Database.Persist.TH
import Test.Hspec

import qualified Control.Monad.Trans.Resource as R
import qualified Data.Set as S
import qualified Data.List as L


-- Test schema
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
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

-- | this could be achieved with S.fromList, but not all lists
--   have Ord instances
sameElementsAs :: Eq a => [a] -> [a] -> Bool
sameElementsAs l1 l2 = null (l1 L.\\ l2)

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

      it "works for a pair of a single value and ()" $
        run $ do
          ret <- select $ return (val (3 :: Int), ())
          liftIO $ ret `shouldBe` [ (Value 3, ()) ]

      it "works for a single ()" $
        run $ do
          ret <- select $ return ()
          liftIO $ ret `shouldBe` [ () ]

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
          liftIO $ ret `shouldSatisfy` sameElementsAs [ (p1e, p1e)
                                                      , (p1e, p2e)
                                                      , (p2e, p1e)
                                                      , (p2e, p2e) ]

      it "works for a self-join via sub_select" $
        run $ do
          p1k <- insert p1
          p2k <- insert p2
          _f1k <- insert (Follow p1k p2k)
          _f2k <- insert (Follow p2k p1k)
          ret <- select $
                 from $ \followA -> do
                 let subquery =
                       from $ \followB -> do
                       where_ $ followA ^. FollowFollower ==. followB ^. FollowFollowed
                       return $ followB ^. FollowFollower
                 where_ $ followA ^. FollowFollowed ==. sub_select subquery
                 return followA
          liftIO $ length ret `shouldBe` 2

      it "works for a self-join via exists" $
        run $ do
          p1k <- insert p1
          p2k <- insert p2
          _f1k <- insert (Follow p1k p2k)
          _f2k <- insert (Follow p2k p1k)
          ret <- select $
                 from $ \followA -> do
                 where_ $ exists $
                          from $ \followB ->
                          where_ $ followA ^. FollowFollower ==. followB ^. FollowFollowed
                 return followA
          liftIO $ length ret `shouldBe` 2


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
          liftIO $ ret `shouldSatisfy` sameElementsAs
                                  [ (Value (personName p1), Value (personName p1))
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

      it "works with sum_" $
        run $ do
          _ <- insert' p1
          _ <- insert' p2
          _ <- insert' p3
          _ <- insert' p4
          ret <- select $
                 from $ \p->
                 return $ joinV $ sum_ (p ^. PersonAge)
#if   defined(WITH_POSTGRESQL)
          liftIO $ ret `shouldBe` [ Value $ Just (36 + 17 + 17 :: Rational ) ]
#elif defined(WITH_MYSQL)
          liftIO $ ret `shouldBe` [ Value $ Just (36 + 17 + 17 :: Double ) ]
#else
          liftIO $ ret `shouldBe` [ Value $ Just (36 + 17 + 17 :: Int) ]
#endif

      it "works with avg_" $
        run $ do
          _ <- insert' p1
          _ <- insert' p2
          _ <- insert' p3
          _ <- insert' p4
          ret <- select $
                 from $ \p->
                 return $ joinV $ avg_ (p ^. PersonAge)
          liftIO $ ret `shouldBe` [ Value $ Just ((36 + 17 + 17) / 3 :: Double) ]

      it "works with min_" $
        run $ do
          _ <- insert' p1
          _ <- insert' p2
          _ <- insert' p3
          _ <- insert' p4
          ret <- select $
                 from $ \p->
                 return $ joinV $ min_ (p ^. PersonAge)
          liftIO $ ret `shouldBe` [ Value $ Just (17 :: Int) ]

      it "works with max_" $
        run $ do
          _ <- insert' p1
          _ <- insert' p2
          _ <- insert' p3
          _ <- insert' p4
          ret <- select $
                 from $ \p->
                 return $ joinV $ max_ (p ^. PersonAge)
          liftIO $ ret `shouldBe` [ Value $ Just (36 :: Int) ]

      it "works with random_" $
        run $ do
#if defined(WITH_POSTGRESQL) || defined(WITH_MYSQL)
          _ <- select $ return (random_ :: SqlExpr (Value Double))
#else
          _ <- select $ return (random_ :: SqlExpr (Value Int))
#endif
          return ()

      it "works with round_" $
        run $ do
          ret <- select $ return $ round_ (val (16.2 :: Double))
          liftIO $ ret `shouldBe` [ Value (16 :: Double) ]

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
          -- in PostgreSQL nulls are bigger than everything
#ifdef WITH_POSTGRESQL
          liftIO $ ret `shouldBe` [ p4e, p3e, p1e , p2e ]
#else
          -- in SQLite and MySQL, its the reverse
          liftIO $ ret `shouldBe` [ p2e, p4e, p3e, p1e ]
#endif

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
#ifdef WITH_POSTGRESQL
          liftIO $ ret `shouldBe` [ p2e, p1e, p4e, p3e ]
#else
          liftIO $ ret `shouldBe` [ p1e, p4e, p3e, p2e ]
#endif

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

      it "works with asc random_" $
        run $ do
          _p1e <- insert' p1
          _p2e <- insert' p2
          _p3e <- insert' p3
          _p4e <- insert' p4
          rets <-
            fmap S.fromList $
            replicateM 11 $
            select $
            from $ \p -> do
            orderBy [asc (random_ :: SqlExpr (Value Double))]
            return (p ^. PersonId :: SqlExpr (Value PersonId))
          -- There are 2^4 = 16 possible orderings.  The chance
          -- of 11 random samplings returning the same ordering
          -- is 1/2^40, so this test should pass almost everytime.
          liftIO $ S.size rets `shouldSatisfy` (>2)


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
          -- PostgreSQL: nulls are bigger than data, and update returns
          --             matched rows, not actually changed rows.
#if   defined(WITH_POSTGRESQL)
          liftIO $ n `shouldBe` 2
          liftIO $ ret `shouldBe` [ Entity p1k (Person anon (Just 73))
                                  , Entity p2k (Person anon Nothing)
                                  , Entity p3k p3 ]
          -- MySQL: nulls appear first, and update returns actual number
          --        of changed rows
#elif defined(WITH_MYSQL)
          liftIO $ n `shouldBe` 1
          liftIO $ ret `shouldBe` [ Entity p2k (Person anon Nothing)
                                  , Entity p1k (Person anon (Just 73))
                                  , Entity p3k p3 ]
#else
          -- SQLite: nulls appear first, update returns matched rows.
          liftIO $ n `shouldBe` 2
          liftIO $ ret `shouldBe` [ Entity p2k (Person anon Nothing)
                                  , Entity p1k (Person anon (Just 73))
                                  , Entity p3k p3 ]
#endif

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

      it "GROUP BY works with HAVING" $
        run $ do
          p1k <- insert p1
          _p2k <- insert p2
          p3k <- insert p3
          replicateM_ 3 (insert $ BlogPost "" p1k)
          replicateM_ 7 (insert $ BlogPost "" p3k)
          ret <- select $
                 from $ \(p `LeftOuterJoin` b) -> do
                 on (p ^. PersonId ==. b ^. BlogPostAuthorId)
                 let cnt = count (b ^. BlogPostId)
                 groupBy (p ^. PersonId)
                 having (cnt >. (val 0))
                 orderBy [ asc cnt ]
                 return (p, cnt)
          liftIO $ ret `shouldBe` [ (Entity p1k p1, Value (3 :: Int))
                                  , (Entity p3k p3, Value 7) ]

    describe "lists of values" $ do
      it "IN works for valList" $
        run $ do
          p1k <- insert p1
          p2k <- insert p2
          _p3k <- insert p3
          ret <- select $
                 from $ \p -> do
                 where_ (p ^. PersonName `in_` valList (personName <$> [p1, p2]))
                 return p
          liftIO $ ret `shouldBe` [ Entity p1k p1
                                  , Entity p2k p2 ]

      it "IN works for valList (null list)" $
        run $ do
          _p1k <- insert p1
          _p2k <- insert p2
          _p3k <- insert p3
          ret <- select $
                 from $ \p -> do
                 where_ (p ^. PersonName `in_` valList [])
                 return p
          liftIO $ ret `shouldBe` []

      it "IN works for subList_select" $
        run $ do
          p1k <- insert p1
          _p2k <- insert p2
          p3k <- insert p3
          _ <- insert (BlogPost "" p1k)
          _ <- insert (BlogPost "" p3k)
          ret <- select $
                 from $ \p -> do
                 let subquery =
                       from $ \bp -> do
                       orderBy [ asc (bp ^. BlogPostAuthorId) ]
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
          _p2k <- insert p2
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

    describe "inserts by select" $ do
      it "IN works for insertSelect" $
        run $ do
          _ <- insert p1
          _ <- insert p2
          _ <- insert p3
          insertSelect $ from $ \p -> do
            return $ BlogPost <# val "FakePost" <&> (p ^. PersonId)
          ret <- select $ from (\(_::(SqlExpr (Entity BlogPost))) -> return countRows)
          liftIO $ ret `shouldBe` [Value (3::Int)]

    describe "rand works" $ do
      it "returns result in random order" $
        run $ do
          _ <- insert p1
          _ <- insert p2
          _ <- insert p3
          _ <- insert p4
          _ <- insert $ Person "Jane"  Nothing
          _ <- insert $ Person "Mark"  Nothing
          _ <- insert $ Person "Sarah" Nothing
          _ <- insert $ Person "Paul"  Nothing
          ret1 <- fmap (map unValue) $ select $ from $ \p -> do
                    orderBy [rand]
                    return (p ^. PersonId)
          ret2 <- fmap (map unValue) $ select $ from $ \p -> do
                    orderBy [rand]
                    return (p ^. PersonId)

          liftIO $ (ret1 == ret2) `shouldBe` False

----------------------------------------------------------------------


insert' :: ( Functor m
           , PersistStore m
           , PersistMonadBackend m ~ PersistEntityBackend val
           , PersistEntity val )
        => val -> m (Entity val)
insert' v = flip Entity v <$> insert v


type RunDbMonad m = ( MonadBaseControl IO m, MonadIO m, MonadLogger m
                    , R.MonadThrow m )

#if defined (WITH_POSTGRESQL) || defined (WITH_MYSQL)
-- With SQLite and in-memory databases, a separate connection implies a
-- separate database. With 'actual databases', the data is persistent and
-- thus must be cleaned after each test.
-- TODO: there is certainly a better way...
cleanDB
  :: (forall m. RunDbMonad m
  => SqlPersistT (R.ResourceT m) ())
cleanDB = do
  delete $ from $ \(blogpost :: SqlExpr (Entity BlogPost))-> return ()
  delete $ from $ \(follow   :: SqlExpr (Entity Follow))  -> return ()
  delete $ from $ \(person   :: SqlExpr (Entity Person))  -> return ()
#endif


run, runSilent, runVerbose :: (forall m. RunDbMonad m => SqlPersistT (R.ResourceT m) a) -> IO a
runSilent  act = runNoLoggingT     $ run_worker act
runVerbose act = runStderrLoggingT $ run_worker act
run =
  if verbose
  then runVerbose
  else runSilent


verbose :: Bool
verbose = True


run_worker :: RunDbMonad m => SqlPersistT (R.ResourceT m) a -> m a
run_worker act =
  R.runResourceT .
#if defined(WITH_POSTGRESQL)
  withPostgresqlConn "host=localhost port=5432 user=test dbname=test" .
#elif defined (WITH_MYSQL)
  withMySQLConn defaultConnectInfo
    { connectHost     = "localhost"
    , connectUser     = "test"
    , connectPassword = "test"
    , connectDatabase = "test"
    } .
#else
  withSqliteConn ":memory:" .
#endif
  runSqlConn .
#if defined (WITH_POSTGRESQL) || defined (WITH_MYSQL)
  (runMigrationSilent migrateAll >>) $ (cleanDB >> act)
#else
  (runMigrationSilent migrateAll >>) $ act
#endif

unValue :: Value a -> a
unValue (Value a) = a
