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
           , CPP
           , TypeSynonymInstances
 #-}
module Main (main) where

import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Control.Exception (IOException)
import Control.Monad (forM_, replicateM, replicateM_, void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger (MonadLogger(..), runStderrLoggingT, runNoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Char (toLower, toUpper)
import Data.List (sortBy)
import Data.Monoid ((<>))
import Data.Ord (comparing)
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
#if MIN_VERSION_persistent_sqlite(2,1,3)
import Database.Sqlite (SqliteException)
#endif
#endif
import Database.Persist.TH
import Test.Hspec

import qualified Control.Monad.Trans.Resource as R
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Text.Lazy.Builder as TLB
import qualified Database.Esqueleto.PostgreSQL as EP
import qualified Database.Esqueleto.Internal.Sql as EI


-- Test schema
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
  Foo
    name Int
    Primary name
  Bar
    quux FooId

  Person
    name String
    age Int Maybe
    weight Int Maybe
    favNum Int
    deriving Eq Show
  BlogPost
    title String
    authorId PersonId
    deriving Eq Show
  Follow
    follower PersonId
    followed PersonId
    deriving Eq Show

  CcList
    names [String]

  Frontcover
    number Int
    title String
    Primary number
    deriving Eq Show
  Article
    title String
    frontcoverNumber Int
    Foreign Frontcover fkfrontcover frontcoverNumber
    deriving Eq Show
  Tag
    name String
    Primary name
    deriving Eq Show
  ArticleTag
    articleId ArticleId
    tagId     TagId
    Primary   articleId tagId
    deriving Eq Show
  Article2
    title String
    frontcoverId FrontcoverId
    deriving Eq Show
  Point
    x Int
    y Int
    name String
    Primary x y
    deriving Eq Show
  Circle
    centerX Int
    centerY Int
    name String
    Foreign Point fkpoint centerX centerY
    deriving Eq Show
  Numbers
    int    Int
    double Double
|]

-- | this could be achieved with S.fromList, but not all lists
--   have Ord instances
sameElementsAs :: Eq a => [a] -> [a] -> Bool
sameElementsAs l1 l2 = null (l1 L.\\ l2)

main :: IO ()
main = do
  let p1 = Person "John"   (Just 36) Nothing   1
      p2 = Person "Rachel" Nothing   (Just 37) 2
      p3 = Person "Mike"   (Just 17) Nothing   3
      p4 = Person "Livia"  (Just 17) (Just 18) 4
      p5 = Person "Mitch"  Nothing   Nothing   5
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

      it "works with non-id primary key" $
        run $ do
          let fc = Frontcover number ""
              number = 101
              Right thePk = keyFromValues [toPersistValue number]
          fcPk <- insert fc
          [Entity _ ret] <- select $ from $ return
          liftIO $ do
            ret `shouldBe` fc
            fcPk `shouldBe` thePk

      it "works when returning a custom non-composite primary key from a query" $
        run $ do
          let name = "foo"
              t = Tag name
              Right thePk = keyFromValues [toPersistValue name]
          tagPk <- insert t
          [Value ret] <- select $ from $ \t' -> return (t'^.TagId)
          liftIO $ do
            ret `shouldBe` thePk
            thePk `shouldBe` tagPk

      it "works when returning a composite primary key from a query" $
        run $ do
          let p = Point 10 20 ""
          thePk <- insert p
          [Value ppk] <- select $ from $ \p' -> return (p'^.PointId)
          liftIO $ ppk `shouldBe` thePk


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

      it "works with ForeignKey to a non-id primary key returning one entity" $
        run $ do
          let fc = Frontcover number ""
              article = Article "Esqueleto supports composite pks!" number
              number = 101
              Right thePk = keyFromValues [toPersistValue number]
          fcPk <- insert fc
          insert_ article
          [Entity _ retFc] <- select $
            from $ \(a `InnerJoin` f) -> do
              on (f^.FrontcoverNumber ==. a^.ArticleFrontcoverNumber)
              return f
          liftIO $ do
            retFc `shouldBe` fc
            fcPk `shouldBe` thePk

      it "works with a ForeignKey to a non-id primary key returning both entities" $
        run $ do
          let fc = Frontcover number ""
              article = Article "Esqueleto supports composite pks!" number
              number = 101
              Right thePk = keyFromValues [toPersistValue number]
          fcPk <- insert fc
          insert_ article
          [(Entity _ retFc, Entity _ retArt)] <- select $
            from $ \(a `InnerJoin` f) -> do
              on (f^.FrontcoverNumber ==. a^.ArticleFrontcoverNumber)
              return (f, a)
          liftIO $ do
            retFc `shouldBe` fc
            retArt `shouldBe` article
            fcPk `shouldBe` thePk
            articleFkfrontcover retArt `shouldBe` thePk

      it "works with a non-id primary key returning one entity" $
        run $ do
          let fc = Frontcover number ""
              article = Article2 "Esqueleto supports composite pks!" thePk
              number = 101
              Right thePk = keyFromValues [toPersistValue number]
          fcPk <- insert fc
          insert_ article
          [Entity _ retFc] <- select $
            from $ \(a `InnerJoin` f) -> do
              on (f^.FrontcoverId ==. a^.Article2FrontcoverId)
              return f
          liftIO $ do
            retFc `shouldBe` fc
            fcPk `shouldBe` thePk

      it "works with a composite primary key" $
        pendingWith "Persistent does not create the CircleFkPoint constructor. See: https://github.com/yesodweb/persistent/issues/341"
        {-
        run $ do
          let p = Point x y ""
              c = Circle x y ""
              x = 10
              y = 15
              Right thePk = keyFromValues [toPersistValue x, toPersistValue y]
          pPk <- insert p
          insert_ c
          [Entity _ ret] <- select $ from $ \(c' `InnerJoin` p') -> do
            on (p'^.PointId ==. c'^.CircleFkpoint)
            return p'
          liftIO $ do
            ret `shouldBe` p
            pPk `shouldBe` thePk
       -}

      it "works when joining via a non-id primary key" $
        run $ do
          let fc = Frontcover number ""
              article = Article "Esqueleto supports composite pks!" number
              tag = Tag "foo"
              otherTag = Tag "ignored"
              number = 101
          insert_ fc
          insert_ otherTag
          artId <- insert article
          tagId <- insert tag
          insert_ $ ArticleTag artId tagId
          [(Entity _ retArt, Entity _ retTag)] <- select $
            from $ \(a `InnerJoin` at `InnerJoin` t) -> do
              on (t^.TagId ==. at^.ArticleTagTagId)
              on (a^.ArticleId ==. at^.ArticleTagArticleId)
              return (a, t)
          liftIO $ do
            retArt `shouldBe` article
            retTag `shouldBe` tag

      it "respects the associativity of joins" $
        run $ do
            void $ insert p1
            ps <- select . from $
                      \((p :: SqlExpr (Entity Person))
                       `LeftOuterJoin`
                        ((_q :: SqlExpr (Entity Person))
                         `InnerJoin` (_r :: SqlExpr (Entity Person)))) -> do
                on (val False) -- Inner join is empty
                on (val True)
                return p
            liftIO $ (entityVal <$> ps) `shouldBe` [p1]

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

      it "works with lower_" $
        run $ do
          p1e <- insert' p1
          p2e@(Entity _ bob) <- insert' $ Person "bob" (Just 36) Nothing   1

          -- lower(name) == 'john'
          ret1 <- select $
                  from $ \p-> do
                  where_ (lower_ (p ^. PersonName) ==. val (map toLower $ personName p1))
                  return p
          liftIO $ ret1 `shouldBe` [ p1e ]

          -- name == lower('BOB')
          ret2 <- select $
                  from $ \p-> do
                  where_ (p ^. PersonName ==. lower_ (val $ map toUpper $ personName bob))
                  return p
          liftIO $ ret2 `shouldBe` [ p2e ]

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

      it "works with a composite primary key" $
        run $ do
          let p = Point x y ""
              x = 10
              y = 15
              Right thePk = keyFromValues [toPersistValue x, toPersistValue y]
          pPk <- insert p
          [Entity _ ret] <- select $ from $ \p' -> do
            where_ (p'^.PointId ==. val pPk)
            return p'
          liftIO $ do
            ret `shouldBe` p
            pPk `shouldBe` thePk


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
#ifdef WITH_POSTGRESQL
          liftIO $ ret `shouldBe` [ p4e, p3e, p1e , p2e ]
#else
          -- in SQLite and MySQL, its the reverse
          liftIO $ ret `shouldBe` [ p2e, p4e, p3e, p1e ]
#endif

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

      it "works on a composite primary key" $
        run $ do
          let ps = [Point 2 1 "", Point 1 2 ""]
          mapM_ insert ps
          eps <- select $
            from $ \p' -> do
              orderBy [asc (p'^.PointId)]
              return p'
          liftIO $ map entityVal eps `shouldBe` reverse ps


    describe "SELECT DISTINCT" $ do
      let selDistTest
            :: (   forall m. RunDbMonad m
                => SqlQuery (SqlExpr (Value String))
                -> SqlPersistT (R.ResourceT m) [Value String])
            -> IO ()
          selDistTest q =
            run $ do
              p1k <- insert p1
              let (t1, t2, t3) = ("a", "b", "c")
              mapM_ (insert . flip BlogPost p1k) [t1, t3, t2, t2, t1]
              ret <- q $
                     from $ \b -> do
                     let title = b ^. BlogPostTitle
                     orderBy [asc title]
                     return title
              liftIO $ ret `shouldBe` [ Value t1, Value t2, Value t3 ]
      it "works on a simple example (selectDistinct)" $
        selDistTest selectDistinct

      it "works on a simple example (select . distinct)" $
        selDistTest (select . distinct)

      it "works on a simple example (distinct (return ()))" $
        selDistTest (\act -> select $ distinct (return ()) >> act)

#if defined(WITH_POSTGRESQL)
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
          liftIO $ ret `shouldBe` sortBy (comparing (blogPostAuthorId . entityVal)) [bpB, bpC]

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
              liftIO $ ret `shouldBe` sortBy (comparing cmp) [bpA, bpB, bpC]
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
#endif

    describe "coalesce/coalesceDefault" $ do
      it "works on a simple example" $
        run $ do
          mapM_ insert' [p1, p2, p3, p4, p5]
          ret1 <- select $
                  from $ \p -> do
                  orderBy [asc (p ^. PersonId)]
                  return (coalesce [p ^. PersonAge, p ^. PersonWeight])
          liftIO $ ret1 `shouldBe` [ Value (Just (36 :: Int))
                                   , Value (Just 37)
                                   , Value (Just 17)
                                   , Value (Just 17)
                                   , Value Nothing
                                   ]

          ret2 <- select $
                  from $ \p -> do
                  orderBy [asc (p ^. PersonId)]
                  return (coalesceDefault [p ^. PersonAge, p ^. PersonWeight] (p ^. PersonFavNum))
          liftIO $ ret2 `shouldBe` [ Value (36 :: Int)
                                   , Value 37
                                   , Value 17
                                   , Value 17
                                   , Value 5
                                   ]

      it "works with sub-queries" $
        run $ do
          p1id <- insert p1
          p2id <- insert p2
          p3id <- insert p3
          _    <- insert p4
          _    <- insert p5
          _ <- insert $ BlogPost "a" p1id
          _ <- insert $ BlogPost "b" p2id
          _ <- insert $ BlogPost "c" p3id
          ret <- select $
                 from $ \b -> do
                   let sub =
                           from $ \p -> do
                           where_ (p ^. PersonId ==. b ^. BlogPostAuthorId)
                           return $ p ^. PersonAge
                   return $ coalesceDefault [sub_select sub] (val (42 :: Int))
          liftIO $ ret `shouldBe` [ Value (36 :: Int)
                                  , Value 42
                                  , Value 17
                                  ]

#if defined(WITH_POSTGRESQL) || defined(WITH_MYSQL)
      it "works on PostgreSQL and MySQL with <2 arguments" $
        run $ do
          _ :: [Value (Maybe Int)] <-
            select $
            from $ \p -> do
            return (coalesce [p ^. PersonAge])
          return ()
#else
      it "throws an exception on SQLite with <2 arguments" $
        run (select $
             from $ \p -> do
             return (coalesce [p ^. PersonAge]) :: SqlQuery (SqlExpr (Value (Maybe Int)))
#if MIN_VERSION_persistent_sqlite(2,1,3)
        ) `shouldThrow` (\(_ :: SqliteException) -> True)
#else
        ) `shouldThrow` (\(_ :: IOException) -> True)
#endif
#endif

    describe "text functions" $ do
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

#if defined(WITH_POSTGRESQL)
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
#endif

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
          liftIO $ ret `shouldBe` [ Entity p1k (Person anon (Just 73) Nothing 1)
                                  , Entity p2k (Person anon Nothing (Just 37) 2)
                                  , Entity p3k p3 ]
          -- MySQL: nulls appear first, and update returns actual number
          --        of changed rows
#elif defined(WITH_MYSQL)
          liftIO $ n `shouldBe` 1
          liftIO $ ret `shouldBe` [ Entity p2k (Person anon Nothing (Just 37) 2)
                                  , Entity p1k (Person anon (Just 73) Nothing 1)
                                  , Entity p3k p3 ]
#else
          -- SQLite: nulls appear first, update returns matched rows.
          liftIO $ n `shouldBe` 2
          liftIO $ ret `shouldBe` [ Entity p2k (Person anon Nothing (Just 37) 2)
                                  , Entity p1k (Person anon (Just 73) Nothing 1)
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

      it "works with a composite primary key" $
        pendingWith "Need refactor to support composite pks on ESet"
        {-
        run $ do
          let p = Point x y ""
              x = 10
              y = 15
              newX = 20
              newY = 25
              Right newPk = keyFromValues [toPersistValue newX, toPersistValue newY]
          insert_ p
          () <- update $ \p' -> do
                set p' [PointId =. val newPk]
          [Entity _ ret] <- select $ from $ return
          liftIO $ do
            ret `shouldBe` Point newX newY []
        -}

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
                 orderBy [asc (p ^. PersonName)]
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


    describe "list fields" $ do
      -- <https://github.com/prowdsponsor/esqueleto/issues/100>
      it "can update list fields" $
        run $ do
          cclist <- insert $ CcList []
          update $ \p -> do
            set p [ CcListNames =. val ["fred"]]
            where_ (p ^. CcListId ==. val cclist)

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

    describe "inserts by select, returns count" $ do
      it "IN works for insertSelectCount" $
        run $ do
          _ <- insert p1
          _ <- insert p2
          _ <- insert p3
          cnt <- insertSelectCount $ from $ \p -> do
            return $ BlogPost <# val "FakePost" <&> (p ^. PersonId)
          ret <- select $ from (\(_::(SqlExpr (Entity BlogPost))) -> return countRows)
          liftIO $ ret `shouldBe` [Value (3::Int)]
          liftIO $ cnt `shouldBe` 3

    describe "Math-related functions" $ do
      it "rand returns result in random order" $
        run $ do
          replicateM_ 20 $ do
            _ <- insert p1
            _ <- insert p2
            _ <- insert p3
            _ <- insert p4
            _ <- insert $ Person "Jane"  Nothing Nothing 0
            _ <- insert $ Person "Mark"  Nothing Nothing 0
            _ <- insert $ Person "Sarah" Nothing Nothing 0
            insert $ Person "Paul"  Nothing Nothing 0
          ret1 <- fmap (map unValue) $ select $ from $ \p -> do
                    orderBy [rand]
                    return (p ^. PersonId)
          ret2 <- fmap (map unValue) $ select $ from $ \p -> do
                    orderBy [rand]
                    return (p ^. PersonId)

          liftIO $ (ret1 == ret2) `shouldBe` False

      it "castNum works for multiplying Int and Double" $
        run $ do
          mapM_ insert [Numbers 2 3.4, Numbers 7 1.1]
          ret <-
            select $
            from $ \n -> do
            let r = castNum (n ^. NumbersInt) *. n ^. NumbersDouble
            orderBy [asc r]
            return r
          liftIO $ length ret `shouldBe` 2
          let [Value a, Value b] = ret
          liftIO $ max (abs (a - 6.8)) (abs (b - 7.7)) `shouldSatisfy` (< 0.01)

    describe "case" $ do
      it "Works for a simple value based when - False" $
        run $ do
          ret <- select $
            return $
              case_
                [ when_ (val False) then_ (val (1 :: Int)) ]
                (else_ (val 2))

          liftIO $ ret `shouldBe` [ Value 2 ]

      it "Works for a simple value based when - True" $
        run $ do
          ret <- select $
            return $
              case_
                [ when_ (val True) then_ (val (1 :: Int)) ]
                (else_ (val 2))

          liftIO $ ret `shouldBe` [ Value 1 ]

      it "works for a semi-complicated query" $
        run $ do
          _ <- insert p1
          _ <- insert p2
          _ <- insert p3
          _ <- insert p4
          _ <- insert p5
          ret <- select $
            return $
              case_
                [ when_
                    (exists $ from $ \p -> do
                        where_ (p ^. PersonName ==. val "Mike"))
                  then_
                    (sub_select $ from $ \v -> do
                        let sub =
                                from $ \c -> do
                                where_ (c ^. PersonName ==. val "Mike")
                                return (c ^. PersonFavNum)
                        where_ (v ^. PersonFavNum >. sub_select sub)
                        return $ count (v ^. PersonName) +. val (1 :: Int)) ]
                (else_ $ val (-1))

          liftIO $ ret `shouldBe` [ Value (3) ]

    describe "locking" $ do
      -- The locking clause is the last one, so try to use many
      -- others to test if it's at the right position.  We don't
      -- care about the text of the rest, nor with the RDBMS'
      -- reaction to the clause.
      let sanityCheck kind syntax = do
            let complexQuery =
                  from $ \(p1 `InnerJoin` p2) -> do
                  on (p1 ^. PersonName ==. p2 ^. PersonName)
                  where_ (p1 ^. PersonFavNum >. val 2)
                  orderBy [desc (p2 ^. PersonAge)]
                  limit 3
                  offset 9
                  groupBy (p1 ^. PersonId)
                  having (countRows <. val (0 :: Int))
                  return (p1, p2)
                queryWithClause1 = do
                  r <- complexQuery
                  locking kind
                  return r
                queryWithClause2 = do
                  locking ForUpdate
                  r <- complexQuery
                  locking ForShare
                  locking kind
                  return r
                queryWithClause3 = do
                  locking kind
                  complexQuery
                toText conn q =
                  let (tlb, _) = EI.toRawSql EI.SELECT (conn, EI.initialIdentState) q
                  in TLB.toLazyText tlb
            [complex, with1, with2, with3] <-
              runNoLoggingT $ withConn $ \conn -> return $
                map (toText conn) [complexQuery, queryWithClause1, queryWithClause2, queryWithClause3]
            let expected = complex <> "\n" <> syntax
            (with1, with2, with3) `shouldBe` (expected, expected, expected)

      it "looks sane for ForUpdate"       $ sanityCheck ForUpdate       "FOR UPDATE"
      it "looks sane for ForShare"        $ sanityCheck ForShare        "FOR SHARE"
      it "looks sane for LockInShareMode" $ sanityCheck LockInShareMode "LOCK IN SHARE MODE"

    describe "counting rows" $ do
      forM_ [ ("count (test A)",    count . (^. PersonAge),         4)
            , ("count (test B)",    count . (^. PersonWeight),      5)
            , ("countRows",         const countRows,                5)
            , ("countDistinct",     countDistinct . (^. PersonAge), 2) ] $
        \(title, countKind, expected) ->
        it (title ++ " works as expected") $
          run $ do
            mapM_ insert
              [ Person "" (Just 1) (Just 1) 1
              , Person "" (Just 2) (Just 1) 1
              , Person "" (Just 2) (Just 1) 1
              , Person "" (Just 2) (Just 2) 1
              , Person "" Nothing  (Just 3) 1]
            [Value n] <- select $ from $ return . countKind
            liftIO $ (n :: Int) `shouldBe` expected

    describe "PostgreSQL module" $ do
      it "should be tested on the PostgreSQL database" $
#if !defined(WITH_POSTGRESQL)
        pendingWith "test suite not running under PostgreSQL, skipping"
#else
        (return () :: IO ())

      it "arrayAgg looks sane" $
        run $ do
          let people = [p1, p2, p3, p4, p5]
          mapM_ insert people
          [Value ret] <-
            select $
            from $ \p -> do
            return (EP.arrayAgg (p ^. PersonName))
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
#endif

----------------------------------------------------------------------


insert' :: ( Functor m
           , PersistStore (PersistEntityBackend val)
           , MonadIO m
           , PersistEntity val )
        => val -> ReaderT (PersistEntityBackend val) m (Entity val)
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
  delete $ from $ \(_ :: SqlExpr (Entity BlogPost))   -> return ()
  delete $ from $ \(_ :: SqlExpr (Entity Follow))     -> return ()
  delete $ from $ \(_ :: SqlExpr (Entity Person))     -> return ()

  delete $ from $ \(_ :: SqlExpr (Entity ArticleTag)) -> return ()
  delete $ from $ \(_ :: SqlExpr (Entity Article))    -> return ()
  delete $ from $ \(_ :: SqlExpr (Entity Article2))   -> return ()
  delete $ from $ \(_ :: SqlExpr (Entity Tag))        -> return ()
  delete $ from $ \(_ :: SqlExpr (Entity Frontcover)) -> return ()

  delete $ from $ \(_ :: SqlExpr (Entity Circle))     -> return ()
  delete $ from $ \(_ :: SqlExpr (Entity Point))      -> return ()

  delete $ from $ \(_ :: SqlExpr (Entity Numbers))    -> return ()
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
run_worker act = withConn $ runSqlConn (migrateIt >> act)


migrateIt :: RunDbMonad m => SqlPersistT (R.ResourceT m) ()
migrateIt = do
  void $ runMigrationSilent migrateAll
#if defined (WITH_POSTGRESQL) || defined (WITH_MYSQL)
  cleanDB
#endif


withConn :: RunDbMonad m => (SqlBackend -> R.ResourceT m a) -> m a
withConn =
  R.runResourceT .
#if defined(WITH_POSTGRESQL)
  withPostgresqlConn "host=localhost port=5432 user=test dbname=test"
#elif defined (WITH_MYSQL)
  withMySQLConn defaultConnectInfo
    { connectHost     = "localhost"
    , connectUser     = "test"
    , connectPassword = "test"
    , connectDatabase = "test"
    }
#else
  withSqliteConn ":memory:"
#endif
