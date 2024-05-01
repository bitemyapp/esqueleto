{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

#if __GLASGOW_HASKELL__ >= 902
{-# LANGUAGE OverloadedRecordDot #-}
#endif

{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Common.Test
    ( tests
    , testLocking
    , testAscRandom
    , testRandomMath
    , migrateAll
    , migrateUnique
    , cleanDB
    , cleanUniques
    , updateRethrowingQuery
    , selectRethrowingQuery
    , p1, p2, p3, p4, p5
    , l1, l2, l3
    , u1, u2, u3, u4
    , insert'
    , EntityField (..)
    , Foo (..)
    , Bar (..)
    , Person (..)
    , BlogPost (..)
    , Lord (..)
    , Deed (..)
    , Follow (..)
    , CcList (..)
    , Frontcover (..)
    , Article (..)
    , Tag (..)
    , ArticleTag (..)
    , Article2 (..)
    , Point (..)
    , Circle (..)
    , Numbers (..)
    , OneUnique(..)
    , Unique(..)
    , DateTruncTest(..)
    , DateTruncTestId
    , Key(..)
    ) where

import Common.Test.Import hiding (from, on)

import Control.Monad (forM_, replicateM, replicateM_, void)
import Data.Char (toLower, toUpper)
import Database.Esqueleto

import Data.Conduit (ConduitT, runConduit, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Text as Text
import qualified Data.Text.Internal.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Database.Esqueleto.Internal.ExprParser as P
import qualified Database.Esqueleto.Internal.Internal as EI
import Database.Esqueleto.PostgreSQL as EP
import Database.Persist.Class.PersistEntity
import qualified UnliftIO.Resource as R

import Common.Record (testDeriveEsqueletoRecord)
import Common.Test.Select

-- Test schema
-- | this could be achieved with S.fromList, but not all lists
--   have Ord instances
sameElementsAs :: Eq a => [a] -> [a] -> Bool
sameElementsAs l1' l2' = null (l1' L.\\ l2')

-- | Helper for rounding to a specific digit
--   Prelude> map (flip roundTo 12.3456) [0..5]
--   [12.0, 12.3, 12.35, 12.346, 12.3456, 12.3456]
roundTo :: (Fractional a, RealFrac a1, Integral b) => b -> a1 -> a
roundTo n f =
    (fromInteger $ round $ f * (10^n)) / (10.0^^n)

p1 :: Person
p1 = Person "John"   (Just 36) Nothing   1

p2 :: Person
p2 = Person "Rachel" Nothing   (Just 37) 2

p3 :: Person
p3 = Person "Mike"   (Just 17) Nothing   3

p4 :: Person
p4 = Person "Livia"  (Just 17) (Just 18) 4

p5 :: Person
p5 = Person "Mitch"  Nothing   Nothing   5

l1 :: Lord
l1 = Lord "Cornwall" (Just 36)

l2 :: Lord
l2 = Lord "Dorset" Nothing

l3 :: Lord
l3 = Lord "Chester" (Just 17)

u1 :: OneUnique
u1 = OneUnique "First" 0

u2 :: OneUnique
u2 = OneUnique "Second" 1

u3 :: OneUnique
u3 = OneUnique "Third" 0

u4 :: OneUnique
u4 = OneUnique "First" 2

testSubSelect :: SpecDb
testSubSelect = do
    let setup :: MonadIO m => SqlPersistT m ()
        setup = do
            _ <- insert $ Numbers 1 2
            _ <- insert $ Numbers 2 4
            _ <- insert $ Numbers 3 5
            _ <- insert $ Numbers 6 7
            pure ()

    describe "subSelect" $ do
        itDb "is safe for queries that may return multiple results" $ do
            let query = do
                    n <- from $ table @Numbers
                    orderBy [asc (n ^. NumbersInt)]
                    pure (n ^. NumbersInt)
            setup
            res <- select $ pure $ subSelect query
            eres <- try $ do
                select $ pure $ sub_select query
            asserting $ do
                res `shouldBe` [Value (Just 1)]
                case eres of
                    Left (SomeException _) ->
                        -- We should receive an exception, but the different database
                        -- libraries throw different exceptions. Hooray.
                        pure ()
                    Right v ->
                        -- This shouldn't happen, but in sqlite land, many things are
                        -- possible.
                        v `shouldBe` [Value 1]

        itDb "is safe for queries that may not return anything" $ do
            let query = do
                    n <- from $ table @Numbers
                    orderBy [asc (n ^. NumbersInt)]
                    limit 1
                    pure (n ^. NumbersInt)
            setup
            res <- select $ pure $ subSelect query
            transactionUndo

            eres <- try $ do
                select $ pure $ sub_select query

            asserting $ do
                res `shouldBe` [Value $ Just 1]
                case eres of
                    Left (_ :: PersistException) ->
                        -- We expect to receive this exception. However, sqlite evidently has
                        -- no problems with itDb, so we can't *require* that the exception is
                        -- thrown. Sigh.
                        pure ()
                    Right v ->
                        -- This shouldn't happen, but in sqlite land, many things are
                        -- possible.
                        v `shouldBe` [Value 1]

    describe "subSelectList" $ do
        itDb "is safe on empty databases as well as good databases" $ do
            let query = do
                    n <- from $ table @Numbers
                    where_ $ n ^. NumbersInt `in_` do
                        subSelectList $ do
                            n' <- from $ table @Numbers
                            where_ $ n' ^. NumbersInt >=. val 3
                            pure (n' ^. NumbersInt)
                    pure n
            empty <- select query

            full <- do
                setup
                select query

            asserting $ do
                empty `shouldBe` []
                full `shouldSatisfy` (not . null)

    describe "subSelectMaybe" $ do
        itDb "is equivalent to joinV . subSelect" $ do
            let query selector = do
                    n <- from $ table @Numbers
                    pure $
                        selector $ do
                        n' <- from $ table @Numbers
                        where_ $ n' ^. NumbersDouble >=. n ^. NumbersDouble
                        pure (max_ (n' ^. NumbersInt))

            setup
            a <- select (query subSelectMaybe)
            b <- select (query (joinV . subSelect))
            asserting $ a `shouldBe` b

    describe "subSelectCount" $ do
        itDb "is a safe way to do a countRows" $ do
            setup
            xs0 <-
                select $ do
                    n <- from $ table @Numbers
                    pure $ (,) n $
                        subSelectCount @Int $ do
                        n' <- from $ table @Numbers
                        where_ $ n' ^. NumbersInt >=. n ^. NumbersInt

            xs1 <-
                select $ do
                    n <- from $ table @Numbers
                    pure $ (,) n $
                        subSelectUnsafe $ do
                        n' <- from $ table @Numbers
                        where_ $ n' ^. NumbersInt >=. n ^. NumbersInt
                        pure countRows

            let getter (Entity _ a, b) = (a, b)
            asserting $
                map getter xs0 `shouldBe` map getter xs1

    describe "subSelectUnsafe" $ do
        itDb "throws exceptions on multiple results" $ do
            setup
            eres <- try $ do
                bad <- select $ do
                    n <- from $ table @Numbers
                    pure $ (,) (n ^. NumbersInt) $
                        subSelectUnsafe $ do
                        n' <- from $ table @Numbers
                        pure (just (n' ^. NumbersDouble))
                good <- select $ do
                    n <- from $ table @Numbers
                    pure $ (,) (n ^. NumbersInt) $
                        subSelect $ do
                        n' <- from $ table @Numbers
                        pure (n' ^. NumbersDouble)
                pure (bad, good)
            asserting $ case eres of
                Left (SomeException _) ->
                    -- Must use SomeException because the database libraries throw their
                    -- own errors.
                    pure ()
                Right (bad, good) -> do
                    -- SQLite just takes the first element of the sub-select. lol.
                    bad `shouldBe` good

        itDb "throws exceptions on null results" $ do
            setup
            eres <- try $ do
                select $ do
                    n <- from $ table @Numbers
                    pure $ (,) (n ^. NumbersInt) $
                        subSelectUnsafe $ do
                        n' <- from $ table @Numbers
                        where_ $ val False
                        pure (n' ^. NumbersDouble)
            asserting $ case eres of
                Left (_ :: PersistException) ->
                    pure ()
                Right xs ->
                    xs `shouldBe` []

testSelectOne :: SpecDb
testSelectOne =
    describe "selectOne" $ do
        let personQuery =
                selectOne $ do
                    person <- from $ table @Person
                    where_ $ person ^. PersonFavNum >=. val 1
                    orderBy [asc (person ^. PersonId)]
                    return $ person ^. PersonId
        itDb "returns Just" $ do
            person <- insert' p1
            _ <- insert' p2
            res <- personQuery
            asserting $
                res `shouldBe` Just (Value $ entityKey person)

        itDb "returns Nothing" $ do
            res <- personQuery
            asserting $
                res `shouldBe` (Nothing :: Maybe (Value PersonId))

testSelectSource :: SpecDb
testSelectSource = do
    describe "selectSource" $ do
        itDb "works for a simple example" $ do
            let query :: ConduitT () (Entity Person) (SqlPersistT (R.ResourceT IO)) ()
                query = selectSource $ from $ table @Person
            p1e <- insert' p1
            ret <- mapReaderT R.runResourceT $ runConduit $ query .| CL.consume
            asserting $ ret `shouldBe` [ p1e ]

        itDb "can run a query many times" $ do
            let query :: ConduitT () (Entity Person) (SqlPersistT (R.ResourceT IO)) ()
                query = selectSource $ from $ table @Person
            p1e <- insert' p1
            ret0 <- mapReaderT R.runResourceT $ runConduit $ query .| CL.consume
            ret1 <- mapReaderT R.runResourceT $ runConduit $ query .| CL.consume
            asserting $ do
                ret0 `shouldBe` [ p1e ]
                ret1 `shouldBe` [ p1e ]

        itDb "works on repro" $ do
            let selectPerson :: R.MonadResource m => String -> ConduitT () (Key Person) (SqlPersistT m) ()
                selectPerson name = do
                    let source =
                            selectSource $ do
                                person <- from $ table @Person
                                where_ $ person ^. PersonName ==. val name
                                return $ person ^. PersonId
                    source .| CL.map unValue
            p1e <- insert' p1
            p2e <- insert' p2
            r1 <- mapReaderT R.runResourceT $ runConduit $ selectPerson (personName p1) .| CL.consume
            r2 <- mapReaderT R.runResourceT $ runConduit $ selectPerson (personName p2) .| CL.consume
            asserting $ do
                r1 `shouldBe` [ entityKey p1e ]
                r2 `shouldBe` [ entityKey p2e ]

testSelectFrom :: SpecDb
testSelectFrom = do
    describe "select/from" $ do
        itDb "works for a simple example" $ do
            p1e <- insert' p1
            ret <- select $ from $ table @Person
            asserting $ ret `shouldBe` [ p1e ]

        itDb "works for a simple example using Table" $ do
            p1e <- insert' p1
            ret <- select $ from $ Table @Person
            asserting $ ret `shouldBe` [ p1e ]


        itDb "works for a self-join via sub_select" $ do
            p1k <- insert p1
            p2k <- insert p2
            _f1k <- insert (Follow p1k p2k)
            _f2k <- insert (Follow p2k p1k)
            ret <- select $ do
                    followA <- from $ table @Follow
                    let subquery = do
                         followB <- from $ table @Follow
                         where_ $ followA ^. FollowFollower ==. followB ^. FollowFollowed
                         return $ followB ^. FollowFollower
                    where_ $ followA ^. FollowFollowed ==. sub_select subquery
                    return followA
            asserting $ length ret `shouldBe` 2

        itDb "works for a self-join via exists" $ do
            p1k <- insert p1
            p2k <- insert p2
            _f1k <- insert (Follow p1k p2k)
            _f2k <- insert (Follow p2k p1k)
            ret <- select $ do
                    followA <- from $ table @Follow
                    where_ $ exists $ do
                        followB <- from $ table @Follow
                        where_ $ followA ^. FollowFollower ==. followB ^. FollowFollowed
                    return followA
            asserting $ length ret `shouldBe` 2


        itDb "works for a simple projection" $ do
            p1k <- insert p1
            p2k <- insert p2
            ret <- select $ do
                    p <- from $ table @Person
                    return (p ^. PersonId, p ^. PersonName)
            asserting $ ret `shouldBe` [ (Value p1k, Value (personName p1))
                                    , (Value p2k, Value (personName p2)) ]


        itDb "works with many kinds of LIMITs and OFFSETs" $ do
            [p1e, p2e, p3e, p4e] <- mapM insert' [p1, p2, p3, p4]
            let people = do
                    p <- from $ table @Person
                    orderBy [asc (p ^. PersonName)]
                    return p
            ret1 <-
                select $ do
                p <- people
                limit 2
                limit 1
                return p
            asserting $ ret1 `shouldBe` [ p1e ]
            ret2 <-
                select $ do
                p <- people
                limit 1
                limit 2
                return p
            asserting $ ret2 `shouldBe` [ p1e, p4e ]
            ret3 <-
                select $ do
                p <- people
                offset 3
                offset 2
                return p
            asserting $ ret3 `shouldBe` [ p3e, p2e ]
            ret4 <-
                select $ do
                p <- people
                offset 3
                limit 5
                offset 2
                limit 3
                offset 1
                limit 2
                return p
            asserting $ ret4 `shouldBe` [ p4e, p3e ]
            ret5 <-
                select $ do
                p <- people
                offset 1000
                limit  1
                limit  1000
                offset 0
                return p
            asserting $ ret5 `shouldBe` [ p1e, p4e, p3e, p2e ]

        itDb "works with non-id primary key" $ do
            let fc = Frontcover number ""
                number = 101 :: Int
                Right thePk = keyFromValues [toPersistValue number]
            fcPk <- insert fc
            [Entity _ ret] <- select $ from $ table @Frontcover
            asserting $ do
                ret `shouldBe` fc
                fcPk `shouldBe` thePk

        itDb "works when returning a custom non-composite primary key from a query" $ do
            let name = "foo"
                t = Tag name
                Right thePk = keyFromValues [toPersistValue name]
            tagPk <- insert t
            [Value ret] <- select $ do
                             t' <- from $ table @Tag
                             return (t'^.TagId)
            asserting $ do
                ret `shouldBe` thePk
                thePk `shouldBe` tagPk

        itDb "works when returning a composite primary key from a query" $ do
            let p = Point 10 20 ""
            thePk <- insert p
            [Value ppk] <- select $ do
                                p' <- from $ table @Point
                                return (p'^.PointId)
            asserting $ ppk `shouldBe` thePk

testSelectJoin :: SpecDb
testSelectJoin = do
  describe "select:JOIN" $ do
    itDb "works with a LEFT OUTER JOIN" $
      do
        p1e <- insert' p1
        p2e <- insert' p2
        p3e <- insert' p3
        p4e <- insert' p4
        b12e <- insert' $ BlogPost "b" (entityKey p1e)
        b11e <- insert' $ BlogPost "a" (entityKey p1e)
        b31e <- insert' $ BlogPost "c" (entityKey p3e)
        ret <- select $ do
                (p :& mb) <- from $ table @Person `leftJoin` table @BlogPost
                                `on` (\(p :& mb) ->
                                    just (p ^. PersonId) ==. mb ?. BlogPostAuthorId)
                orderBy [ asc (p ^. PersonName), asc (mb ?. BlogPostTitle) ]
                return (p, mb)
        asserting $ ret `shouldBe` [ (p1e, Just b11e)
                                , (p1e, Just b12e)
                                , (p4e, Nothing)
                                , (p3e, Just b31e)
                                , (p2e, Nothing) ]

    itDb "typechecks (A LEFT OUTER JOIN (B LEFT OUTER JOIN C))" $
        let
            _x :: SqlPersistT IO [Entity Person]
            _x =
                select $ do
                    (a :& _) <- from $ table @Person
                                `leftJoin` (table @Person
                                    `leftJoin` table @Person
                                    `on` (\(person2 :& person3) -> just (person2 ^. PersonId) ==. person3 ?. PersonId))
                                `on` (\(person1 :& (person2 :& _)) ->
                                    just (person1 ^. PersonId) ==. person2 ?. PersonId)

                    return a
      in asserting noExceptions

    itDb "typechecks ((A LEFT OUTER JOIN B) LEFT OUTER JOIN C)" $
        let _x :: SqlPersistT IO [Entity Person]
            _x =
                select $ do
                    (a :& _ :& _) <-
                        from $ (table @Person
                            `leftJoin` table @Person
                            `on` (\(person1 :& person2) ->
                                just (person1 ^. PersonId) ==. person2 ?. PersonId))
                            `leftJoin` table @Person
                            `on` (\(_ :& person2 :& person3) -> person2 ?. PersonId ==. person3 ?. PersonId)
                    return a
        in asserting noExceptions

    itDb "works with ForeignKey to a non-id primary key returning one entity" $
      do
        let fc = Frontcover number ""
            article = Article "Esqueleto supports composite pks!" number
            number = 101
            Right thePk = keyFromValues [toPersistValue number]
        fcPk <- insert fc
        insert_ article
        [Entity _ retFc] <- select $ do
            (_a :& f) <-
                from $ table @Article
                `innerJoin` table @Frontcover
                `on` (\(a :& f) ->
                    f^.FrontcoverNumber ==. a^.ArticleFrontcoverNumber)
            return f
        asserting $ do
          retFc `shouldBe` fc
          fcPk `shouldBe` thePk

    itDb "allows using a primary key that is itself a key of another table" $
      do
        let number = 101
        insert_ $ Frontcover number ""
        articleId <- insert $ Article "title" number
        articleMetaE <- insert' (ArticleMetadata articleId)
        result <- select $ do
            articleMetadata <- from $ table @ArticleMetadata
            where_ $ articleMetadata ^. ArticleMetadataId ==. (val (ArticleMetadataKey articleId))
            pure articleMetadata
        asserting $ [articleMetaE] `shouldBe` result

    itDb "allows joining between a primary key that is itself a key of another table, using ToBaseId" $ do
      do
        let number = 101
        insert_ $ Frontcover number ""
        articleE@(Entity articleId _) <- insert' $ Article "title" number
        articleMetaE <- insert' (ArticleMetadata articleId)

        articlesAndMetadata <- select $ do
            (article :& articleMetadata) <-
                from $ table @Article
                `innerJoin` table @ArticleMetadata
                `on` (\(article :& articleMetadata) ->
                    toBaseId (articleMetadata ^. ArticleMetadataId) ==. article ^. ArticleId)
            return (article, articleMetadata)
        asserting $ [(articleE, articleMetaE)] `shouldBe` articlesAndMetadata

    itDb "works with a ForeignKey to a non-id primary key returning both entities" $
      do
        let fc = Frontcover number ""
            article = Article "Esqueleto supports composite pks!" number
            number = 101
            Right thePk = keyFromValues [toPersistValue number]
        fcPk <- insert fc
        insert_ article
        [(Entity _ retFc, Entity _ retArt)] <- select $ do
            (a :& f) <-
                from $ table @Article
                `innerJoin` table @Frontcover
                `on` (\(a :& f) ->
                    f ^. FrontcoverNumber ==. a ^. ArticleFrontcoverNumber)
            return (f, a)
        asserting $ do
          retFc `shouldBe` fc
          retArt `shouldBe` article
          fcPk `shouldBe` thePk
          articleFkfrontcover retArt `shouldBe` thePk

    itDb "works with a non-id primary key returning one entity" $
      do
        let fc = Frontcover number ""
            article = Article2 "Esqueleto supports composite pks!" thePk
            number = 101
            Right thePk = keyFromValues [toPersistValue number]
        fcPk <- insert fc
        insert_ article
        [Entity _ retFc] <- select $ do
            (_a :& f) <-
                from $ table @Article2
                `innerJoin` table @Frontcover
                `on` (\(a :& f) ->
                    f ^. FrontcoverId ==. a ^. Article2FrontcoverId)
            return f
        asserting $ do
          retFc `shouldBe` fc
          fcPk `shouldBe` thePk

    it "works with a composite primary key" $ \_ ->
      pendingWith "Persistent does not create the CircleFkPoint constructor. See: https://github.com/yesodweb/persistent/issues/341"
      {-
      do
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
        asserting $ do
          ret `shouldBe` p
          pPk `shouldBe` thePk
     -}

    itDb "works when joining via a non-id primary key" $
      do
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
        [(Entity _ retArt, Entity _ retTag)] <- select $ do
            (a :& _ :& t) <-
                from $ table @Article
                    `innerJoin` table @ArticleTag
                    `on` (\(a :& at) ->
                            a ^. ArticleId ==. at ^. ArticleTagArticleId)
                    `innerJoin` table @Tag
                    `on` (\(_ :& at :& t) ->
                            t ^. TagId ==. at ^. ArticleTagTagId)
            return (a, t)
        asserting $ do
          retArt `shouldBe` article
          retTag `shouldBe` tag

    itDb "respects the associativity of joins" $
      do
          void $ insert p1
          ps <- select $ do
              (p :& _) <-
                  from $ table @Person
                    `leftJoin` (table @Person
                        `innerJoin` table @Person
                        `on` (\_ -> val False))
                    `on` (\_ -> val True)
              return p
          asserting $ (entityVal <$> ps) `shouldBe` [p1]

testSelectSubQuery :: SpecDb
testSelectSubQuery = describe "select subquery" $ do
    itDb "works" $ do
        _ <- insert' p1
        let q = do
                p <- from $ table @Person
                return ( p ^. PersonName, p ^. PersonAge)
        ret <- select $ from q
        asserting $ ret `shouldBe` [ (Value $ personName p1, Value $ personAge p1) ]

    itDb "works with SubQuery wrapper" $ do
        _ <- insert' p1
        let q = do
                p <- from $ table @Person
                return ( p ^. PersonName, p ^. PersonAge)
        ret <- select $ from $ SubQuery q
        asserting $ ret `shouldBe` [ (Value $ personName p1, Value $ personAge p1) ]

    itDb "supports sub-selecting Maybe entities" $ do
        l1e <- insert' l1
        l3e <- insert' l3
        l1Deeds <- mapM (\k -> insert' $ Deed k (entityKey l1e)) (map show [1..3 :: Int])
        let l1WithDeeds = do d <- l1Deeds
                             pure (l1e, Just d)
        let q = from $ do
                  (lords :& deeds) <-
                      from $ table @Lord
                      `LeftOuterJoin` table @Deed
                      `on` (\(l :& d) -> just (l ^. LordId) ==. d ?. DeedOwnerId)
                  pure (lords, deeds)

        ret <- select q
        asserting $ ret `shouldMatchList` ((l3e, Nothing) : l1WithDeeds)

    itDb "lets you order by alias" $ do
        _ <- insert' p1
        _ <- insert' p3
        let q = do
                (name, age) <-
                  from $ do
                      p <- from $ table @Person
                      return ( p ^. PersonName, p ^. PersonAge)
                orderBy [ asc age ]
                pure name
        ret <- select q
        asserting $ ret `shouldBe` [ Value $ personName p3, Value $ personName p1 ]

    itDb "supports groupBy" $ do
        l1k <- insert l1
        l3k <- insert l3
        mapM_ (\k -> insert $ Deed k l1k) (map show [1..3 :: Int])

        mapM_ (\k -> insert $ Deed k l3k) (map show [4..10 :: Int])
        let q = do
                (lord :& deed) <- from $ table @Lord
                                        `InnerJoin` table @Deed
                                  `on` (\(lord :& deed) ->
                                                       lord ^. LordId ==. deed ^. DeedOwnerId)
                return (lord ^. LordId, deed ^. DeedId)
            q' = do
                 (lordId, deedId) <- from q
                 groupBy (lordId)
                 return (lordId, count deedId)
        (ret :: [(Value (Key Lord), Value Int)]) <- select q'

        asserting $ ret `shouldMatchList` [ (Value l3k, Value 7)
                                       , (Value l1k, Value 3) ]

    itDb "Can count results of aggregate query" $ do
        l1k <- insert l1
        l3k <- insert l3
        mapM_ (\k -> insert $ Deed k l1k) (map show [1..3 :: Int])

        mapM_ (\k -> insert $ Deed k l3k) (map show [4..10 :: Int])
        let q = do
                (lord :& deed) <- from $ table @Lord
                                        `InnerJoin` table @Deed
                                  `on` (\(lord :& deed) ->
                                                      lord ^. LordId ==. deed ^. DeedOwnerId)
                groupBy (lord ^. LordId)
                return (lord ^. LordId, count (deed ^. DeedId))

        (ret :: [(Value Int)]) <- select $ do
                 (lordId, deedCount) <- from q
                 where_ $ deedCount >. val (3 :: Int)
                 return (count lordId)

        asserting $ ret `shouldMatchList` [ (Value 1) ]

    itDb "joins on subqueries" $ do
        l1k <- insert l1
        l3k <- insert l3
        mapM_ (\k -> insert $ Deed k l1k) (map show [1..3 :: Int])

        mapM_ (\k -> insert $ Deed k l3k) (map show [4..10 :: Int])
        let q = do
                (lord :& deed) <- from $ table @Lord
                        `InnerJoin` (from $ table @Deed)
                        `on` (\(lord :& deed) ->
                                             lord ^. LordId ==. deed ^. DeedOwnerId)
                groupBy (lord ^. LordId)
                return (lord ^. LordId, count (deed ^. DeedId))
        (ret :: [(Value (Key Lord), Value Int)]) <- select q
        asserting $ ret `shouldMatchList` [ (Value l3k, Value 7)
                                       , (Value l1k, Value 3) ]

    itDb "flattens maybe values" $ do
        l1k <- insert l1
        l3k <- insert l3
        let q = do
                (lord :& (_, dogCounts)) <- from $ table @Lord
                        `LeftOuterJoin` do
                            lord <- from $ table @Lord
                            pure (lord ^. LordId, lord ^. LordDogs)
                        `on` (\(lord :& (lordId, _)) ->
                                             just (lord ^. LordId) ==. lordId)
                groupBy (lord ^. LordId, dogCounts)
                return (lord ^. LordId, dogCounts)
        (ret :: [(Value (Key Lord), Value (Maybe Int))]) <- select q
        asserting $ ret `shouldMatchList` [ (Value l3k, Value (lordDogs l3))
                                       , (Value l1k, Value (lordDogs l1)) ]
    itDb "unions" $ do
          _ <- insert p1
          _ <- insert p2
          let q = from $
                  (do
                    p <- from $ table @Person
                    where_ $ not_ $ isNothing $ p ^. PersonAge
                    return (p ^. PersonName))
                  `union_`
                  (do
                    p <- from $ table @Person
                    where_ $ isNothing $ p ^. PersonAge
                    return (p ^. PersonName))
                  `union_`
                  (do
                    p <- from $ table @Person
                    where_ $ isNothing $ p ^. PersonAge
                    return (p ^. PersonName))
          names <- select q
          asserting $ names `shouldMatchList` [ (Value $ personName p1)
                                           , (Value $ personName p2) ]

    itDb "allows re-using (:&) joined tables" $ do
      let q = do
              result@(_persons :& _profiles :& _posts) <-
                from $  table @Person
                         `InnerJoin` table @Profile
                   `on` (\(people :& profiles) ->
                                        people ^. PersonId ==. profiles ^. ProfilePerson)
                     `InnerJoin` table @BlogPost
                   `on` (\(people :& _ :& posts) ->
                                        people ^. PersonId ==. posts ^. BlogPostAuthorId)
              pure result
      _rows <- select $ do
        (persons :& profiles :& posts) <- from q
        pure (persons ^. PersonId, profiles ^. ProfileId, posts ^. BlogPostId)
      -- We don't care about the result of the query, only that it
      -- rendered & executed.
      asserting noExceptions

testSelectWhere :: SpecDb
testSelectWhere = describe "select where_" $ do
    itDb "works for a simple example with (==.)" $ do
        p1e <- insert' p1
        _   <- insert' p2
        _   <- insert' p3
        ret <- select $ do
               p <- from $ table @Person
               where_ (p ^. PersonName ==. val "John")
               return p
        asserting $ ret `shouldBe` [ p1e ]

    itDb "works for a simple example with (==.) and (||.)" $ do
        p1e <- insert' p1
        p2e <- insert' p2
        _   <- insert' p3
        ret <- select $ do
                 p <- from $ table @Person
                 where_ (p ^. PersonName ==. val "John" ||. p ^. PersonName ==. val "Rachel")
                 return p
        asserting $ ret `shouldBe` [ p1e, p2e ]

    itDb "works for a simple example with (>.) [uses val . Just]" $ do
        p1e <- insert' p1
        _   <- insert' p2
        _   <- insert' p3
        ret <- select $ do
                p <- from $ table @Person
                where_ (p ^. PersonAge >. val (Just 17))
                return p
        asserting $ ret `shouldBe` [ p1e ]

    itDb "works for a simple example with (>.) and not_ [uses just . val]" $ do
        _   <- insert' p1
        _   <- insert' p2
        p3e <- insert' p3
        ret <- select $ do
                p <- from $ table @Person
                where_ (not_ $ p ^. PersonAge >. just (val 17))
                return p
        asserting $ ret `shouldBe` [ p3e ]

    describe "when using between" $ do
        itDb "works for a simple example with [uses just . val]" $ do
            p1e  <- insert' p1
            _    <- insert' p2
            _    <- insert' p3
            ret  <- select $ do
                p <- from $ table @Person
                where_ ((p ^. PersonAge) `between` (just $ val 20, just $ val 40))
                return p
            asserting $ ret `shouldBe` [ p1e ]
        itDb "works for a proyected fields value" $ do
            _ <- insert' p1 >> insert' p2 >> insert' p3
            ret <-
              select $ do
                p <- from $ table @Person
                where_ $ just (p ^. PersonFavNum)
                  `between`
                    (p ^. PersonAge, p ^.  PersonWeight)
            asserting $ ret `shouldBe` []
        describe "when projecting composite keys" $ do
            itDb "works when using composite keys with val" $ do
                insert_ $ Point 1 2 ""
                ret <-
                  select $ do
                    p <- from $ table @Point
                    where_ $
                        p ^. PointId
                          `between`
                            ( val $ PointKey 1 2
                            , val $ PointKey 5 6 )
                asserting $ ret `shouldBe` [()]

    itDb "works with avg_" $ do
        _ <- insert' p1
        _ <- insert' p2
        _ <- insert' p3
        _ <- insert' p4
        ret <- select $ do
               p <- from $ table @Person
               return $ joinV $ avg_ (p ^. PersonAge)
        let testV :: Double
            testV = roundTo (4 :: Integer) $ (36 + 17 + 17) / (3 :: Double)

            retV :: [Value (Maybe Double)]
            retV = map (Value . fmap (roundTo (4 :: Integer)) . unValue) (ret :: [Value (Maybe Double)])
        asserting $ retV `shouldBe` [ Value $ Just testV ]

    itDb "works with min_" $
      do
        _ <- insert' p1
        _ <- insert' p2
        _ <- insert' p3
        _ <- insert' p4
        ret <- select $ do
               p <- from $ table @Person
               return $ joinV $ min_ (p ^. PersonAge)
        asserting $ ret `shouldBe` [ Value $ Just (17 :: Int) ]

    itDb "works with max_" $ do
        _ <- insert' p1
        _ <- insert' p2
        _ <- insert' p3
        _ <- insert' p4
        ret <- select $ do
               p <- from $ table @Person
               return $ joinV $ max_ (p ^. PersonAge)
        asserting $ ret `shouldBe` [ Value $ Just (36 :: Int) ]

    itDb "works with lower_" $ do
        p1e <- insert' p1
        p2e@(Entity _ bob) <- insert' $ Person "bob" (Just 36) Nothing   1

        -- lower(name) == 'john'
        ret1 <- select $ do
                p <- from $ table @Person
                where_ (lower_ (p ^. PersonName) ==. val (map toLower $ personName p1))
                return p
        asserting $ ret1 `shouldBe` [ p1e ]

        -- name == lower('BOB')
        ret2 <- select $ do
                p <- from $ table @Person
                where_ (p ^. PersonName ==. lower_ (val $ map toUpper $ personName bob))
                return p
        asserting $ ret2 `shouldBe` [ p2e ]

    itDb "works with round_" $ do
        ret <- select $ return $ round_ (val (16.2 :: Double))
        asserting $ ret `shouldBe` [ Value (16 :: Double) ]

    itDb "works with isNothing" $ do
        _   <- insert' p1
        p2e <- insert' p2
        _   <- insert' p3
        ret <- select $ do
                p <- from $ table @Person
                where_ $ isNothing (p ^. PersonAge)
                return p
        asserting $ ret `shouldBe` [ p2e ]

    itDb "works with not_ . isNothing" $ do
        p1e <- insert' p1
        _   <- insert' p2
        ret <- select $ do
                p <- from $ table @Person
                where_ $ not_ (isNothing (p ^. PersonAge))
                return p
        asserting $ ret `shouldBe` [ p1e ]

    itDb "works for a many-to-many explicit join" $ do
        p1e@(Entity p1k _) <- insert' p1
        p2e@(Entity p2k _) <- insert' p2
        _                  <- insert' p3
        p4e@(Entity p4k _) <- insert' p4
        f12 <- insert' (Follow p1k p2k)
        f21 <- insert' (Follow p2k p1k)
        f42 <- insert' (Follow p4k p2k)
        f11 <- insert' (Follow p1k p1k)
        ret <- select $ do
                (follower :& follows :& followed) <-
                    from $ table @Person
                        `innerJoin` table @Follow
                        `on` (\(follower :& follows) ->
                            follower ^. PersonId ==. follows ^. FollowFollower)
                        `innerJoin` table @Person
                        `on` (\(_ :& follows :& followed) ->
                            followed ^. PersonId  ==. follows ^. FollowFollowed)
                orderBy [ asc (follower ^. PersonName)
                        , asc (followed ^. PersonName) ]
                return (follower, follows, followed)
        asserting $ ret `shouldBe` [ (p1e, f11, p1e)
                                , (p1e, f12, p2e)
                                , (p4e, f42, p2e)
                                , (p2e, f21, p1e) ]

    itDb "works for a many-to-many explicit join and on order doesn't matter" $ do
      void $
        selectRethrowingQuery $ do
            (person :& _blog :& comment) <-
                from $ table @Person
                `innerJoin` table @BlogPost
                `on` (\(person :& blog) ->
                   person ^. PersonId ==. blog ^. BlogPostAuthorId)
                `innerJoin` table @Comment
                `on` (\(_ :& blog :& comment) ->
                    blog ^. BlogPostId ==. comment ^. CommentBlog)
            pure (person, comment)

      -- we only care that we don't have a SQL error
      asserting noExceptions

    itDb "works for a many-to-many explicit join with LEFT OUTER JOINs" $ do
        p1e@(Entity p1k _) <- insert' p1
        p2e@(Entity p2k _) <- insert' p2
        p3e                <- insert' p3
        p4e@(Entity p4k _) <- insert' p4
        f12 <- insert' (Follow p1k p2k)
        f21 <- insert' (Follow p2k p1k)
        f42 <- insert' (Follow p4k p2k)
        f11 <- insert' (Follow p1k p1k)
        ret <- select $ do
                (follower :& mfollows :& mfollowed) <-
                    from $ table @Person
                        `leftJoin` table @Follow
                        `on` (\(follower :& mfollows) ->
                            just (follower ^. PersonId) ==. mfollows ?. FollowFollower)
                        `leftJoin` table @Person
                        `on` (\(_ :& mfollows :& mfollowed) ->
                            mfollowed ?. PersonId  ==. mfollows ?. FollowFollowed)
                orderBy [ asc ( follower ^. PersonName)
                        , asc (mfollowed ?. PersonName) ]
                return (follower, mfollows, mfollowed)
        asserting $ ret `shouldBe` [ (p1e, Just f11, Just p1e)
                                , (p1e, Just f12, Just p2e)
                                , (p4e, Just f42, Just p2e)
                                , (p3e, Nothing,  Nothing)
                                , (p2e, Just f21, Just p1e) ]

    itDb "works with a composite primary key" $ do
        let p = Point x y ""
            x = 10
            y = 15
            Right thePk = keyFromValues [toPersistValue x, toPersistValue y]
        pPk <- insert p
        [Entity _ ret] <- select $ do
                p' <- from $ table @Point
                where_ (p'^.PointId ==. val pPk)
                return p'
        asserting $ do
          ret `shouldBe` p
          pPk `shouldBe` thePk

testSelectOrderBy :: SpecDb
testSelectOrderBy = describe "select/orderBy" $ do
    itDb "works with a single ASC field" $ do
        p1e <- insert' p1
        p2e <- insert' p2
        p3e <- insert' p3
        ret <- select $ do
                p <- from $ table @Person
                orderBy [asc $ p ^. PersonName]
                return p
        asserting $ ret `shouldBe` [ p1e, p3e, p2e ]

    itDb "works with a sub_select" $ do
        [p1k, p2k, p3k, p4k] <- mapM insert [p1, p2, p3, p4]
        [b1k, b2k, b3k, b4k] <- mapM (insert . BlogPost "") [p1k, p2k, p3k, p4k]
        ret <- select $ do
               b <- from $ table @BlogPost
               orderBy [desc $ sub_select $ do
                               p <- from $ table @Person
                               where_ (p ^. PersonId ==. b ^. BlogPostAuthorId)
                               return (p ^. PersonName)
                       ]
               return (b ^. BlogPostId)
        asserting $ ret `shouldBe` (Value <$> [b2k, b3k, b4k, b1k])

    itDb "works on a composite primary key" $ do
        let ps = [Point 2 1 "", Point 1 2 ""]
        mapM_ insert ps
        eps <- select $ do
                p' <- from $ table @Point
                orderBy [asc (p'^.PointId)]
                return p'
        asserting $ map entityVal eps `shouldBe` reverse ps

testAscRandom :: SqlExpr (Value Double) -> SpecDb
testAscRandom rand' = describe "random_" $
    itDb "asc random_ works" $ do
        _p1e <- insert' p1
        _p2e <- insert' p2
        _p3e <- insert' p3
        _p4e <- insert' p4
        rets <-
          fmap S.fromList $
          replicateM 11 $
          select $ do
            p <- from $ table @Person
            orderBy [asc (rand' :: SqlExpr (Value Double))]
            return (p ^. PersonId :: SqlExpr (Value PersonId))
        -- There are 2^4 = 16 possible orderings.  The chance
        -- of 11 random samplings returning the same ordering
        -- is 1/2^40, so this test should pass almost everytime.
        asserting $ S.size rets `shouldSatisfy` (>2)

testSelectDistinct :: SpecDb
testSelectDistinct = do
  describe "SELECT DISTINCT" $ do
    let selDistTest
          ::
          ( SqlQuery (SqlExpr (Value String))
              -> SqlPersistT IO [Value String]
              )
          -> SqlPersistT IO ()
        selDistTest q = do
          p1k <- insert p1
          let (t1, t2, t3) = ("a", "b", "c")
          mapM_ (insert . flip BlogPost p1k) [t1, t3, t2, t2, t1]
          ret <- q $ do
                 b <- from $ table @BlogPost
                 let title = b ^. BlogPostTitle
                 orderBy [asc title]
                 return title
          asserting $ ret `shouldBe` [ Value t1, Value t2, Value t3 ]

    itDb "works on a simple example (select . distinct)" $
      selDistTest (\a -> select $ distinct a)

    itDb "works on a simple example (distinct (return ()))" $
      selDistTest (\act -> select $ distinct (return ()) >> act)



testCoasleceDefault :: SpecDb
testCoasleceDefault = describe "coalesce/coalesceDefault" $ do
    itDb "works on a simple example" $ do
        mapM_ insert' [p1, p2, p3, p4, p5]
        ret1 <- select $ do
                p <- from $ table @Person
                orderBy [asc (p ^. PersonId)]
                return (coalesce [p ^. PersonAge, p ^. PersonWeight])
        asserting $ ret1 `shouldBe` [ Value (Just (36 :: Int))
                                 , Value (Just 37)
                                 , Value (Just 17)
                                 , Value (Just 17)
                                 , Value Nothing
                                 ]

        ret2 <- select $ do
                p <- from $ table @Person
                orderBy [asc (p ^. PersonId)]
                return (coalesceDefault [p ^. PersonAge, p ^. PersonWeight] (p ^. PersonFavNum))
        asserting $ ret2 `shouldBe` [ Value (36 :: Int)
                                 , Value 37
                                 , Value 17
                                 , Value 17
                                 , Value 5
                                 ]

    itDb "works with sub-queries" $ do
        p1id <- insert p1
        p2id <- insert p2
        p3id <- insert p3
        _    <- insert p4
        _    <- insert p5
        _ <- insert $ BlogPost "a" p1id
        _ <- insert $ BlogPost "b" p2id
        _ <- insert $ BlogPost "c" p3id
        ret <- select $ do
               b <- from $ table @BlogPost
               let sub = do
                            p <- from $ table @Person
                            where_ (p ^. PersonId ==. b ^. BlogPostAuthorId)
                            return $ p ^. PersonAge
               return $ coalesceDefault [subSelectMaybe sub] (val (42 :: Int))
        asserting $ ret `shouldBe` [ Value (36 :: Int)
                                , Value 42
                                , Value 17
                                ]


testDelete :: SpecDb
testDelete = describe "delete" $ do
    itDb "works on a simple example" $ do
        p1e <- insert' p1
        p2e <- insert' p2
        p3e <- insert' p3
        let getAll = select $ do
                        p <- from $ table @Person
                        orderBy [asc (p ^. PersonName)]
                        return p
        ret1 <- getAll
        asserting $ ret1 `shouldBe` [ p1e, p3e, p2e ]
        delete $ do
            p <- from $ table @Person
            where_ (p ^. PersonName ==. val (personName p1))
        ret2 <- getAll
        asserting $ ret2 `shouldBe` [ p3e, p2e ]
        n    <- deleteCount $ void $ from $ table @Person
        ret3 <- getAll
        asserting $ (n, ret3) `shouldBe` (2, [])

testUpdate :: SpecDb
testUpdate = describe "update" $ do
    itDb "works with a subexpression having COUNT(*)" $ do
        p1k <- insert p1
        p2k <- insert p2
        p3k <- insert p3
        replicateM_ 3 (insert $ BlogPost "" p1k)
        replicateM_ 7 (insert $ BlogPost "" p3k)
        let blogPostsBy p = do
                b <- from $ table @BlogPost
                where_ (b ^. BlogPostAuthorId ==. p ^. PersonId)
        ()  <- update $ \p -> do
               set p [ PersonAge =. just (subSelectCount (blogPostsBy p)) ]
        ret <- select $ do
                p <- from $ table @Person
                orderBy [ asc (p ^. PersonName) ]
                return p
        asserting $ ret `shouldBe` [ Entity p1k p1 { personAge = Just 3 }
                                , Entity p3k p3 { personAge = Just 7 }
                                , Entity p2k p2 { personAge = Just 0 } ]

    it "works with a composite primary key" $ \_ ->
        pendingWith "Need refactor to support composite pks on ESet"
      {-
      do
        let p = Point x y ""
            x = 10
            y = 15
            newX = 20
            newY = 25
            Right newPk = keyFromValues [toPersistValue newX, toPersistValue newY]
        insert_ p
        () <- update $ \p' -> do
              set p' [PointId =. val newPk]
        [Entity _ ret] <- select $ from $ table @Point
        asserting $ do
          ret `shouldBe` Point newX newY []
      -}

    itDb "GROUP BY works with COUNT" $ do
        p1k <- insert p1
        p2k <- insert p2
        p3k <- insert p3
        replicateM_ 3 (insert $ BlogPost "" p1k)
        replicateM_ 7 (insert $ BlogPost "" p3k)
        ret <- select $ do
                (p :& b) <-
                    from $ table @Person
                        `leftJoin` table @BlogPost
                        `on` (\(p :& b) ->
                            just (p ^. PersonId) ==. b ?. BlogPostAuthorId)
                groupBy (p ^. PersonId)
                let cnt = count (b ?. BlogPostId)
                orderBy [ asc cnt ]
                return (p, cnt)
        asserting $ ret `shouldBe` [ (Entity p2k p2, Value (0 :: Int))
                                , (Entity p1k p1, Value 3)
                                , (Entity p3k p3, Value 7) ]

    itDb "GROUP BY works with composite primary key" $ do
        p1k <- insert $ Point 1 2 "asdf"
        p2k <- insert $ Point 2 3 "asdf"
        ret <-
            selectRethrowingQuery $ do
            point <- from $ table @Point
            where_ $ point ^. PointName ==. val "asdf"
            groupBy (point ^. PointId)
            pure (point ^. PointId)
        asserting $ do
            ret `shouldMatchList`
                map Value [p1k, p2k]



    itDb "GROUP BY works with COUNT and InnerJoin" $ do
        l1k <- insert l1
        l3k <- insert l3
        mapM_ (\k -> insert $ Deed k l1k) (map show [1..3 :: Int])

        mapM_ (\k -> insert $ Deed k l3k) (map show [4..10 :: Int])

        ret <- select $ do
                  (lord :& deed) <-
                      from $ table @Lord
                            `innerJoin` table @Deed
                            `on` (\(lord :& deed) ->
                                lord ^. LordId ==. deed ^. DeedOwnerId)
                  groupBy (lord ^. LordId)
                  return (lord ^. LordId, count @Int $ deed ^. DeedId)
        asserting $ ret `shouldMatchList` [ (Value l3k, Value 7)
                                          , (Value l1k, Value 3) ]

    itDb "GROUP BY works with nested tuples" $ do
        l1k <- insert l1
        l3k <- insert l3
        mapM_ (\k -> insert $ Deed k l1k) (map show [1..3 :: Int])

        mapM_ (\k -> insert $ Deed k l3k) (map show [4..10 :: Int])

        ret <- select $ do
                  (lord :& deed) <-
                      from $ table @Lord
                            `innerJoin` table @Deed
                            `on` (\(lord :& deed) ->
                                lord ^. LordId ==. deed ^. DeedOwnerId)
                  groupBy ((lord ^. LordId, lord ^. LordDogs), deed ^. DeedContract)
                  return (lord ^. LordId, count @Int $ deed ^. DeedId)
        asserting $ length ret `shouldBe` 10

    itDb "GROUP BY works with HAVING" $ do
        p1k <- insert p1
        _p2k <- insert p2
        p3k <- insert p3
        replicateM_ 3 (insert $ BlogPost "" p1k)
        replicateM_ 7 (insert $ BlogPost "" p3k)
        ret <- select $ do
                (p :& b) <-
                    from $ table @Person
                        `leftJoin` table @BlogPost
                        `on` (\(p :& b) ->
                            just (p ^. PersonId) ==. b ?. BlogPostAuthorId)
                let cnt = count (b ?. BlogPostId)
                groupBy (p ^. PersonId)
                having (cnt >. (val 0))
                orderBy [ asc cnt ]
                return (p, cnt)
        asserting $ ret `shouldBe` [ (Entity p1k p1, Value (3 :: Int))
                                , (Entity p3k p3, Value 7) ]

-- we only care that this compiles. check that SqlWriteT doesn't fail on
-- updates.
testSqlWriteT :: MonadIO m => SqlWriteT m ()
testSqlWriteT =
  update $ \p -> do
    set p [ PersonAge =. just (val 6) ]

-- we only care that this compiles. checks that the SqlWriteT monad can run
-- select queries.
testSqlWriteTRead :: MonadIO m => SqlWriteT m [(Value (Key Lord), Value Int)]
testSqlWriteTRead =
  select $ do
      (lord :& deed) <-
          from $ table @Lord
                `innerJoin` table @Deed
                `on` (\(lord :& deed) ->
                    lord ^. LordId ==. deed ^. DeedOwnerId)
      groupBy (lord ^. LordId)
      return (lord ^. LordId, count $ deed ^. DeedId)

-- we only care that this compiles checks that SqlReadT allows
testSqlReadT :: MonadIO m => SqlReadT m [(Value (Key Lord), Value Int)]
testSqlReadT =
  select $ do
      (lord :& deed) <-
          from $ table @Lord
                `innerJoin` table @Deed
                `on` (\(lord :& deed) ->
                    lord ^. LordId ==. deed ^. DeedOwnerId)
      groupBy (lord ^. LordId)
      return (lord ^. LordId, count $ deed ^. DeedId)

testListOfValues :: SpecDb
testListOfValues = describe "lists of values" $ do
    itDb "IN works for valList" $ do
        p1k <- insert p1
        p2k <- insert p2
        _p3k <- insert p3
        ret <- select $ do
               p <- from $ table @Person
               where_ (p ^. PersonName `in_` valList (personName <$> [p1, p2]))
               return p
        asserting $ ret `shouldBe` [ Entity p1k p1
                                , Entity p2k p2 ]

    itDb "IN works for valList (null list)" $ do
        _p1k <- insert p1
        _p2k <- insert p2
        _p3k <- insert p3
        ret <- select $ do
                p <- from $ table @Person
                where_ (p ^. PersonName `in_` valList [])
                return p
        asserting $ ret `shouldBe` []

    itDb "IN works for subList_select" $ do
        p1k <- insert p1
        _p2k <- insert p2
        p3k <- insert p3
        _ <- insert (BlogPost "" p1k)
        _ <- insert (BlogPost "" p3k)
        ret <- select $ do
               p <- from $ table @Person
               let subquery = do
                     bp <- from $ table @BlogPost
                     orderBy [ asc (bp ^. BlogPostAuthorId) ]
                     return (bp ^. BlogPostAuthorId)
               where_ (p ^. PersonId `in_` subList_select subquery)
               return p
        asserting $ L.sort ret `shouldBe` L.sort [Entity p1k p1, Entity p3k p3]

    itDb "NOT IN works for subList_select" $ do
        p1k <- insert p1
        p2k <- insert p2
        p3k <- insert p3
        _ <- insert (BlogPost "" p1k)
        _ <- insert (BlogPost "" p3k)
        ret <- select $ do
                p <- from $ table @Person
                let subquery = do
                     bp <- from $ table @BlogPost
                     return (bp ^. BlogPostAuthorId)
                where_ (p ^. PersonId `notIn` subSelectList subquery)
                return p
        asserting $ ret `shouldBe` [ Entity p2k p2 ]

    itDb "NOT IN works for valList (null list)" $ do
        p1k <- insert p1
        p2k <- insert p2
        p3k <- insert p3
        ret <- select $ do
                p <- from $ table @Person
                where_ (p ^. PersonName `notIn` valList [])
                return p
        asserting $ ret `shouldMatchList` [ Entity p1k p1
                                          , Entity p2k p2
                                          , Entity p3k p3
                                          ]

    itDb "EXISTS works for subList_select" $ do
        p1k <- insert p1
        _p2k <- insert p2
        p3k <- insert p3
        _ <- insert (BlogPost "" p1k)
        _ <- insert (BlogPost "" p3k)
        ret <- select $ do
                p <- from $ table @Person
                where_ $ exists $ do
                    bp <- from $ table @BlogPost
                    where_ (bp ^. BlogPostAuthorId ==. p ^. PersonId)
                orderBy [asc (p ^. PersonName)]
                return p
        asserting $ ret `shouldBe` [ Entity p1k p1
                                , Entity p3k p3 ]

    itDb "EXISTS works for subList_select" $ do
        p1k <- insert p1
        p2k <- insert p2
        p3k <- insert p3
        _ <- insert (BlogPost "" p1k)
        _ <- insert (BlogPost "" p3k)
        ret <- select $ do
                p <- from $ table @Person
                where_ $ notExists $ do
                        bp <- from $ table @BlogPost
                        where_ (bp ^. BlogPostAuthorId ==. p ^. PersonId)
                return p
        asserting $ ret `shouldBe` [ Entity p2k p2 ]

testListFields :: SpecDb
testListFields = describe "list fields" $ do
    -- <https://github.com/prowdsponsor/esqueleto/issues/100>
    itDb "can update list fields" $ do
        cclist <- insert $ CcList []
        update $ \p -> do
            set p [ CcListNames =. val ["fred"]]
            where_ (p ^. CcListId ==. val cclist)
        asserting noExceptions

testInsertsBySelect :: SpecDb
testInsertsBySelect = do
  describe "inserts by select" $ do
    itDb "IN works for insertSelect" $
      do
        _ <- insert p1
        _ <- insert p2
        _ <- insert p3
        insertSelect $ do
          p <- from $ table @Person
          return $ BlogPost <# val "FakePost" <&> (p ^. PersonId)
        ret <- select $ do
            _ <- from $ table @BlogPost
            return countRows
        asserting $ ret `shouldBe` [Value (3::Int)]





testInsertsBySelectReturnsCount :: SpecDb
testInsertsBySelectReturnsCount = do
  describe "inserts by select, returns count" $ do
    itDb "IN works for insertSelectCount" $
      do
        _ <- insert p1
        _ <- insert p2
        _ <- insert p3
        cnt <- insertSelectCount $ do
            p <- from $ table @Person
            return $ BlogPost <# val "FakePost" <&> (p ^. PersonId)
        ret <- select $ do
            _ <- from $ table @BlogPost
            return countRows
        asserting $ ret `shouldBe` [Value (3::Int)]
        asserting $ cnt `shouldBe` 3




testRandomMath :: SpecDb
testRandomMath = describe "random_ math" $
    itDb "rand returns result in random order" $
      do
        replicateM_ 20 $ do
          _ <- insert p1
          _ <- insert p2
          _ <- insert p3
          _ <- insert p4
          _ <- insert $ Person "Jane"  Nothing Nothing 0
          _ <- insert $ Person "Mark"  Nothing Nothing 0
          _ <- insert $ Person "Sarah" Nothing Nothing 0
          insert $ Person "Paul"  Nothing Nothing 0
        ret1 <- fmap (map unValue) $ select $ do
                  p <- from $ table @Person
                  orderBy [rand]
                  return (p ^. PersonId)
        ret2 <- fmap (map unValue) $ select $ do
                  p <- from $ table @Person
                  orderBy [rand]
                  return (p ^. PersonId)

        asserting $ (ret1 == ret2) `shouldBe` False

testMathFunctions :: SpecDb
testMathFunctions = do
  describe "Math-related functions" $ do
    itDb "castNum works for multiplying Int and Double" $
      do
        mapM_ insert [Numbers 2 3.4, Numbers 7 1.1]
        ret <-
          select $ do
              n <- from $ table @Numbers
              let r = castNum (n ^. NumbersInt) *. n ^. NumbersDouble
              orderBy [asc r]
              return r
        asserting $ length ret `shouldBe` 2
        let [Value a, Value b] = ret
        asserting $ max (abs (a - 6.8)) (abs (b - 7.7)) `shouldSatisfy` (< 0.01)

testCase :: SpecDb
testCase = do
  describe "case" $ do
    itDb "Works for a simple value based when - False" $
      do
        ret <- select $
          return $
            case_
              [ when_ (val False) then_ (val (1 :: Int)) ]
              (else_ (val 2))

        asserting $ ret `shouldBe` [ Value 2 ]

    itDb "Works for a simple value based when - True" $
      do
        ret <- select $
          return $
            case_
              [ when_ (val True) then_ (val (1 :: Int)) ]
              (else_ (val 2))

        asserting $ ret `shouldBe` [ Value 1 ]

    itDb "works for a semi-complicated query" $
      do
        _ <- insert p1
        _ <- insert p2
        _ <- insert p3
        _ <- insert p4
        _ <- insert p5
        ret <- select $
          return $
            case_
              [ when_
                  (exists $ do
                      p <- from $ table @Person
                      where_ (p ^. PersonName ==. val "Mike"))
                then_
                  (subSelect $ do
                      v <- from $ table @Person
                      let sub = do
                            c <- from $ table @Person
                            where_ (c ^. PersonName ==. val "Mike")
                            return (c ^. PersonFavNum)
                      where_ (just (v ^. PersonFavNum) >. subSelect sub)
                      return $ count (v ^. PersonName) +. val (1 :: Int)) ]
              (else_ $ just $ val (-1))

        asserting $ ret `shouldBe` [ Value (Just 3) ]

testLocking :: SpecDb
testLocking = do
  let toText conn q =
        let (tlb, _) = EI.toRawSql EI.SELECT (conn, EI.initialIdentState) q
         in TLB.toLazyText tlb
      complexQuery = do
          (p1' :& p2') <- from $ table @Person
                            `innerJoin` table @Person
                            `on` (\(p1' :& p2') ->
                                p1' ^. PersonName ==. p2' ^. PersonName)
          where_ (p1' ^. PersonFavNum >. val 2)
          orderBy [desc (p2' ^. PersonAge)]
          limit 3
          offset 9
          groupBy (p1' ^. PersonId)
          having (countRows <. val (0 :: Int))
          return (p1', p2')
  describe "locking" $ do
    -- The locking clause is the last one, so try to use many
    -- others to test if it's at the right position.  We don't
    -- care about the text of the rest, nor with the RDBMS'
    -- reaction to the clause.
    let sanityCheck kind syntax = do
          let queryWithClause1 = do
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
          conn <- ask
          [complex, with1, with2, with3] <-
            return $
              map (toText conn) [complexQuery, queryWithClause1, queryWithClause2, queryWithClause3]
          let expected = complex <> syntax <> "\n"
          asserting $ do
            with1 `shouldBe` expected
            with2 `shouldBe` expected
            with3 `shouldBe` expected
    itDb "looks sane for ForUpdate"           $ sanityCheck ForUpdate           "FOR UPDATE"
    itDb "looks sane for ForUpdateSkipLocked" $ sanityCheck ForUpdateSkipLocked "FOR UPDATE SKIP LOCKED"
    itDb "looks sane for ForShare"            $ sanityCheck ForShare            "FOR SHARE"
    itDb "looks sane for LockInShareMode"     $ sanityCheck LockInShareMode     "LOCK IN SHARE MODE"

  describe "Monoid instance" $ do
    let
        multiplePostgresLockingClauses p = do
            EP.forUpdateOf p EP.skipLocked
            EP.forUpdateOf p EP.skipLocked
            EP.forShareOf p EP.skipLocked

        multipleLegacyLockingClauses = do
            locking ForShare
            locking ForUpdate

        multipleLockingQueryPostgresLast = do
            p <- from $ table @Person
            multipleLegacyLockingClauses
            multiplePostgresLockingClauses p

        multipleLockingQueryLegacyLast = do
            p <- from $ table @Person
            multiplePostgresLockingClauses p
            multipleLegacyLockingClauses

        expectedPostgresQuery = do
            p <- from $ table @Person
            EP.forUpdateOf p EP.skipLocked
            EP.forUpdateOf p EP.skipLocked
            EP.forShareOf p EP.skipLocked

        expectedLegacyQuery = do
            _p <- from $ table @Person
            locking ForUpdate

    itDb "prioritizes last grouping of locks when mixing legacy and postgres specific locks" $ do

        conn <- ask
        let resPostgresLast = toText conn multipleLockingQueryPostgresLast
            resLegacyLast = toText conn multipleLockingQueryLegacyLast
            resExpectedPostgres = toText conn expectedPostgresQuery
            resExpectedLegacy = toText conn expectedLegacyQuery

        asserting $ resPostgresLast `shouldBe` resExpectedPostgres
        asserting $ resLegacyLast `shouldBe` resExpectedLegacy

testCountingRows :: SpecDb
testCountingRows = do
  describe "counting rows" $ do
    forM_ [ ("count (test A)",    count . (^. PersonAge),         4)
          , ("count (test B)",    count . (^. PersonWeight),      5)
          , ("countRows",         const countRows,                5)
          , ("countDistinct",     countDistinct . (^. PersonAge), 2) ] $
      \(title, countKind, expected) ->
      itDb (title ++ " works as expected") $
        do
          mapM_ insert
            [ Person "" (Just 1) (Just 1) 1
            , Person "" (Just 2) (Just 1) 1
            , Person "" (Just 2) (Just 1) 1
            , Person "" (Just 2) (Just 2) 1
            , Person "" Nothing  (Just 3) 1]
          [Value n] <- select $ do
              p <- from $ table @Person
              return $ countKind p
          asserting $ (n :: Int) `shouldBe` expected

testRenderSql :: SpecDb
testRenderSql = do
  describe "testRenderSql" $ do
    itDb "works" $ do
      (queryText, queryVals) <- renderQuerySelect $ do
          p <- from $ table @Person
          where_ $ p ^. PersonName ==. val "Johhny Depp"
          pure (p ^. PersonName, p ^. PersonAge)
      -- the different backends use different quote marks, so I filter them out
      -- here instead of making a duplicate test
      asserting $ do
          Text.filter (\c -> c `notElem` ['`', '"']) queryText
            `shouldBe`
              Text.unlines
                [ "SELECT Person.name, Person.age"
                , "FROM Person"
                , "WHERE Person.name = ?"
                ]
          queryVals
            `shouldBe`
              [toPersistValue ("Johhny Depp" :: TL.Text)]

  describe "renderExpr" $ do
    itDb "renders a value" $ do
      (c, expr) <- do
        conn <- ask
        let Right c = P.mkEscapeChar conn
        let user = EI.unsafeSqlEntity (EI.I "user")
            blogPost = EI.unsafeSqlEntity (EI.I "blog_post")
        pure $ (,) c $ EI.renderExpr conn $
          user ^. PersonId ==. blogPost ^. BlogPostAuthorId
      asserting $ do
          expr
            `shouldBe`
              Text.intercalate (Text.singleton c) ["", "user", ".", "id", ""]
              <>
              " = "
              <>
              Text.intercalate (Text.singleton c) ["", "blog_post", ".", "authorId", ""]

    itDb "renders ? for a val" $ do
      expr <- ask >>= \c -> pure $ EI.renderExpr c (val (PersonKey 0) ==. val (PersonKey 1))
      asserting $ expr `shouldBe` "? = ?"



listsEqualOn :: (HasCallStack, Show a1, Eq a1) => [a2] -> [a2] -> (a2 -> a1) -> Expectation
listsEqualOn a b f = map f a `shouldBe` map f b

tests :: SpecDb
tests =
    describe "Esqueleto" $ do
        testSelect
        testGetTable
        testSubSelect
        testSelectOne
        testSelectSource
        testSelectFrom
        testSelectJoin
        testSelectSubQuery
        testSelectWhere
        testSelectOrderBy
        testSelectDistinct
        testCoasleceDefault
        testDelete
        testUpdate
        testListOfValues
        testListFields
        testInsertsBySelect
        testMathFunctions
        testCase
        testCountingRows
        testRenderSql
        testLocking
        testOverloadedRecordDot
        testDeriveEsqueletoRecord

insert' :: ( Functor m
           , BaseBackend backend ~ PersistEntityBackend val
           , PersistStore backend
           , MonadIO m
#if MIN_VERSION_persistent(2,14,0)
           , SafeToInsert val
#endif
           , PersistEntity val )
        => val -> ReaderT backend m (Entity val)
insert' v = flip Entity v <$> insert v


-- With SQLite and in-memory databases, a separate connection implies a
-- separate database. With 'actual databases', the data is persistent and
-- thus must be cleaned after each test.
-- TODO: there is certainly a better way...
cleanDB :: MonadIO m => SqlPersistT m ()
cleanDB = do
  delete $ void $ from $ table @Bar
  delete $ void $ from $ table @Foo

  delete $ void $ from $ table @Reply
  delete $ void $ from $ table @Comment
  delete $ void $ from $ table @Profile
  delete $ void $ from $ table @BlogPost
  delete $ void $ from $ table @Follow
  delete $ void $ from $ table @Person

  delete $ void $ from $ table @Deed
  delete $ void $ from $ table @Lord

  delete $ void $ from $ table @CcList

  delete $ void $ from $ table @ArticleTag
  delete $ void $ from $ table @ArticleMetadata
  delete $ void $ from $ table @Article
  delete $ void $ from $ table @Article2
  delete $ void $ from $ table @Tag
  delete $ void $ from $ table @Frontcover

  delete $ void $ from $ table @Circle
  delete $ void $ from $ table @Point

  delete $ void $ from $ table @Numbers
  delete $ void $ from $ table @JoinMany
  delete $ void $ from $ table @JoinFour
  delete $ void $ from $ table @JoinThree
  delete $ void $ from $ table @JoinTwo
  delete $ void $ from $ table @JoinOne
  delete $ void $ from $ table @JoinOther

  delete $ void $ from $ table @DateTruncTest


cleanUniques
    :: forall m. MonadIO m
    => SqlPersistT m ()
cleanUniques =
    delete $ void $ from $ table @OneUnique

selectRethrowingQuery
  :: (MonadIO m, EI.SqlSelect a r, MonadUnliftIO m)
  => SqlQuery a
  -> SqlPersistT m [r]
selectRethrowingQuery query =
  select query
    `catch` \(SomeException e) -> do
      (text, _) <- renderQuerySelect query
      liftIO . throwIO . userError $ Text.unpack text <> "\n\n" <> show e

updateRethrowingQuery
    ::
    ( MonadUnliftIO  m
    , PersistEntity val
    , BackendCompatible SqlBackend (PersistEntityBackend val)
    )
    => (SqlExpr (Entity val) -> SqlQuery ())
    -> SqlWriteT m ()
updateRethrowingQuery k =
    update k
        `catch` \(SomeException e) -> do
            (text, _) <- renderQueryUpdate (from table >>= k)
            liftIO . throwIO . userError $ Text.unpack text <> "\n\n" <> show e

shouldBeOnClauseWithoutMatchingJoinException
    :: (HasCallStack, Show a)
    => Either SomeException a
    -> Expectation
shouldBeOnClauseWithoutMatchingJoinException ea =
    case ea of
        Left (fromException -> Just OnClauseWithoutMatchingJoinException {}) ->
            pure ()
        _ ->
            expectationFailure $ "Expected OnClauseWithMatchingJoinException, got: " <> show ea

testOverloadedRecordDot :: SpecDb
testOverloadedRecordDot = describe "OverloadedRecordDot" $ do
#if __GLASGOW_HASKELL__ >= 902
    describe "with SqlExpr (Entity rec)" $ do
        itDb "lets you project from a record" $ do
            select $ do
                bp <- from $ table @BlogPost
                pure bp.title
    describe "with SqlExpr (Maybe (Entity rec))" $ do
        itDb "lets you project from a Maybe record" $ do
            select $ do
                p :& mbp <- from $
                    table @Person
                    `leftJoin` table @BlogPost
                        `on` do
                            \(p :& mbp) ->
                                just p.id ==. mbp.authorId
                pure (p.id, mbp.title)

#else
    it "is only supported in GHC 9.2 or above" $ \_ -> do
        pending
#endif

testGetTable :: SpecDb
testGetTable =
    describe "GetFirstTable" $ do
        itDb "works to make long join chains easier" $ do
            void $ select $ do
                (person :& blogPost :& profile :& reply) <-
                    from $
                        table @Person
                        `leftJoin` table @BlogPost
                            `on` do
                                \(p :& bp) ->
                                    just (p ^. PersonId) ==. bp ?. BlogPostAuthorId
                        `leftJoin` table @Profile
                            `on` do
                                \((getTable @Person -> p) :& profile) ->
                                    just (p ^. PersonId) ==. profile ?. ProfilePerson
                        `leftJoin` table @Reply
                            `on` do
                                \((getTable @Person -> p) :& reply) ->
                                    just (p ^. PersonId) ==. reply ?. ReplyGuy
                pure (person, blogPost, profile, reply)
            asserting noExceptions

