{-# OPTIONS_GHC -fno-warn-unused-binds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module PostgreSQL.Test where

import Control.Arrow ((&&&))
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger (runNoLoggingT, runStderrLoggingT)
import Control.Monad.Trans.Reader (ReaderT, ask, mapReaderT)
import qualified Control.Monad.Trans.Resource as R
import Data.Aeson hiding (Value)
import qualified Data.Aeson as A (Value)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Char as Char
import Data.Coerce
import Data.Foldable
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Database.Esqueleto hiding (random_)
import Database.Esqueleto.Experimental hiding (from, on, random_)
import qualified Database.Esqueleto.Experimental as Experimental
import qualified Database.Esqueleto.Internal.Internal as ES
import Database.Esqueleto.PostgreSQL (random_)
import qualified Database.Esqueleto.PostgreSQL as EP
import Database.Esqueleto.PostgreSQL.JSON hiding ((-.), (?.), (||.))
import qualified Database.Esqueleto.PostgreSQL.JSON as JSON
import qualified Database.Esqueleto.PostgreSQL.WindowFunction as Window
import Database.Persist.Postgresql (createPostgresqlPool, withPostgresqlConn)
import Database.PostgreSQL.Simple (ExecStatus(..), SqlError(..))
import System.Environment
import Test.Hspec
import Test.Hspec.QuickCheck

import Common.Test
import Common.Test.Import hiding (from, on)
import PostgreSQL.MigrateJSON

returningType :: forall a m . m a -> m a
returningType a = a

testPostgresqlCoalesce :: SpecDb
testPostgresqlCoalesce = do
    itDb "works on PostgreSQL and MySQL with <2 arguments" $ do
        void $ returningType @[Value (Maybe Int)] $
            select $
            from $ \p -> do
            return (coalesce [p ^. PersonAge])
        asserting noExceptions

testPostgresqlTextFunctions :: SpecDb
testPostgresqlTextFunctions = do
    describe "text functions" $ do
        itDb "like, (%) and (++.) work on a simple example" $ do
            let nameContains t =
                    select $
                    from $ \p -> do
                    where_
                        (like
                        (p ^. PersonName)
                        ((%) ++. val t ++. (%)))
                    orderBy [asc (p ^. PersonName)]
                    return p
            [p1e, p2e, p3e, p4e] <- mapM insert' [p1, p2, p3, p4]
            h <- nameContains "h"
            i <- nameContains "i"
            iv <- nameContains "iv"
            asserting $ do
                h `shouldBe` [p1e, p2e]
                i `shouldBe` [p4e, p3e]
                iv `shouldBe` [p4e]

        itDb "ilike, (%) and (++.) work on a simple example on PostgreSQL" $ do
            [p1e, _, p3e, _, p5e] <- mapM insert' [p1, p2, p3, p4, p5]
            let nameContains t = do
                    select $
                     from $ \p -> do
                     where_ (p ^. PersonName `ilike` (%) ++. val t ++. (%))
                     orderBy [asc (p ^. PersonName)]
                     return p
            mi <- nameContains "mi"
            john <- nameContains "JOHN"
            asserting $ do
                mi `shouldBe` [p3e, p5e]
                john `shouldBe` [p1e]

testPostgresqlUpdate :: SpecDb
testPostgresqlUpdate = do
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
        -- PostgreSQL: nulls are bigger than data, and update returns
        --             matched rows, not actually changed rows.
        asserting $ do
            n `shouldBe` 2
            ret `shouldBe`
                [ Entity p1k (Person anon (Just 73) Nothing 1)
                , Entity p2k (Person anon Nothing (Just 37) 2)
                , Entity p3k p3
                ]

testPostgresqlRandom :: SpecDb
testPostgresqlRandom = do
    itDb "works with random_" $ do
        _ <- select $ return (random_ :: SqlExpr (Value Double))
        asserting noExceptions

testPostgresqlSum :: SpecDb
testPostgresqlSum = do
    itDb "works with sum_" $ do
        _ <- insert' p1
        _ <- insert' p2
        _ <- insert' p3
        _ <- insert' p4
        ret <- select $
               from $ \p->
               return $ joinV $ sum_ (p ^. PersonAge)
        asserting $ ret `shouldBe` [ Value $ Just (36 + 17 + 17 :: Rational ) ]

testPostgresqlTwoAscFields :: SpecDb
testPostgresqlTwoAscFields = do
    itDb "works with two ASC fields (one call)" $ do
        p1e <- insert' p1
        p2e <- insert' p2
        p3e <- insert' p3
        p4e <- insert' p4
        ret <- select $
               from $ \p -> do
               orderBy [asc (p ^. PersonAge), asc (p ^. PersonName)]
               return p
        -- in PostgreSQL nulls are bigger than everything
        asserting $ ret `shouldBe` [ p4e, p3e, p1e , p2e ]

testPostgresqlOneAscOneDesc :: SpecDb
testPostgresqlOneAscOneDesc = do
  itDb "works with one ASC and one DESC field (two calls)" $
     do
      p1e <- insert' p1
      p2e <- insert' p2
      p3e <- insert' p3
      p4e <- insert' p4
      ret <- select $
             from $ \p -> do
             orderBy [desc (p ^. PersonAge)]
             orderBy [asc (p ^. PersonName)]
             return p
      asserting $ ret `shouldBe` [ p2e, p1e, p4e, p3e ]

testSelectDistinctOn :: SpecDb
testSelectDistinctOn = do
  describe "SELECT DISTINCT ON" $ do
    itDb "works on a simple example" $ do
       do
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
           do
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

    itDb "works on a slightly less simple example (two distinctOn calls, orderBy)" $
      slightlyLessSimpleTest $ \bp act ->
        distinctOn [don (bp ^. BlogPostAuthorId)] $
        distinctOn [don (bp ^. BlogPostTitle)] $ do
          orderBy [asc (bp ^. BlogPostAuthorId), asc (bp ^. BlogPostTitle)]
          act

    itDb "works on a slightly less simple example (one distinctOn call, orderBy)" $ do
      slightlyLessSimpleTest $ \bp act ->
        distinctOn [don (bp ^. BlogPostAuthorId), don (bp ^. BlogPostTitle)] $ do
          orderBy [asc (bp ^. BlogPostAuthorId), asc (bp ^. BlogPostTitle)]
          act

    itDb "works on a slightly less simple example (distinctOnOrderBy)" $ do
      slightlyLessSimpleTest $ \bp ->
        distinctOnOrderBy [asc (bp ^. BlogPostAuthorId), asc (bp ^. BlogPostTitle)]

    itDb "generates correct sql with nested expression (distinctOnOrderBy)" $ do
      let query = do
            let orderVal = coalesce [nothing, just $ val (10 :: Int)]
            distinctOnOrderBy [ asc orderVal, desc orderVal ] $ pure orderVal
      select query
      asserting noExceptions




testArrayAggWith :: SpecDb
testArrayAggWith = do
  describe "ALL, no ORDER BY" $ do
    itDb "creates sane SQL" $  do
      (query, args) <- showQuery ES.SELECT $ from $ \p ->
            return (EP.arrayAggWith EP.AggModeAll (p ^. PersonAge) [])
      liftIO $ query `shouldBe`
        "SELECT array_agg(\"Person\".\"age\")\n\
        \FROM \"Person\"\n"
      liftIO $ args `shouldBe` []

    itDb "works on an example" $  do
      let people = [p1, p2, p3, p4, p5]
      mapM_ insert people
      [Value (Just ret)] <-
        select $ from $ \p ->
          return (EP.arrayAggWith EP.AggModeAll (p ^. PersonName) [])
      liftIO $ L.sort ret `shouldBe` L.sort (map personName people)

  describe "DISTINCT, no ORDER BY" $ do
    itDb "creates sane SQL" $  do
      (query, args) <- showQuery ES.SELECT $ from $ \p ->
            return (EP.arrayAggWith EP.AggModeDistinct (p ^. PersonAge) [])
      liftIO $ query `shouldBe`
        "SELECT array_agg(DISTINCT \"Person\".\"age\")\n\
        \FROM \"Person\"\n"
      liftIO $ args `shouldBe` []

    itDb "works on an example" $  do
      let people = [p1, p2, p3, p4, p5]
      mapM_ insert people
      [Value (Just ret)] <-
        select $ from $ \p ->
          return (EP.arrayAggWith EP.AggModeDistinct (p ^. PersonAge) [])
      liftIO $ L.sort ret `shouldBe` [Nothing, Just 17, Just 36]

  describe "ALL, ORDER BY" $ do
    itDb "creates sane SQL" $  do
      (query, args) <- showQuery ES.SELECT $ from $ \p ->
            return (EP.arrayAggWith EP.AggModeAll (p ^. PersonAge)
                    [ asc $ p ^. PersonName
                    , desc $ p ^. PersonFavNum
                    ])
      liftIO $ query `shouldBe`
        "SELECT array_agg(\"Person\".\"age\" \
          \ORDER BY \"Person\".\"name\" ASC, \"Person\".\"favNum\" DESC)\n\
        \FROM \"Person\"\n"
      liftIO $ args `shouldBe` []

    itDb "works on an example" $  do
      let people = [p1, p2, p3, p4, p5]
      mapM_ insert people
      [Value (Just ret)] <-
        select $ from $ \p ->
          return (EP.arrayAggWith EP.AggModeAll (p ^. PersonName) [])
      liftIO $ L.sort ret `shouldBe` L.sort (map personName people)

  describe "DISTINCT, ORDER BY" $ do
    itDb "creates sane SQL" $  do
      (query, args) <- showQuery ES.SELECT $ from $ \p ->
          return (EP.arrayAggWith EP.AggModeDistinct (p ^. PersonAge)
                   [asc $ p ^. PersonAge])
      liftIO $ query `shouldBe`
        "SELECT array_agg(DISTINCT \"Person\".\"age\" \
          \ORDER BY \"Person\".\"age\" ASC)\n\
        \FROM \"Person\"\n"
      liftIO $ args `shouldBe` []

    itDb "works on an example" $  do
      let people = [p1, p2, p3, p4, p5]
      mapM_ insert people
      [Value (Just ret)] <-
        select $ from $ \p ->
          return (EP.arrayAggWith EP.AggModeDistinct (p ^. PersonAge)
                   [asc $ p ^. PersonAge])
      liftIO $ ret `shouldBe` [Just 17, Just 36, Nothing]





testStringAggWith :: SpecDb
testStringAggWith = do
  describe "ALL, no ORDER BY" $ do
    itDb "creates sane SQL" $  do
      (query, args) <- showQuery ES.SELECT $ from $ \p ->
            return (EP.stringAggWith EP.AggModeAll (p ^. PersonName)
                     (val " ") [])
      liftIO $ query `shouldBe`
        "SELECT string_agg(\"Person\".\"name\", ?)\n\
        \FROM \"Person\"\n"
      liftIO $ args `shouldBe` [PersistText " "]

    itDb "works on an example" $  do
      let people = [p1, p2, p3, p4, p5]
      mapM_ insert people
      [Value (Just ret)] <-
        select $ from $ \p ->
          return (EP.stringAggWith EP.AggModeAll (p ^. PersonName) (val " ")[])
      liftIO $ (L.sort $ words ret) `shouldBe` L.sort (map personName people)

    itDb "works with zero rows" $  do
      [Value ret] <-
        select $ from $ \p ->
          return (EP.stringAggWith EP.AggModeAll (p ^. PersonName) (val " ")[])
      liftIO $ ret `shouldBe` Nothing

  describe "DISTINCT, no ORDER BY" $ do
    itDb "creates sane SQL" $  do
      (query, args) <- showQuery ES.SELECT $ from $ \p ->
            return $ EP.stringAggWith EP.AggModeDistinct (p ^. PersonName)
                     (val " ") []
      liftIO $ query `shouldBe`
        "SELECT string_agg(DISTINCT \"Person\".\"name\", ?)\n\
        \FROM \"Person\"\n"
      liftIO $ args `shouldBe` [PersistText " "]

    itDb "works on an example" $  do
      let people = [p1, p2, p3 {personName = "John"}, p4, p5]
      mapM_ insert people
      [Value (Just ret)] <-
        select $ from $ \p ->
          return $ EP.stringAggWith EP.AggModeDistinct (p ^. PersonName) (val " ")
                   []
      liftIO $ (L.sort $ words ret) `shouldBe`
        (L.sort . L.nub $ map personName people)

  describe "ALL, ORDER BY" $ do
    itDb "creates sane SQL" $  do
      (query, args) <- showQuery ES.SELECT $ from $ \p ->
            return (EP.stringAggWith EP.AggModeAll (p ^. PersonName) (val " ")
                    [ asc $ p ^. PersonName
                    , desc $ p ^. PersonFavNum
                    ])
      liftIO $ query `shouldBe`
        "SELECT string_agg(\"Person\".\"name\", ? \
          \ORDER BY \"Person\".\"name\" ASC, \"Person\".\"favNum\" DESC)\n\
        \FROM \"Person\"\n"
      liftIO $ args `shouldBe` [PersistText " "]

    itDb "works on an example" $  do
      let people = [p1, p2, p3, p4, p5]
      mapM_ insert people
      [Value (Just ret)] <-
        select $ from $ \p ->
          return $ EP.stringAggWith EP.AggModeAll (p ^. PersonName) (val " ")
                    [desc $ p ^. PersonName]
      liftIO $ (words ret)
        `shouldBe` (L.reverse . L.sort $ map personName people)

  describe "DISTINCT, ORDER BY" $ do
    itDb "creates sane SQL" $  do
      (query, args) <- showQuery ES.SELECT $ from $ \p ->
            return $ EP.stringAggWith EP.AggModeDistinct (p ^. PersonName)
                     (val " ") [desc $ p ^. PersonName]
      liftIO $ query `shouldBe`
        "SELECT string_agg(DISTINCT \"Person\".\"name\", ? \
        \ORDER BY \"Person\".\"name\" DESC)\n\
        \FROM \"Person\"\n"
      liftIO $ args `shouldBe` [PersistText " "]

    itDb "works on an example" $  do
      let people = [p1, p2, p3 {personName = "John"}, p4, p5]
      mapM_ insert people
      [Value (Just ret)] <-
        select $ from $ \p ->
          return $ EP.stringAggWith EP.AggModeDistinct (p ^. PersonName) (val " ")
                   [desc $ p ^. PersonName]
      liftIO $ (words ret) `shouldBe`
        (L.reverse . L.sort . L.nub $ map personName people)





testAggregateFunctions :: SpecDb
testAggregateFunctions = do
  describe "arrayAgg" $ do
    itDb "looks sane" $  do
      let people = [p1, p2, p3, p4, p5]
      mapM_ insert people
      [Value (Just ret)] <-
        select $ from $ \p -> return (EP.arrayAgg (p ^. PersonName))
      liftIO $ L.sort ret `shouldBe` L.sort (map personName people)

    itDb "works on zero rows" $  do
      [Value ret] <-
        select $ from $ \p -> return (EP.arrayAgg (p ^. PersonName))
      liftIO $ ret `shouldBe` Nothing
  describe "arrayAggWith" testArrayAggWith
  describe "stringAgg" $ do
    itDb "looks sane" $
       do
        let people = [p1, p2, p3, p4, p5]
        mapM_ insert people
        [Value (Just ret)] <-
          select $
          from $ \p -> do
          return (EP.stringAgg (p ^. PersonName) (val " "))
        liftIO $ L.sort (words ret) `shouldBe` L.sort (map personName people)
    itDb "works on zero rows" $  do
      [Value ret] <-
        select $ from $ \p -> return (EP.stringAgg (p ^. PersonName) (val " "))
      liftIO $ ret `shouldBe` Nothing
  describe "stringAggWith" testStringAggWith

  describe "array_remove (NULL)" $ do
    itDb "removes NULL from arrays from nullable fields" $  do
      mapM_ insert [ Person "1" Nothing   Nothing 1
                   , Person "2" (Just 7)  Nothing 1
                   , Person "3" (Nothing) Nothing 1
                   , Person "4" (Just 8)  Nothing 2
                   , Person "5" (Just 9)  Nothing 2
                   ]
      ret <- select $ from $ \(person :: SqlExpr (Entity Person)) -> do
        groupBy (person ^. PersonFavNum)
        return . EP.arrayRemoveNull . EP.maybeArray . EP.arrayAgg
          $ person ^. PersonAge
      liftIO $ (L.sort $ map (L.sort . unValue) ret)
        `shouldBe` [[7], [8,9]]

  describe "maybeArray" $ do
    itDb "Coalesces NULL into an empty array" $  do
      [Value ret] <-
        select $ from $ \p ->
          return (EP.maybeArray $ EP.arrayAgg (p ^. PersonName))
      liftIO $ ret `shouldBe` []

testPostgresModule :: SpecDb
testPostgresModule = do
    describe "date_trunc" $ modifyMaxSuccess (`div` 10) $ do
        propDb "works" $ \run listOfDateParts -> run $ do
            let
                utcTimes =
                    map
                      (\(y, m, d, s) ->
                        fromInteger s
                          `addUTCTime`
                            UTCTime (fromGregorian (2000 + y) m d) 0
                      )
                    listOfDateParts
                truncateDate
                    :: SqlExpr (Value String)  -- ^ .e.g (val "day")
                    -> SqlExpr (Value UTCTime) -- ^ input field
                    -> SqlExpr (Value UTCTime) -- ^ truncated date
                truncateDate datePart expr =
                    ES.unsafeSqlFunction "date_trunc" (datePart, expr)
                vals =
                    zip (map (DateTruncTestKey . fromInteger) [1..]) utcTimes
            for_ vals $ \(idx, utcTime) -> do
                insertKey idx (DateTruncTest utcTime)

            -- Necessary to get the test to pass; see the discussion in
            -- https://github.com/bitemyapp/esqueleto/pull/180
            rawExecute "SET TIME ZONE 'UTC'" []
            ret <-
                fmap (Map.fromList . coerce :: _ -> Map DateTruncTestId (UTCTime, UTCTime)) $
                select $
                from $ \dt -> do
                pure
                  ( dt ^. DateTruncTestId
                  , ( dt ^. DateTruncTestCreated
                    , truncateDate (val "day") (dt ^. DateTruncTestCreated)
                    )
                  )

            asserting $ for_ vals $ \(idx, utcTime) -> do
              case Map.lookup idx ret of
                Nothing ->
                  expectationFailure "index not found"
                Just (original, truncated) -> do
                  utcTime `shouldBe` original
                  if utctDay utcTime == utctDay truncated
                    then
                      utctDay utcTime `shouldBe` utctDay truncated
                    else
                      -- use this if/else to get a better error message
                      utcTime `shouldBe` truncated

    describe "PostgreSQL module" $ do
        describe "Aggregate functions" testAggregateFunctions
        itDb "chr looks sane" $ do
            [Value (ret :: String)] <- select $ return (EP.chr (val 65))
            liftIO $ ret `shouldBe` "A"

        itDb "allows unit for functions" $ do
            let
                fn :: SqlExpr (Value UTCTime)
                fn = ES.unsafeSqlFunction "now" ()
            vals <- select $ pure fn
            liftIO $ vals `shouldSatisfy` ((1 ==) . length)

        itDb "works with now" $
           do
            nowDb <- select $ return EP.now_
            nowUtc <- liftIO getCurrentTime
            let oneSecond = realToFrac (1 :: Double)

            -- | Check the result is not null
            liftIO $ nowDb `shouldSatisfy` (not . null)

            -- | Unpack the now value
            let (Value now: _) = nowDb

            -- | Get the time diff and check it's less than a second
            liftIO $ diffUTCTime nowUtc now `shouldSatisfy` (< oneSecond)

testJSONInsertions :: SpecDb
testJSONInsertions =
    describe "JSON Insertions" $ do
        itDb "adds scalar values" $ do
            insertIt Null
            insertIt $ Bool True
            insertIt $ Number 1
            insertIt $ String "test"
        itDb "adds arrays" $ do
            insertIt $ toJSON ([] :: [A.Value])
            insertIt $ toJSON [Number 1, Bool True, Null]
            insertIt $ toJSON [String "test",object ["a" .= Number 3.14], Null, Bool True]
        itDb "adds objects" $ do
            insertIt $ object ["a" .= (1 :: Int), "b" .= False]
            insertIt $ object ["a" .= object ["b" .= object ["c" .= String "message"]]]
  where
    insertIt :: MonadIO m => A.Value -> SqlPersistT m ()
    insertIt = insert_ . Json . JSONB

testJSONOperators :: SpecDb
testJSONOperators =
  describe "JSON Operators" $ do
    testArrowOperators
    testFilterOperators
    testConcatDeleteOperators

testArrowOperators :: SpecDb
testArrowOperators =
  describe "Arrow Operators" $ do
    testArrowJSONB
    testArrowText
    testHashArrowJSONB
    testHashArrowText

testArrowJSONB :: SpecDb
testArrowJSONB =
    describe "Single Arrow (JSONB)" $ do
        itDb "creates sane SQL" $
            createSaneSQL @JSONValue
                (jsonbVal (object ["a" .= True]) ->. "a")
                "SELECT (? -> ?)\nFROM \"Json\"\n"
                [ PersistLiteralEscaped "{\"a\":true}"
                , PersistText "a"
                ]
        itDb "creates sane SQL (chained)" $ do
            let obj = object ["a" .= [1 :: Int,2,3]]
            createSaneSQL @JSONValue
              (jsonbVal obj ->. "a" ->. 1)
              "SELECT ((? -> ?) -> ?)\nFROM \"Json\"\n"
              [ PersistLiteralEscaped "{\"a\":[1,2,3]}"
              , PersistText "a"
              , PersistInt64 1 ]
        itDb "works as expected" $  do
          x <- selectJSONwhere $ \v -> v ->. "b" ==. jsonbVal (Bool False)
          y <- selectJSONwhere $ \v -> v ->. 1 ==. jsonbVal (Bool True)
          z <- selectJSONwhere $ \v -> v ->. "a" ->. "b" ->. "c" ==. jsonbVal (String "message")
          asserting $ do
              length x `shouldBe` 1
              length y `shouldBe` 1
              length z `shouldBe` 1

testArrowText :: SpecDb
testArrowText =
    describe "Single Arrow (Text)" $ do
        itDb "creates sane SQL" $
          createSaneSQL
            (jsonbVal (object ["a" .= True]) ->>. "a")
            "SELECT (? ->> ?)\nFROM \"Json\"\n"
            [ PersistLiteralEscaped "{\"a\":true}"
            , PersistText "a" ]
        itDb "creates sane SQL (chained)" $ do
          let obj = object ["a" .= [1 :: Int,2,3]]
          createSaneSQL
            (jsonbVal obj ->. "a" ->>. 1)
            "SELECT ((? -> ?) ->> ?)\nFROM \"Json\"\n"
            [ PersistLiteralEscaped "{\"a\":[1,2,3]}"
            , PersistText "a"
            , PersistInt64 1 ]
        itDb "works as expected" $  do
          x <- selectJSONwhere $ \v -> v ->>. "b" ==. just (val "false")
          y <- selectJSONwhere $ \v -> v ->>. 1 ==. just (val "true")
          z <- selectJSONwhere $ \v -> v ->. "a" ->. "b" ->>. "c" ==. just (val "message")
          liftIO $ length x `shouldBe` 1
          liftIO $ length y `shouldBe` 1
          liftIO $ length z `shouldBe` 1

testHashArrowJSONB :: SpecDb
testHashArrowJSONB =
  describe "Double Arrow (JSONB)" $ do
    itDb "creates sane SQL" $ do
      let list = ["a","b","c"]
      createSaneSQL @JSONValue
        (jsonbVal (object ["a" .= True]) #>. list)
        "SELECT (? #> ?)\nFROM \"Json\"\n"
        [ PersistLiteralEscaped "{\"a\":true}"
        , persistTextArray list ]
    itDb "creates sane SQL (chained)" $ do
      let obj = object ["a" .= [object ["b" .= True]]]
      createSaneSQL @JSONValue
        (jsonbVal obj #>. ["a","1"] #>. ["b"])
        "SELECT ((? #> ?) #> ?)\nFROM \"Json\"\n"
        [ PersistLiteralEscaped "{\"a\":[{\"b\":true}]}"
        , persistTextArray ["a","1"]
        , persistTextArray ["b"] ]
    itDb "works as expected" $  do
      x <- selectJSONwhere $ \v -> v #>. ["a","b","c"] ==. jsonbVal (String "message")
      y <- selectJSONwhere $ \v -> v #>. ["1","a"] ==. jsonbVal (Number 3.14)
      z <- selectJSONwhere $ \v -> v #>. ["1"] #>. ["a"] ==. jsonbVal (Number 3.14)
      liftIO $ length x `shouldBe` 1
      liftIO $ length y `shouldBe` 1
      liftIO $ length z `shouldBe` 1

testHashArrowText :: SpecDb
testHashArrowText =
  describe "Double Arrow (Text)" $ do
    itDb "creates sane SQL" $ do
      let list = ["a","b","c"]
      createSaneSQL
        (jsonbVal (object ["a" .= True]) #>>. list)
        "SELECT (? #>> ?)\nFROM \"Json\"\n"
        [ PersistLiteralEscaped "{\"a\":true}"
        , persistTextArray list ]
    itDb "creates sane SQL (chained)" $ do
      let obj = object ["a" .= [object ["b" .= True]]]
      createSaneSQL
        (jsonbVal obj #>. ["a","1"] #>>. ["b"])
        "SELECT ((? #> ?) #>> ?)\nFROM \"Json\"\n"
        [ PersistLiteralEscaped "{\"a\":[{\"b\":true}]}"
        , persistTextArray ["a","1"]
        , persistTextArray ["b"] ]
    itDb "works as expected" $  do
      x <- selectJSONwhere $ \v -> v #>>. ["a","b","c"] ==. just (val "message")
      y <- selectJSONwhere $ \v -> v #>>. ["1","a"] ==. just (val "3.14")
      z <- selectJSONwhere $ \v -> v #>. ["1"] #>>. ["a"] ==. just (val "3.14")
      liftIO $ length x `shouldBe` 1
      liftIO $ length y `shouldBe` 1
      liftIO $ length z `shouldBe` 1


testFilterOperators :: SpecDb
testFilterOperators =
    describe "Filter Operators" $ do
        testInclusion
        testQMark
        testQMarkAny
        testQMarkAll

testInclusion :: SpecDb
testInclusion = do
    describe "@>" $ do
        itDb "creates sane SQL" $ do
            let obj = object ["a" .= False, "b" .= True]
                encoded = BSL.toStrict $ encode obj
            createSaneSQL
                (jsonbVal obj  @>. jsonbVal (object ["a" .= False]))
                "SELECT (? @> ?)\nFROM \"Json\"\n"
                [ PersistLiteralEscaped encoded
                , PersistLiteralEscaped "{\"a\":false}"
                ]
        itDb "creates sane SQL (chained)" $ do
            let obj = object ["a" .= [object ["b" .= True]]]
                encoded = BSL.toStrict $ encode obj
            createSaneSQL
                (jsonbVal obj ->. "a" @>. jsonbVal (object ["b" .= True]))
                "SELECT ((? -> ?) @> ?)\nFROM \"Json\"\n"
                [ PersistLiteralEscaped encoded
                , PersistText "a"
                , PersistLiteralEscaped "{\"b\":true}"
                ]
        itDb "works as expected" $  do
          x <- selectJSONwhere $ \v -> v @>. jsonbVal (Number 1)
          y <- selectJSONwhere $ \v -> v @>. jsonbVal (toJSON [object ["a" .= Number 3.14]])
          z <- selectJSONwhere $ \v -> v ->. 1 @>. jsonbVal (object ["a" .= Number 3.14])
          liftIO $ length x `shouldBe` 2
          liftIO $ length y `shouldBe` 1
          liftIO $ length z `shouldBe` 1
    describe "<@" $ do
        itDb "creates sane SQL" $ do
            let obj = object ["a" .= False, "b" .= True]
                encoded = BSL.toStrict $ encode obj
            createSaneSQL
                (jsonbVal (object ["a" .= False]) <@. jsonbVal obj )
                "SELECT (? <@ ?)\nFROM \"Json\"\n"
                [ PersistLiteralEscaped "{\"a\":false}"
                , PersistLiteralEscaped encoded
                ]
        itDb "creates sane SQL (chained)" $ do
            let obj = object ["a" .= [object ["b" .= True]]]
                obj' = object ["b" .= True, "c" .= Null]
                encoded = BSL.toStrict $ encode obj'
            createSaneSQL
                (jsonbVal obj ->. "a" <@. jsonbVal obj')
                "SELECT ((? -> ?) <@ ?)\nFROM \"Json\"\n"
                [ PersistLiteralEscaped "{\"a\":[{\"b\":true}]}"
                , PersistText "a"
                , PersistLiteralEscaped encoded
                ]
        itDb "works as expected" $  do
            x <- selectJSONwhere $ \v -> v <@. jsonbVal (toJSON [Number 1])
            y <- selectJSONwhere $ \v -> v <@. jsonbVal (object ["a" .= (1 :: Int), "b" .= False, "c" .= Null])
            z <- selectJSONwhere $ \v -> v #>. ["a","b"] <@. jsonbVal (object ["b" .= False, "c" .= String "message"])
            liftIO $ length x `shouldBe` 2
            liftIO $ length y `shouldBe` 1
            liftIO $ length z `shouldBe` 1

testQMark :: SpecDb
testQMark = do
    describe "Question Mark" $ do
        itDb "creates sane SQL" $ do
            let obj = object ["a" .= False, "b" .= True]
                encoded = BSL.toStrict $ encode obj
            createSaneSQL
              (jsonbVal obj JSON.?. "a")
              "SELECT (? ?? ?)\nFROM \"Json\"\n"
              [ PersistLiteralEscaped encoded
              , PersistText "a"
              ]
        itDb "creates sane SQL (chained)" $ do
            let obj = object ["a" .= [object ["b" .= True]]]
                encoded = BSL.toStrict $ encode obj
            createSaneSQL
                (jsonbVal obj #>. ["a","0"] JSON.?. "b")
                "SELECT ((? #> ?) ?? ?)\nFROM \"Json\"\n"
                [ PersistLiteralEscaped encoded
                , persistTextArray ["a","0"]
                , PersistText "b"
                ]
        itDb "works as expected" $  do
            x <- selectJSONwhere (JSON.?. "a")
            y <- selectJSONwhere (JSON.?. "test")
            z <- selectJSONwhere $ \v -> v ->. "a" JSON.?. "b"
            liftIO $ length x `shouldBe` 2
            liftIO $ length y `shouldBe` 2
            liftIO $ length z `shouldBe` 1

testQMarkAny :: SpecDb
testQMarkAny = do
    describe "Question Mark (Any)" $ do
        itDb "creates sane SQL" $ do
            let obj = (object ["a" .= False, "b" .= True])
                encoded = BSL.toStrict $ encode obj
            createSaneSQL
                (jsonbVal obj  ?|. ["a","c"])
                "SELECT (? ??| ?)\nFROM \"Json\"\n"
                [ PersistLiteralEscaped encoded
                , persistTextArray ["a","c"]
                ]
        itDb "creates sane SQL (chained)" $ do
            let obj = object ["a" .= [object ["b" .= True]]]
                encoded = BSL.toStrict $ encode obj
            createSaneSQL
                (jsonbVal obj #>. ["a","0"] ?|. ["b","c"])
                "SELECT ((? #> ?) ??| ?)\nFROM \"Json\"\n"
                [ PersistLiteralEscaped encoded
                , persistTextArray ["a","0"]
                , persistTextArray ["b","c"]
                ]
        itDb "works as expected" $  do
          x <- selectJSONwhere (?|. ["b","test"])
          y <- selectJSONwhere (?|. ["a"])
          z <- selectJSONwhere $ \v -> v ->. (-3) ?|. ["a"]
          w <- selectJSONwhere (?|. [])
          liftIO $ length x `shouldBe` 3
          liftIO $ length y `shouldBe` 2
          liftIO $ length z `shouldBe` 1
          liftIO $ length w `shouldBe` 0

testQMarkAll :: SpecDb
testQMarkAll = do
    describe "Question Mark (All)" $ do
        itDb "creates sane SQL" $ do
            let obj = object ["a" .= False, "b" .= True]
                encoded = BSL.toStrict $ encode obj
            createSaneSQL
                (jsonbVal obj  ?&. ["a","c"])
                "SELECT (? ??& ?)\nFROM \"Json\"\n"
                [ PersistLiteralEscaped encoded
                , persistTextArray ["a","c"]
                ]
        itDb "creates sane SQL (chained)" $ do
            let obj = object ["a" .= [object ["b" .= True]]]
                encoded = BSL.toStrict $ encode obj
            createSaneSQL
                (jsonbVal obj #>. ["a","0"] ?&. ["b","c"])
                "SELECT ((? #> ?) ??& ?)\nFROM \"Json\"\n"
                [ PersistLiteralEscaped encoded
                , persistTextArray ["a","0"]
                , persistTextArray ["b","c"]
                ]
        itDb "works as expected" $  do
            x <- selectJSONwhere (?&. ["test"])
            y <- selectJSONwhere (?&. ["a","b"])
            z <- selectJSONwhere $ \v -> v ->. "a" ?&. ["b"]
            w <- selectJSONwhere (?&. [])
            liftIO $ length x `shouldBe` 2
            liftIO $ length y `shouldBe` 1
            liftIO $ length z `shouldBe` 1
            liftIO $ length w `shouldBe` 9

testConcatDeleteOperators :: SpecDb
testConcatDeleteOperators = do
  describe "Concatenation Operator" testConcatenationOperator
  describe "Deletion Operators" $ do
    testMinusOperator
    testMinusOperatorV10
    testHashMinusOperator

testConcatenationOperator :: SpecDb
testConcatenationOperator = do
    describe "Concatenation" $ do
        itDb "creates sane SQL" $ do
            let objAB = object ["a" .= False, "b" .= True]
                objC = object ["c" .= Null]
            createSaneSQL @JSONValue
                (jsonbVal objAB
                    JSON.||. jsonbVal objC)
                "SELECT (? || ?)\nFROM \"Json\"\n"
                [ PersistLiteralEscaped $ BSL.toStrict $ encode objAB
                , PersistLiteralEscaped $ BSL.toStrict $ encode objC
                ]
        itDb "creates sane SQL (chained)" $ do
            let obj = object ["a" .= [object ["b" .= True]]]
                encoded = BSL.toStrict $ encode obj
            createSaneSQL @JSONValue
                (jsonbVal obj ->. "a" JSON.||. jsonbVal (toJSON [Null]))
                "SELECT ((? -> ?) || ?)\nFROM \"Json\"\n"
                [ PersistLiteralEscaped encoded
                , PersistText "a"
                , PersistLiteralEscaped "[null]"
                ]
        itDb "works as expected" $  do
          x <- selectJSON $ \v -> do
              where_ $ v @>. jsonbVal (object [])
              where_ $ v JSON.||. jsonbVal (object ["x" .= True])
                              @>. jsonbVal (object ["x" .= True])
          y <- selectJSONwhere $ \v ->
              v JSON.||. jsonbVal (toJSON [String "a", String "b"])
                    ->>. 4 ==. just (val "b")
          z <- selectJSONwhere $ \v ->
              v JSON.||. jsonbVal (toJSON [Bool False])
                     ->. 0 JSON.@>. jsonbVal (Number 1)
          w <- selectJSON $ \v -> do
              where_ . not_ $ v @>. jsonbVal (object [])
              where_ $ jsonbVal (String "test1") JSON.||. v ->>. 0 ==. just (val "test1")
          liftIO $ length x `shouldBe` 2
          liftIO $ length y `shouldBe` 1
          liftIO $ length z `shouldBe` 2
          liftIO $ length w `shouldBe` 7

testMinusOperator :: SpecDb
testMinusOperator =
    describe "Minus Operator" $ do
        itDb "creates sane SQL" $ do
            let obj = object ["a" .= False, "b" .= True]
                encoded = BSL.toStrict $ encode obj
            createSaneSQL @JSONValue
                (jsonbVal obj JSON.-. "a")
                "SELECT (? - ?)\nFROM \"Json\"\n"
                [ PersistLiteralEscaped encoded
                , PersistText "a"
                ]
        itDb "creates sane SQL (chained)" $ do
            let obj = object ["a" .= [object ["b" .= True]]]
                encoded = BSL.toStrict $ encode obj
            createSaneSQL @JSONValue
                (jsonbVal obj ->. "a" JSON.-. 0)
                "SELECT ((? -> ?) - ?)\nFROM \"Json\"\n"
                [ PersistLiteralEscaped encoded
                , PersistText "a"
                , PersistInt64 0
                ]
        itDb "works as expected" $  do
            x <- selectJSON $ \v -> do
                where_ $ v @>. jsonbVal (toJSON ([] :: [Int]))
                where_ $ v JSON.-. 0 @>. jsonbVal (toJSON [Bool True])
            y <- selectJSON $ \v -> do
                where_ $ v @>. jsonbVal (toJSON ([] :: [Int]))
                where_ $ v JSON.-. (-1) @>. jsonbVal (toJSON [Null])
            z <- selectJSON_ $ \v -> v JSON.-. "b" ?&. ["a", "b"]
            w <- selectJSON_ $ \v -> do
                v JSON.-. "test" @>. jsonbVal (toJSON [String "test"])
            liftIO $ length x `shouldBe` 2
            liftIO $ length y `shouldBe` 1
            liftIO $ length z `shouldBe` 0
            liftIO $ length w `shouldBe` 0
            sqlFailWith "22023" $ selectJSONwhere $ \v ->
                v JSON.-. 0 @>. jsonbVal (toJSON ([] :: [Int]))
  where
    selectJSON_ f = selectJSON $ \v -> do
        where_
            $ v @>. jsonbVal (object [])
            ||. v @>. jsonbVal (toJSON ([] :: [Int]))
        where_ $ f v

testMinusOperatorV10 :: SpecDb
testMinusOperatorV10 = do
    describe "Minus Operator (PSQL >= v10)" $ do
        itDb "creates sane SQL" $ do
            let obj = object ["a" .= False, "b" .= True]
                encoded = BSL.toStrict $ encode obj
            createSaneSQL @JSONValue
                (jsonbVal obj  --. ["a","b"])
                "SELECT (? - ?)\nFROM \"Json\"\n"
                [ PersistLiteralEscaped encoded
                , persistTextArray ["a","b"]
                ]
        itDb "creates sane SQL (chained)" $ do
            let obj = object ["a" .= [object ["b" .= True]]]
                encoded = BSL.toStrict $ encode obj
            createSaneSQL @JSONValue
              (jsonbVal obj #>. ["a","0"] --. ["b"])
              "SELECT ((? #> ?) - ?)\nFROM \"Json\"\n"
              [ PersistLiteralEscaped encoded
              , persistTextArray ["a","0"]
              , persistTextArray ["b"]
              ]
        itDb "works as expected" $  do
            x <- selectJSON $ \v -> do
                where_ $ v @>. jsonbVal (toJSON ([] :: [Int]))
                where_ $ v --. ["test","a"] @>. jsonbVal (toJSON [String "test"])
            y <- selectJSON $ \v -> do
                where_ $ v @>. jsonbVal (object [])
                where_ $ v --. ["a","b"] <@. jsonbVal (object [])
            z <- selectJSON_ $ \v -> v --. ["b"] <@. jsonbVal (object ["a" .= (1 :: Int)])
            w <- selectJSON_ $ \v -> do
                v --. ["test"] @>. jsonbVal (toJSON [String "test"])
            liftIO $ length x `shouldBe` 0
            liftIO $ length y `shouldBe` 2
            liftIO $ length z `shouldBe` 1
            liftIO $ length w `shouldBe` 0
            sqlFailWith "22023" $ selectJSONwhere $ \v ->
                v --. ["a"] @>. jsonbVal (toJSON ([] :: [Int]))
  where
    selectJSON_ f = selectJSON $ \v -> do
        where_ $ v @>. jsonbVal (object [])
               ||. v @>. jsonbVal (toJSON ([] :: [Int]))
        where_ $ f v

testHashMinusOperator :: SpecDb
testHashMinusOperator =
  describe "Hash-Minus Operator" $ do
    itDb "creates sane SQL" $
      createSaneSQL @JSONValue
        (jsonbVal (object ["a" .= False, "b" .= True]) #-. ["a"])
        "SELECT (? #- ?)\nFROM \"Json\"\n"
        [ PersistLiteralEscaped (BSL.toStrict $ encode $ object ["a" .= False, "b" .= True])
        , persistTextArray ["a"] ]
    itDb "creates sane SQL (chained)" $ do
      let obj = object ["a" .= [object ["b" .= True]]]
      createSaneSQL @JSONValue
        (jsonbVal obj ->. "a" #-. ["0","b"])
        "SELECT ((? -> ?) #- ?)\nFROM \"Json\"\n"
        [ PersistLiteralEscaped (BSL.toStrict $ encode obj)
        , PersistText "a"
        , persistTextArray ["0","b"] ]
    itDb "works as expected" $  do
      x <- selectJSON $ \v -> do
          where_ $ v @>. jsonbVal (toJSON ([] :: [Int]))
          where_ $ v #-. ["1","a"] @>. jsonbVal (toJSON [object []])
      y <- selectJSON $ \v -> do
          where_ $ v @>. jsonbVal (toJSON ([] :: [Int]))
          where_ $ v #-. ["-3","a"] @>. jsonbVal (toJSON [object []])
      z <- selectJSON_ $ \v -> v #-. ["a","b","c"]
              @>. jsonbVal (object ["a" .= object ["b" .= object ["c" .= String "message"]]])
      w <- selectJSON_ $ \v -> v #-. ["a","b"] JSON.?. "b"
      liftIO $ length x `shouldBe` 1
      liftIO $ length y `shouldBe` 1
      liftIO $ length z `shouldBe` 0
      liftIO $ length w `shouldBe` 1
      sqlFailWith "22023" $ selectJSONwhere $ \v ->
          v #-. ["0"] @>. jsonbVal (toJSON ([] :: [Int]))
  where selectJSON_ f = selectJSON $ \v -> do
          where_ $ v @>. jsonbVal (object [])
          where_ $ f v

testInsertUniqueViolation :: SpecDb
testInsertUniqueViolation =
    describe "Unique Violation on Insert" $
        itDb "Unique throws exception" $ do
            eres <-
                try $ do
                    _ <- insert u1
                    _ <- insert u2
                    insert u3
            liftIO $ case eres of
                Left err | err == exception ->
                    pure ()
                _ ->
                    expectationFailure $ "Expected a SQL exception, got: " <>
                        show eres

  where
    exception = SqlError {
      sqlState = "23505",
      sqlExecStatus = FatalError,
      sqlErrorMsg = "duplicate key value violates unique constraint \"UniqueValue\"",
      sqlErrorDetail = "Key (value)=(0) already exists.",
      sqlErrorHint = ""}

testUpsert :: SpecDb
testUpsert =
  describe "Upsert test" $ do
    itDb "Upsert can insert like normal" $  do
      u1e <- EP.upsert u1 [OneUniqueName =. val "fifth"]
      liftIO $ entityVal u1e `shouldBe` u1
    itDb "Upsert performs update on collision" $  do
      u1e <- EP.upsert u1 [OneUniqueName =. val "fifth"]
      liftIO $ entityVal u1e `shouldBe` u1
      u2e <- EP.upsert u2 [OneUniqueName =. val "fifth"]
      liftIO $ entityVal u2e `shouldBe` u2
      u3e <- EP.upsert u3 [OneUniqueName =. val "fifth"]
      liftIO $ entityVal u3e `shouldBe` u1{oneUniqueName="fifth"}

testInsertSelectWithConflict :: SpecDb
testInsertSelectWithConflict =
  describe "insertSelectWithConflict test" $ do
    itDb "Should do Nothing when no updates set" $  do
      _ <- insert p1
      _ <- insert p2
      _ <- insert p3
      n1 <- EP.insertSelectWithConflictCount UniqueValue (
          from $ \p -> return $ OneUnique <# val "test" <&> (p ^. PersonFavNum)
        )
        (\current excluded -> [])
      uniques1 <- select $ Experimental.from $ Experimental.table @OneUnique
      n2 <- EP.insertSelectWithConflictCount UniqueValue (
          from $ \p -> return $ OneUnique <# val "test" <&> (p ^. PersonFavNum)
        )
        (\current excluded -> [])
      uniques2 <- select $ Experimental.from $ Experimental.table @OneUnique
      liftIO $ n1 `shouldBe` 3
      liftIO $ n2 `shouldBe` 0
      let test = map (OneUnique "test" . personFavNum) [p1,p2,p3]
      liftIO $ map entityVal uniques1 `shouldBe` test
      liftIO $ map entityVal uniques2 `shouldBe` test
    itDb "Should update a value if given an update on conflict" $  do
        _ <- insert p1
        _ <- insert p2
        _ <- insert p3
        -- Note, have to sum 4 so that the update does not conflicts again with another row.
        n1 <- EP.insertSelectWithConflictCount UniqueValue (
            from $ \p -> return $ OneUnique <# val "test" <&> (p ^. PersonFavNum)
          )
          (\current excluded -> [OneUniqueValue =. val 4 +. (current ^. OneUniqueValue) +. (excluded ^. OneUniqueValue)])
        uniques1 <- select $ Experimental.from $ Experimental.table @OneUnique
        n2 <- EP.insertSelectWithConflictCount UniqueValue (
            from $ \p -> return $ OneUnique <# val "test" <&> (p ^. PersonFavNum)
          )
          (\current excluded -> [OneUniqueValue =. val 4 +. (current ^. OneUniqueValue) +. (excluded ^. OneUniqueValue)])
        uniques2 <- select $ Experimental.from $ Experimental.table @OneUnique
        liftIO $ n1 `shouldBe` 3
        liftIO $ n2 `shouldBe` 3
        let test = map (OneUnique "test" . personFavNum) [p1,p2,p3]
            test2 = map (OneUnique "test" . (+4) . (*2) . personFavNum) [p1,p2,p3]
        liftIO $ map entityVal uniques1 `shouldBe` test
        liftIO $ map entityVal uniques2 `shouldBe` test2

testFilterWhere :: SpecDb
testFilterWhere =
  describe "filterWhere" $ do
    itDb "adds a filter clause to count aggregation" $  do
      -- Person "John"   (Just 36) Nothing   1
      _ <- insert p1
      -- Person "Rachel" Nothing   (Just 37) 2
      _ <- insert p2
      --  Person "Mike"   (Just 17) Nothing   3
      _ <- insert p3
      -- Person "Livia"  (Just 17) (Just 18) 4
      _ <- insert p4
      -- Person "Mitch"  Nothing   Nothing   5
      _ <- insert p5

      usersByAge <- do
          select $ from $ \users -> do
            groupBy $ users ^. PersonAge
            return
              ( users ^. PersonAge
              -- Nothing: [Rachel { favNum = 2 }, Mitch { favNum = 5 }] = 2
              -- Just 36: [John { favNum = 1 } (excluded)] = 0
              -- Just 17: [Mike { favNum = 3 }, Livia { favNum = 4 }] = 2
              , count (users ^. PersonId) `EP.filterWhere` (users ^. PersonFavNum >=. val 2)
              -- Nothing: [Rachel { favNum = 2 } (excluded), Mitch { favNum = 5 } (excluded)] = 0
              -- Just 36: [John { favNum = 1 }] = 1
              -- Just 17: [Mike { favNum = 3 } (excluded), Livia { favNum = 4 } (excluded)] = 0
              , count (users ^. PersonFavNum) `EP.filterWhere` (users ^. PersonFavNum <. val 2)
              )

      asserting $ usersByAge `shouldMatchList`
          (
              [ (Value Nothing, Value 2, Value 0)
              , (Value (Just 36), Value 0, Value 1)
              , (Value (Just 17), Value 2, Value 0)
              ] :: [(Value (Maybe Int), Value Int, Value Int)]
          )


    itDb "adds a filter clause to sum aggregation" $  do
      -- Person "John"   (Just 36) Nothing   1
      _ <- insert p1
      -- Person "Rachel" Nothing   (Just 37) 2
      _ <- insert p2
      --  Person "Mike"   (Just 17) Nothing   3
      _ <- insert p3
      -- Person "Livia"  (Just 17) (Just 18) 4
      _ <- insert p4
      -- Person "Mitch"  Nothing   Nothing   5
      _ <- insert p5

      usersByAge <- fmap (\(Value a, Value b, Value c) -> (a, b, c)) <$> do
          select $ from $ \users -> do
            groupBy $ users ^. PersonAge
            return
              ( users ^. PersonAge
              -- Nothing: [Rachel { favNum = 2 }, Mitch { favNum = 5 }] = Just 7
              -- Just 36: [John { favNum = 1 } (excluded)] = Nothing
              -- Just 17: [Mike { favNum = 3 }, Livia { favNum = 4 }] = Just 7
              , sum_ (users ^. PersonFavNum) `EP.filterWhere` (users ^. PersonFavNum >=. val 2)
              -- Nothing: [Rachel { favNum = 2 } (excluded), Mitch { favNum = 5 } (excluded)] = Nothing
              -- Just 36: [John { favNum = 1 }] = Just 1
              -- Just 17: [Mike { favNum = 3 } (excluded), Livia { favNum = 4 } (excluded)] = Nothing
              , sum_ (users ^. PersonFavNum) `EP.filterWhere` (users ^. PersonFavNum <. val 2)
              )

      liftIO $ usersByAge `shouldMatchList`
        ( [ (Nothing, Just 7, Nothing)
          , (Just 36, Nothing, Just 1)
          , (Just 17, Just 7, Nothing)
          ] :: [(Maybe Int, Maybe Rational, Maybe Rational)]
        )

testCommonTableExpressions :: SpecDb
testCommonTableExpressions = do
    describe "You can run them" $ do
        itDb "will run" $ do
            void $ select $ do
                limitedLordsCte <-
                    Experimental.with $ do
                        lords <- Experimental.from $ Experimental.table @Lord
                        limit 10
                        pure lords
                lords <- Experimental.from limitedLordsCte
                orderBy [asc $ lords ^. LordId]
                pure lords

            asserting noExceptions

    itDb "can do multiple recursive queries" $ do
        let
            oneToTen =
                Experimental.withRecursive
                    (pure $ val (1 :: Int))
                    Experimental.unionAll_
                    (\self -> do
                        v <- Experimental.from self
                        where_ $ v <. val 10
                        pure $ v +. val 1
                    )

        vals <- select $ do
            cte <- oneToTen
            cte2 <- oneToTen
            res1 <- Experimental.from cte
            res2 <- Experimental.from cte2
            pure (res1, res2)
        asserting $ vals `shouldBe` (((,) <$> fmap Value [1..10] <*> fmap Value [1..10]))

    itDb "passing previous query works" $ do
        let
            oneToTen =
                Experimental.withRecursive
                     (pure $ val (1 :: Int))
                     Experimental.unionAll_
                     (\self -> do
                         v <- Experimental.from self
                         where_ $ v <. val 10
                         pure $ v +. val 1
                     )

            oneMore q =
                Experimental.with $ do
                    v <- Experimental.from q
                    pure $ v +. val 1
        vals <- select $ do
            cte <- oneToTen
            cte2 <- oneMore cte
            res <- Experimental.from cte2
            pure res
        asserting $ vals `shouldBe` fmap Value [2..11]

-- Since lateral queries arent supported in Sqlite or older versions of mysql
-- the test is in the Postgres module
testLateralQuery :: SpecDb
testLateralQuery = do
    describe "Lateral queries" $ do
        itDb "supports CROSS JOIN LATERAL" $ do
          _ <-  do
            select $ do
                l :& c <-
                  Experimental.from $ table @Lord
                  `CrossJoin` \lord -> do
                        deed <- Experimental.from $ table @Deed
                        where_ $ lord ^. LordId ==. deed ^. DeedOwnerId
                        pure $ countRows @Int
                pure (l, c)
          liftIO $ True `shouldBe` True

        itDb "supports INNER JOIN LATERAL" $ do
            let subquery lord = do
                                deed <- Experimental.from $ table @Deed
                                where_ $ lord ^. LordId ==. deed ^. DeedOwnerId
                                pure $ countRows @Int
            res <- select $ do
              l :& c <- Experimental.from $ table @Lord
                              `InnerJoin` subquery
                              `Experimental.on` (const $ val True)
              pure (l, c)

            let _ = res :: [(Entity Lord, Value Int)]
            asserting noExceptions

        itDb "supports LEFT JOIN LATERAL" $ do
            res <- select $ do
                l :& c <- Experimental.from $ table @Lord
                                `LeftOuterJoin` (\lord -> do
                                          deed <- Experimental.from $ table @Deed
                                          where_ $ lord ^. LordId ==. deed ^. DeedOwnerId
                                          pure $ countRows @Int)
                                `Experimental.on` (const $ val True)
                pure (l, c)

            let _ = res :: [(Entity Lord, Value (Maybe Int))]
            asserting noExceptions

testValuesExpression :: SpecDb
testValuesExpression = do
    describe "(VALUES (..)) query" $ do
        itDb "works with joins and other sql expressions" $ do
            p1k <- insert p1
            p2k <- insert p2
            p3k <- insert p3
            let exprs :: NE.NonEmpty (SqlExpr (Value Int), SqlExpr (Value Text))
                exprs =   (val 10, val "ten")
                    NE.:| [ (val 20, val "twenty")
                        , (val 30, val "thirty") ]
                query = do
                    (bound, boundName) :& person <- Experimental.from $
                        EP.values exprs
                        `Experimental.InnerJoin` table @Person
                        `Experimental.on` (\((bound, boundName) :& person) -> person^.PersonAge >=. just bound)
                    groupBy bound
                    orderBy [ asc bound ]
                    pure (bound, count @Int $ person^.PersonName)
            result <- select query
            liftIO $ result `shouldBe` [ (Value 10, Value 2)
                                       , (Value 20, Value 1)
                                       , (Value 30, Value 1) ]

        itDb "supports single-column query" $ do
            let query = do
                    vInt <- Experimental.from $ EP.values $ val 1 NE.:| [ val 2, val 3 ]
                    pure (vInt :: SqlExpr (Value Int))
            result <- select query
            asserting noExceptions
            liftIO $ result `shouldBe` [ Value 1, Value 2, Value 3 ]

        itDb "supports multi-column query (+ nested simple expression and null)" $ do
            let query = do
                    (vInt, vStr, vDouble) <- Experimental.from
                        $ EP.values $ (val 1, val "str1", coalesce [just $ val 1.0, nothing])
                                NE.:| [ (val 2, val "str2", just $ val 2.5)
                                      , (val 3, val "str3", nothing) ]
                    pure ( vInt :: SqlExpr (Value Int)
                            , vStr :: SqlExpr (Value Text)
                            , vDouble :: SqlExpr (Value (Maybe Double)) )
            result <- select query
            asserting noExceptions
            liftIO $ result `shouldBe` [ (Value 1, Value "str1", Value $ Just 1.0)
                                       , (Value 2, Value "str2", Value $ Just 2.5)
                                       , (Value 3, Value "str3", Value Nothing) ]

testWindowFunctions :: SpecDb
testWindowFunctions = do
    describe "Window Functions" $ do

        itDb "supports over ()" $ do
            _ <- insert $ Numbers 1 2
            _ <- insert $ Numbers 2 4
            _ <- insert $ Numbers 3 5
            _ <- insert $ Numbers 6 7
            let query = do
                    n <- Experimental.from $ table @Numbers
                    pure ( n ^. NumbersInt
                         , Window.sum_ @Double (n ^. NumbersDouble) `Window.over_` ()
                         )
            result <- select query
            asserting noExceptions
            asserting $ result `shouldMatchList`
                                          [ (Value 1, Value (Just 18.0))
                                          , (Value 2, Value (Just 18.0))
                                          , (Value 3, Value (Just 18.0))
                                          , (Value 6, Value (Just 18.0))]

        itDb "supports partitioning" $ do
            _ <- insert $ Numbers 1 2
            _ <- insert $ Numbers 2 4
            _ <- insert $ Numbers 3 5
            _ <- insert $ Numbers 6 7

            let (%.)  = ES.unsafeSqlBinOp " % "
            let query = do
                    n <- Experimental.from $ table @Numbers
                    pure ( n ^. NumbersInt
                         , Window.sum_ @Double (n ^. NumbersDouble)
                            `Window.over_` (Window.partitionBy_ (n ^. NumbersInt %. val @Int 2))
                         )
            result <- select query
            asserting noExceptions
            asserting $ result `shouldMatchList`
                                          [ (Value 1, Value (Just 7.0))
                                          , (Value 2, Value (Just 11.0))
                                          , (Value 3, Value (Just 7.0))
                                          , (Value 6, Value (Just 11.0))
                                          ]

        itDb "supports running total" $ do
            _ <- insert $ Numbers 1 2
            _ <- insert $ Numbers 2 4
            _ <- insert $ Numbers 3 5
            _ <- insert $ Numbers 6 7
            let query = do
                    n <- Experimental.from $ table @Numbers
                    pure ( n ^. NumbersInt
                         , Window.sum_ @Double (n ^. NumbersDouble) `Window.over_` (Window.orderBy_ [asc (n ^. NumbersInt)]
                                         <> Window.frame_ Window.unboundedPreceding)
                         )
            result <- select query
            asserting noExceptions
            asserting $ result `shouldBe` [ (Value 1, Value (Just 2.0))
                                          , (Value 2, Value (Just 6.0))
                                          , (Value 3, Value (Just 11.0))
                                          , (Value 6, Value (Just 18.0))]

        itDb "supports running total excluding current row and addition to sum" $ do
            _ <- insert $ Numbers 1 2
            _ <- insert $ Numbers 2 4
            _ <- insert $ Numbers 3 5
            _ <- insert $ Numbers 6 7
            let query = do
                    n <- Experimental.from $ table @Numbers
                    pure ( n ^. NumbersInt
                         , Window.liftExpr (just (n ^. NumbersDouble)) +.
                             Window.sum_ (n ^. NumbersDouble)
                                `Window.over_` (Window.orderBy_ [asc (n ^. NumbersInt)]
                                             <> Window.frame_ (Window.excludeCurrentRow Window.unboundedPreceding)
                                             )
                         )
            result <- select query
            asserting noExceptions
            asserting $ result `shouldBe` [ (Value 1, Value Nothing)
                                          , (Value 2, Value (Just 6.0))
                                          , (Value 3, Value (Just 11.0))
                                          , (Value 6, Value (Just 18.0))]

        itDb "supports postgres filter and over clauses" $ do
            _ <- insert $ Numbers 1 2
            _ <- insert $ Numbers 2 4
            _ <- insert $ Numbers 3 5
            _ <- insert $ Numbers 6 7
            let query = do
                    n <- Experimental.from $ table @Numbers
                    pure ( n ^. NumbersInt
                         , Window.sum_ @Double (n ^. NumbersDouble)
                            `EP.filterWhere` (n ^. NumbersInt >. val 2)
                            `Window.over_` (Window.frame_ Window.unboundedPreceding)
                         )
            result <- select query
            asserting noExceptions
            asserting $ result `shouldBe` [ (Value 1, Value Nothing)
                                          , (Value 2, Value Nothing)
                                          , (Value 3, Value (Just 5.0))
                                          , (Value 6, Value (Just 12.0))]

type JSONValue = Maybe (JSONB A.Value)

createSaneSQL :: (PersistField a, MonadIO m) => SqlExpr (Value a) -> T.Text -> [PersistValue] -> SqlPersistT m ()
createSaneSQL act q vals = do
    (query, args) <- showQuery ES.SELECT $ fromValue act
    liftIO $ do
        query `shouldBe` q
        args `shouldBe` vals

fromValue :: (PersistField a) => SqlExpr (Value a) -> SqlQuery (SqlExpr (Value a))
fromValue act = from $ \x -> do
    let _ = x :: SqlExpr (Entity Json)
    return act

persistTextArray :: [T.Text] -> PersistValue
persistTextArray = PersistArray . fmap PersistText

sqlFailWith
    :: (HasCallStack, MonadUnliftIO m, Show a)
    => ByteString
    -> SqlPersistT m a
    -> SqlPersistT m ()
sqlFailWith errState f = do
    eres <- try f
    case eres of
        Left err ->
            success err
        Right a ->
            liftIO $ expectationFailure $ mconcat
                [ "should fail with error code: "
                , T.unpack errStateT
                , ", but got: "
                , show a
                ]
  where
    success SqlError{sqlState}
        | sqlState == errState =
            pure ()
        | otherwise = do
            liftIO $ expectationFailure $ T.unpack $ T.concat
                [ "should fail with: ", errStateT
                , ", but received: ", TE.decodeUtf8 sqlState
                ]
    errStateT =
        TE.decodeUtf8 errState

selectJSONwhere
  :: MonadIO m
  => (JSONBExpr A.Value -> SqlExpr (Value Bool))
  -> SqlPersistT m [Entity Json]
selectJSONwhere f = selectJSON $ where_ . f

selectJSON
  :: MonadIO m
  => (JSONBExpr A.Value -> SqlQuery ())
  -> SqlPersistT m [Entity Json]
selectJSON f = select $ from $ \v -> do
    f $ just (v ^. JsonValue)
    return v

--------------- JSON --------------- JSON --------------- JSON ---------------
--------------- JSON --------------- JSON --------------- JSON ---------------
--------------- JSON --------------- JSON --------------- JSON ---------------



spec :: Spec
spec = beforeAll mkConnectionPool $ do
    tests

    describe "PostgreSQL specific tests" $ do
        testAscRandom random_
        testRandomMath
        testSelectDistinctOn
        testPostgresModule
        testPostgresqlOneAscOneDesc
        testPostgresqlTwoAscFields
        testPostgresqlSum
        testPostgresqlRandom
        testPostgresqlUpdate
        testPostgresqlCoalesce
        testPostgresqlTextFunctions
        testInsertUniqueViolation
        testUpsert
        testInsertSelectWithConflict
        testFilterWhere
        testCommonTableExpressions
        setDatabaseState insertJsonValues cleanJSON
            $ describe "PostgreSQL JSON tests" $ do
                testJSONInsertions
                testJSONOperators
        testLateralQuery
        testValuesExpression
        testWindowFunctions

insertJsonValues :: SqlPersistT IO ()
insertJsonValues = do
    insertIt Null
    insertIt $ Bool True
    insertIt $ Number 1
    insertIt $ String "test"
    insertIt $ toJSON ([] :: [A.Value])
    insertIt $ toJSON [Number 1, Bool True, Null]
    insertIt $ toJSON [String "test",object ["a" .= Number 3.14], Null, Bool True]
    insertIt $ object ["a" .= (1 :: Int), "b" .= False]
    insertIt $ object ["a" .= object ["b" .= object ["c" .= String "message"]]]
  where
    insertIt :: MonadIO m => A.Value -> SqlPersistT m ()
    insertIt = insert_ . Json . JSONB

verbose :: Bool
verbose = False

migrateIt :: _ => SqlPersistT m ()
migrateIt = mapReaderT runNoLoggingT $ do
    void $ runMigrationSilent $ do
        migrateAll
        migrateUnique
        migrateJSON
    cleanDB
    cleanUniques

mkConnectionPool :: IO ConnectionPool
mkConnectionPool = do
    verbose' <- lookupEnv "VERBOSE" >>= \case
        Nothing ->
            return verbose
        Just x
            | map Char.toLower x == "true" -> return True
            | null x -> return True
            | otherwise -> return False
    pool <- if verbose'
        then
            runStderrLoggingT $
                createPostgresqlPool
                "host=localhost port=5432 user=esqutest password=esqutest dbname=esqutest"
                4
        else
            runNoLoggingT $
                createPostgresqlPool
                "host=localhost port=5432 user=esqutest password=esqutest dbname=esqutest"
                4
    flip runSqlPool pool $ do
        migrateIt
    pure pool

-- | Show the SQL generated by a query
showQuery :: (Monad m, ES.SqlSelect a r, BackendCompatible SqlBackend backend)
          => ES.Mode -> SqlQuery a -> ReaderT backend m (T.Text, [PersistValue])
showQuery mode query = do
  backend <- ask
  let (builder, values) = ES.toRawSql mode (backend, ES.initialIdentState) query
  return (ES.builderToText builder, values)
