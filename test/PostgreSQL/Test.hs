{-# OPTIONS_GHC -fno-warn-unused-binds  #-}
{-# LANGUAGE FlexibleContexts
           , LambdaCase
           , NamedFieldPuns
           , OverloadedStrings
           , RankNTypes
           , ScopedTypeVariables
           , TypeApplications
           , TypeFamilies
           , PartialTypeSignatures
 #-}
module Main (main) where

import Data.Coerce
import Data.Foldable
import qualified Data.Map.Strict as Map
import Data.Map (Map)
import Data.Time
import Control.Arrow ((&&&))
import Control.Monad (void, when)
import Control.Monad.Catch (MonadCatch, catch)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger (runStderrLoggingT, runNoLoggingT)
import Control.Monad.Trans.Reader (ReaderT, ask)
import qualified Control.Monad.Trans.Resource as R
import Data.Aeson hiding (Value)
import qualified Data.Aeson as A (Value)
import Data.ByteString (ByteString)
import qualified Data.Char as Char
import qualified Data.List as L
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime)
import Database.Esqueleto hiding (random_)
import Database.Esqueleto.Experimental hiding (random_, from, on)
import qualified Database.Esqueleto.Experimental as Experimental
import qualified Database.Esqueleto.Internal.Sql as ES
import Database.Esqueleto.PostgreSQL (random_)
import qualified Database.Esqueleto.PostgreSQL as EP
import Database.Esqueleto.PostgreSQL.JSON hiding ((?.), (-.), (||.))
import qualified Database.Esqueleto.PostgreSQL.JSON as JSON
import Database.Persist.Postgresql (withPostgresqlConn)
import Database.PostgreSQL.Simple (SqlError(..), ExecStatus(..))
import System.Environment
import Test.Hspec
import Test.Hspec.QuickCheck

import Common.Test
import PostgreSQL.MigrateJSON



testPostgresqlCoalesce :: Spec
testPostgresqlCoalesce = do
  it "works on PostgreSQL and MySQL with <2 arguments" $
    run $ do
      _ :: [Value (Maybe Int)] <-
        select $
        from $ \p -> do
        return (coalesce [p ^. PersonAge])
      return ()

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
                  ((%) ++. val t ++. (%)))
         orderBy [asc (p ^. PersonName)]
         return p
  liftIO $ ret `shouldBe` expected


testPostgresqlTextFunctions :: Spec
testPostgresqlTextFunctions = do
  describe "text functions" $ do
    it "like, (%) and (++.) work on a simple example" $
       run $ do
         [p1e, p2e, p3e, p4e] <- mapM insert' [p1, p2, p3, p4]
         nameContains like "h"  [p1e, p2e]
         nameContains like "i"  [p4e, p3e]
         nameContains like "iv" [p4e]

    it "ilike, (%) and (++.) work on a simple example on PostgreSQL" $
      run $ do
        [p1e, _, p3e, _, p5e] <- mapM insert' [p1, p2, p3, p4, p5]
        let nameContains' t expected = do
              ret <- select $
                     from $ \p -> do
                     where_ (p ^. PersonName `ilike` (%) ++. val t ++. (%))
                     orderBy [asc (p ^. PersonName)]
                     return p
              liftIO $ ret `shouldBe` expected
        nameContains' "mi" [p3e, p5e]
        nameContains' "JOHN" [p1e]





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





testPostgresqlRandom :: Spec
testPostgresqlRandom = do
  it "works with random_" $
    run $ do
      _ <- select $ return (random_ :: SqlExpr (Value Double))
      return ()





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





testArrayAggWith :: Spec
testArrayAggWith = do
  describe "ALL, no ORDER BY" $ do
    it "creates sane SQL" $ run $ do
      (query, args) <- showQuery ES.SELECT $ from $ \p ->
            return (EP.arrayAggWith EP.AggModeAll (p ^. PersonAge) [])
      liftIO $ query `shouldBe`
        "SELECT array_agg(\"Person\".\"age\")\n\
        \FROM \"Person\"\n"
      liftIO $ args `shouldBe` []

    it "works on an example" $ run $ do
      let people = [p1, p2, p3, p4, p5]
      mapM_ insert people
      [Value (Just ret)] <-
        select . from $ \p ->
          return (EP.arrayAggWith EP.AggModeAll (p ^. PersonName) [])
      liftIO $ L.sort ret `shouldBe` L.sort (map personName people)

  describe "DISTINCT, no ORDER BY" $ do
    it "creates sane SQL" $ run $ do
      (query, args) <- showQuery ES.SELECT $ from $ \p ->
            return (EP.arrayAggWith EP.AggModeDistinct (p ^. PersonAge) [])
      liftIO $ query `shouldBe`
        "SELECT array_agg(DISTINCT \"Person\".\"age\")\n\
        \FROM \"Person\"\n"
      liftIO $ args `shouldBe` []

    it "works on an example" $ run $ do
      let people = [p1, p2, p3, p4, p5]
      mapM_ insert people
      [Value (Just ret)] <-
        select . from $ \p ->
          return (EP.arrayAggWith EP.AggModeDistinct (p ^. PersonAge) [])
      liftIO $ L.sort ret `shouldBe` [Nothing, Just 17, Just 36]

  describe "ALL, ORDER BY" $ do
    it "creates sane SQL" $ run $ do
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

    it "works on an example" $ run $ do
      let people = [p1, p2, p3, p4, p5]
      mapM_ insert people
      [Value (Just ret)] <-
        select . from $ \p ->
          return (EP.arrayAggWith EP.AggModeAll (p ^. PersonName) [])
      liftIO $ L.sort ret `shouldBe` L.sort (map personName people)

  describe "DISTINCT, ORDER BY" $ do
    it "creates sane SQL" $ run $ do
      (query, args) <- showQuery ES.SELECT $ from $ \p ->
          return (EP.arrayAggWith EP.AggModeDistinct (p ^. PersonAge)
                   [asc $ p ^. PersonAge])
      liftIO $ query `shouldBe`
        "SELECT array_agg(DISTINCT \"Person\".\"age\" \
          \ORDER BY \"Person\".\"age\" ASC)\n\
        \FROM \"Person\"\n"
      liftIO $ args `shouldBe` []

    it "works on an example" $ run $ do
      let people = [p1, p2, p3, p4, p5]
      mapM_ insert people
      [Value (Just ret)] <-
        select . from $ \p ->
          return (EP.arrayAggWith EP.AggModeDistinct (p ^. PersonAge)
                   [asc $ p ^. PersonAge])
      liftIO $ ret `shouldBe` [Just 17, Just 36, Nothing]





testStringAggWith :: Spec
testStringAggWith = do
  describe "ALL, no ORDER BY" $ do
    it "creates sane SQL" $ run $ do
      (query, args) <- showQuery ES.SELECT $ from $ \p ->
            return (EP.stringAggWith EP.AggModeAll (p ^. PersonName)
                     (val " ") [])
      liftIO $ query `shouldBe`
        "SELECT string_agg(\"Person\".\"name\", ?)\n\
        \FROM \"Person\"\n"
      liftIO $ args `shouldBe` [PersistText " "]

    it "works on an example" $ run $ do
      let people = [p1, p2, p3, p4, p5]
      mapM_ insert people
      [Value (Just ret)] <-
        select . from $ \p ->
          return (EP.stringAggWith EP.AggModeAll (p ^. PersonName) (val " ")[])
      liftIO $ (L.sort $ words ret) `shouldBe` L.sort (map personName people)

    it "works with zero rows" $ run $ do
      [Value ret] <-
        select . from $ \p ->
          return (EP.stringAggWith EP.AggModeAll (p ^. PersonName) (val " ")[])
      liftIO $ ret `shouldBe` Nothing

  describe "DISTINCT, no ORDER BY" $ do
    it "creates sane SQL" $ run $ do
      (query, args) <- showQuery ES.SELECT $ from $ \p ->
            return $ EP.stringAggWith EP.AggModeDistinct (p ^. PersonName)
                     (val " ") []
      liftIO $ query `shouldBe`
        "SELECT string_agg(DISTINCT \"Person\".\"name\", ?)\n\
        \FROM \"Person\"\n"
      liftIO $ args `shouldBe` [PersistText " "]

    it "works on an example" $ run $ do
      let people = [p1, p2, p3 {personName = "John"}, p4, p5]
      mapM_ insert people
      [Value (Just ret)] <-
        select . from $ \p ->
          return $ EP.stringAggWith EP.AggModeDistinct (p ^. PersonName) (val " ")
                   []
      liftIO $ (L.sort $ words ret) `shouldBe`
        (L.sort . L.nub $ map personName people)

  describe "ALL, ORDER BY" $ do
    it "creates sane SQL" $ run $ do
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

    it "works on an example" $ run $ do
      let people = [p1, p2, p3, p4, p5]
      mapM_ insert people
      [Value (Just ret)] <-
        select . from $ \p ->
          return $ EP.stringAggWith EP.AggModeAll (p ^. PersonName) (val " ")
                    [desc $ p ^. PersonName]
      liftIO $ (words ret)
        `shouldBe` (L.reverse . L.sort $ map personName people)

  describe "DISTINCT, ORDER BY" $ do
    it "creates sane SQL" $ run $ do
      (query, args) <- showQuery ES.SELECT $ from $ \p ->
            return $ EP.stringAggWith EP.AggModeDistinct (p ^. PersonName)
                     (val " ") [desc $ p ^. PersonName]
      liftIO $ query `shouldBe`
        "SELECT string_agg(DISTINCT \"Person\".\"name\", ? \
        \ORDER BY \"Person\".\"name\" DESC)\n\
        \FROM \"Person\"\n"
      liftIO $ args `shouldBe` [PersistText " "]

    it "works on an example" $ run $ do
      let people = [p1, p2, p3 {personName = "John"}, p4, p5]
      mapM_ insert people
      [Value (Just ret)] <-
        select . from $ \p ->
          return $ EP.stringAggWith EP.AggModeDistinct (p ^. PersonName) (val " ")
                   [desc $ p ^. PersonName]
      liftIO $ (words ret) `shouldBe`
        (L.reverse . L.sort . L.nub $ map personName people)





testAggregateFunctions :: Spec
testAggregateFunctions = do
  describe "arrayAgg" $ do
    it "looks sane" $ run $ do
      let people = [p1, p2, p3, p4, p5]
      mapM_ insert people
      [Value (Just ret)] <-
        select . from $ \p -> return (EP.arrayAgg (p ^. PersonName))
      liftIO $ L.sort ret `shouldBe` L.sort (map personName people)

    it "works on zero rows" $ run $ do
      [Value ret] <-
        select . from $ \p -> return (EP.arrayAgg (p ^. PersonName))
      liftIO $ ret `shouldBe` Nothing
  describe "arrayAggWith" testArrayAggWith
  describe "stringAgg" $ do
    it "looks sane" $
      run $ do
        let people = [p1, p2, p3, p4, p5]
        mapM_ insert people
        [Value (Just ret)] <-
          select $
          from $ \p -> do
          return (EP.stringAgg (p ^. PersonName) (val " "))
        liftIO $ L.sort (words ret) `shouldBe` L.sort (map personName people)
    it "works on zero rows" $ run $ do
      [Value ret] <-
        select . from $ \p -> return (EP.stringAgg (p ^. PersonName) (val " "))
      liftIO $ ret `shouldBe` Nothing
  describe "stringAggWith" testStringAggWith

  describe "array_remove (NULL)" $ do
    it "removes NULL from arrays from nullable fields" $ run $ do
      mapM_ insert [ Person "1" Nothing   Nothing 1
                   , Person "2" (Just 7)  Nothing 1
                   , Person "3" (Nothing) Nothing 1
                   , Person "4" (Just 8)  Nothing 2
                   , Person "5" (Just 9)  Nothing 2
                   ]
      ret <- select . from $ \(person :: SqlExpr (Entity Person)) -> do
        groupBy (person ^. PersonFavNum)
        return . EP.arrayRemoveNull . EP.maybeArray . EP.arrayAgg
          $ person ^. PersonAge
      liftIO $ (L.sort $ map (L.sort . unValue) ret)
        `shouldBe` [[7], [8,9]]

  describe "maybeArray" $ do
    it "Coalesces NULL into an empty array" $ run $ do
      [Value ret] <-
        select . from $ \p ->
          return (EP.maybeArray $ EP.arrayAgg (p ^. PersonName))
      liftIO $ ret `shouldBe` []





testPostgresModule :: Spec
testPostgresModule = do
  describe "date_trunc" $ modifyMaxSuccess (`div` 10) $ do
    prop "works" $ \listOfDateParts -> run $ do
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

      liftIO $ for_ vals $ \(idx, utcTime) -> do
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
    it "chr looks sane" $
      run $ do
        [Value (ret :: String)] <- select $ return (EP.chr (val 65))
        liftIO $ ret `shouldBe` "A"

    it "allows unit for functions" $ do
      vals <- run $ do
        let
          fn :: SqlExpr (Value UTCTime)
          fn = ES.unsafeSqlFunction "now" ()
        select $ pure fn
      vals `shouldSatisfy` ((1 ==) . length)

    it "works with now" $
      run $ do
        nowDb <- select $ return EP.now_
        nowUtc <- liftIO getCurrentTime
        let oneSecond = realToFrac (1 :: Double)

        -- | Check the result is not null
        liftIO $ nowDb `shouldSatisfy` (not . null)

        -- | Unpack the now value
        let (Value now: _) = nowDb

        -- | Get the time diff and check it's less than a second
        liftIO $ diffUTCTime nowUtc now `shouldSatisfy` (< oneSecond)


--------------- JSON --------------- JSON --------------- JSON ---------------
--------------- JSON --------------- JSON --------------- JSON ---------------
--------------- JSON --------------- JSON --------------- JSON ---------------

testJSONInsertions :: Spec
testJSONInsertions =
  describe "JSON Insertions" $ do
    it "adds scalar values" $ do
      run $ do
        insertIt Null
        insertIt $ Bool True
        insertIt $ Number 1
        insertIt $ String "test"
    it "adds arrays" $ do
      run $ do
        insertIt $ toJSON ([] :: [A.Value])
        insertIt $ toJSON [Number 1, Bool True, Null]
        insertIt $ toJSON [String "test",object ["a" .= Number 3.14], Null, Bool True]
    it "adds objects" $ do
      run $ do
        insertIt $ object ["a" .= (1 :: Int), "b" .= False]
        insertIt $ object ["a" .= object ["b" .= object ["c" .= String "message"]]]
  where insertIt :: MonadIO m => A.Value -> SqlPersistT m ()
        insertIt = insert_ . Json . JSONB


testJSONOperators :: Spec
testJSONOperators =
  describe "JSON Operators" $ do
    testArrowOperators
    testFilterOperators
    testConcatDeleteOperators

testArrowOperators :: Spec
testArrowOperators =
  describe "Arrow Operators" $ do
    testArrowJSONB
    testArrowText
    testHashArrowJSONB
    testHashArrowText

testArrowJSONB :: Spec
testArrowJSONB =
  describe "Single Arrow (JSONB)" $ do
    it "creates sane SQL" $
      createSaneSQL @JSONValue
        (jsonbVal (object ["a" .= True]) ->. "a")
        "SELECT (? -> ?)\nFROM \"Json\"\n"
        [ PersistDbSpecific "{\"a\":true}"
        , PersistText "a" ]
    it "creates sane SQL (chained)" $ do
      let obj = object ["a" .= [1 :: Int,2,3]]
      createSaneSQL @JSONValue
        (jsonbVal obj ->. "a" ->. 1)
        "SELECT ((? -> ?) -> ?)\nFROM \"Json\"\n"
        [ PersistDbSpecific "{\"a\":[1,2,3]}"
        , PersistText "a"
        , PersistInt64 1 ]
    it "works as expected" $ run $ do
      x <- selectJSONwhere $ \v -> v ->. "b" ==. jsonbVal (Bool False)
      y <- selectJSONwhere $ \v -> v ->. 1 ==. jsonbVal (Bool True)
      z <- selectJSONwhere $ \v -> v ->. "a" ->. "b" ->. "c" ==. jsonbVal (String "message")
      liftIO $ length x `shouldBe` 1
      liftIO $ length y `shouldBe` 1
      liftIO $ length z `shouldBe` 1

testArrowText :: Spec
testArrowText =
  describe "Single Arrow (Text)" $ do
    it "creates sane SQL" $
      createSaneSQL
        (jsonbVal (object ["a" .= True]) ->>. "a")
        "SELECT (? ->> ?)\nFROM \"Json\"\n"
        [ PersistDbSpecific "{\"a\":true}"
        , PersistText "a" ]
    it "creates sane SQL (chained)" $ do
      let obj = object ["a" .= [1 :: Int,2,3]]
      createSaneSQL
        (jsonbVal obj ->. "a" ->>. 1)
        "SELECT ((? -> ?) ->> ?)\nFROM \"Json\"\n"
        [ PersistDbSpecific "{\"a\":[1,2,3]}"
        , PersistText "a"
        , PersistInt64 1 ]
    it "works as expected" $ run $ do
      x <- selectJSONwhere $ \v -> v ->>. "b" ==. just (val "false")
      y <- selectJSONwhere $ \v -> v ->>. 1 ==. just (val "true")
      z <- selectJSONwhere $ \v -> v ->. "a" ->. "b" ->>. "c" ==. just (val "message")
      liftIO $ length x `shouldBe` 1
      liftIO $ length y `shouldBe` 1
      liftIO $ length z `shouldBe` 1

testHashArrowJSONB :: Spec
testHashArrowJSONB =
  describe "Double Arrow (JSONB)" $ do
    it "creates sane SQL" $ do
      let list = ["a","b","c"]
      createSaneSQL @JSONValue
        (jsonbVal (object ["a" .= True]) #>. list)
        "SELECT (? #> ?)\nFROM \"Json\"\n"
        [ PersistDbSpecific "{\"a\":true}"
        , persistTextArray list ]
    it "creates sane SQL (chained)" $ do
      let obj = object ["a" .= [object ["b" .= True]]]
      createSaneSQL @JSONValue
        (jsonbVal obj #>. ["a","1"] #>. ["b"])
        "SELECT ((? #> ?) #> ?)\nFROM \"Json\"\n"
        [ PersistDbSpecific "{\"a\":[{\"b\":true}]}"
        , persistTextArray ["a","1"]
        , persistTextArray ["b"] ]
    it "works as expected" $ run $ do
      x <- selectJSONwhere $ \v -> v #>. ["a","b","c"] ==. jsonbVal (String "message")
      y <- selectJSONwhere $ \v -> v #>. ["1","a"] ==. jsonbVal (Number 3.14)
      z <- selectJSONwhere $ \v -> v #>. ["1"] #>. ["a"] ==. jsonbVal (Number 3.14)
      liftIO $ length x `shouldBe` 1
      liftIO $ length y `shouldBe` 1
      liftIO $ length z `shouldBe` 1

testHashArrowText :: Spec
testHashArrowText =
  describe "Double Arrow (Text)" $ do
    it "creates sane SQL" $ do
      let list = ["a","b","c"]
      createSaneSQL
        (jsonbVal (object ["a" .= True]) #>>. list)
        "SELECT (? #>> ?)\nFROM \"Json\"\n"
        [ PersistDbSpecific "{\"a\":true}"
        , persistTextArray list ]
    it "creates sane SQL (chained)" $ do
      let obj = object ["a" .= [object ["b" .= True]]]
      createSaneSQL
        (jsonbVal obj #>. ["a","1"] #>>. ["b"])
        "SELECT ((? #> ?) #>> ?)\nFROM \"Json\"\n"
        [ PersistDbSpecific "{\"a\":[{\"b\":true}]}"
        , persistTextArray ["a","1"]
        , persistTextArray ["b"] ]
    it "works as expected" $ run $ do
      x <- selectJSONwhere $ \v -> v #>>. ["a","b","c"] ==. just (val "message")
      y <- selectJSONwhere $ \v -> v #>>. ["1","a"] ==. just (val "3.14")
      z <- selectJSONwhere $ \v -> v #>. ["1"] #>>. ["a"] ==. just (val "3.14")
      liftIO $ length x `shouldBe` 1
      liftIO $ length y `shouldBe` 1
      liftIO $ length z `shouldBe` 1


testFilterOperators :: Spec
testFilterOperators =
  describe "Filter Operators" $ do
    testInclusion
    testQMark
    testQMarkAny
    testQMarkAll

testInclusion :: Spec
testInclusion = do
  describe "@>" $ do
    it "creates sane SQL" $
      createSaneSQL
        (jsonbVal (object ["a" .= False, "b" .= True]) @>. jsonbVal (object ["a" .= False]))
        "SELECT (? @> ?)\nFROM \"Json\"\n"
        [ PersistDbSpecific "{\"a\":false,\"b\":true}"
        , PersistDbSpecific "{\"a\":false}" ]
    it "creates sane SQL (chained)" $ do
      let obj = object ["a" .= [object ["b" .= True]]]
      createSaneSQL
        (jsonbVal obj ->. "a" @>. jsonbVal (object ["b" .= True]))
        "SELECT ((? -> ?) @> ?)\nFROM \"Json\"\n"
        [ PersistDbSpecific "{\"a\":[{\"b\":true}]}"
        , PersistText "a"
        , PersistDbSpecific "{\"b\":true}" ]
    it "works as expected" $ run $ do
      x <- selectJSONwhere $ \v -> v @>. jsonbVal (Number 1)
      y <- selectJSONwhere $ \v -> v @>. jsonbVal (toJSON [object ["a" .= Number 3.14]])
      z <- selectJSONwhere $ \v -> v ->. 1 @>. jsonbVal (object ["a" .= Number 3.14])
      liftIO $ length x `shouldBe` 2
      liftIO $ length y `shouldBe` 1
      liftIO $ length z `shouldBe` 1
  describe "<@" $ do
    it "creates sane SQL" $
      createSaneSQL
        (jsonbVal (object ["a" .= False]) <@. jsonbVal (object ["a" .= False, "b" .= True]))
        "SELECT (? <@ ?)\nFROM \"Json\"\n"
        [ PersistDbSpecific "{\"a\":false}"
        , PersistDbSpecific "{\"a\":false,\"b\":true}" ]
    it "creates sane SQL (chained)" $ do
      let obj = object ["a" .= [object ["b" .= True]]]
      createSaneSQL
        (jsonbVal obj ->. "a" <@. jsonbVal (object ["b" .= True, "c" .= Null]))
        "SELECT ((? -> ?) <@ ?)\nFROM \"Json\"\n"
        [ PersistDbSpecific "{\"a\":[{\"b\":true}]}"
        , PersistText "a"
        , PersistDbSpecific "{\"b\":true,\"c\":null}" ]
    it "works as expected" $ run $ do
      x <- selectJSONwhere $ \v -> v <@. jsonbVal (toJSON [Number 1])
      y <- selectJSONwhere $ \v -> v <@. jsonbVal (object ["a" .= (1 :: Int), "b" .= False, "c" .= Null])
      z <- selectJSONwhere $ \v -> v #>. ["a","b"] <@. jsonbVal (object ["b" .= False, "c" .= String "message"])
      liftIO $ length x `shouldBe` 2
      liftIO $ length y `shouldBe` 1
      liftIO $ length z `shouldBe` 1

testQMark :: Spec
testQMark =
  describe "Question Mark" $ do
    it "creates sane SQL" $
      createSaneSQL
        (jsonbVal (object ["a" .= False, "b" .= True]) JSON.?. "a")
        "SELECT (? ?? ?)\nFROM \"Json\"\n"
        [ PersistDbSpecific "{\"a\":false,\"b\":true}"
        , PersistText "a" ]
    it "creates sane SQL (chained)" $ do
      let obj = object ["a" .= [object ["b" .= True]]]
      createSaneSQL
        (jsonbVal obj #>. ["a","0"] JSON.?. "b")
        "SELECT ((? #> ?) ?? ?)\nFROM \"Json\"\n"
        [ PersistDbSpecific "{\"a\":[{\"b\":true}]}"
        , persistTextArray ["a","0"]
        , PersistText "b" ]
    it "works as expected" $ run $ do
      x <- selectJSONwhere (JSON.?. "a")
      y <- selectJSONwhere (JSON.?. "test")
      z <- selectJSONwhere $ \v -> v ->. "a" JSON.?. "b"
      liftIO $ length x `shouldBe` 2
      liftIO $ length y `shouldBe` 2
      liftIO $ length z `shouldBe` 1

testQMarkAny :: Spec
testQMarkAny =
  describe "Question Mark (Any)" $ do
    it "creates sane SQL" $
      createSaneSQL
        (jsonbVal (object ["a" .= False, "b" .= True]) ?|. ["a","c"])
        "SELECT (? ??| ?)\nFROM \"Json\"\n"
        [ PersistDbSpecific "{\"a\":false,\"b\":true}"
        , persistTextArray ["a","c"] ]
    it "creates sane SQL (chained)" $ do
      let obj = object ["a" .= [object ["b" .= True]]]
      createSaneSQL
        (jsonbVal obj #>. ["a","0"] ?|. ["b","c"])
        "SELECT ((? #> ?) ??| ?)\nFROM \"Json\"\n"
        [ PersistDbSpecific "{\"a\":[{\"b\":true}]}"
        , persistTextArray ["a","0"]
        , persistTextArray ["b","c"] ]
    it "works as expected" $ run $ do
      x <- selectJSONwhere (?|. ["b","test"])
      y <- selectJSONwhere (?|. ["a"])
      z <- selectJSONwhere $ \v -> v ->. (-3) ?|. ["a"]
      w <- selectJSONwhere (?|. [])
      liftIO $ length x `shouldBe` 3
      liftIO $ length y `shouldBe` 2
      liftIO $ length z `shouldBe` 1
      liftIO $ length w `shouldBe` 0

testQMarkAll :: Spec
testQMarkAll =
  describe "Question Mark (All)" $ do
    it "creates sane SQL" $
      createSaneSQL
        (jsonbVal (object ["a" .= False, "b" .= True]) ?&. ["a","c"])
        "SELECT (? ??& ?)\nFROM \"Json\"\n"
        [ PersistDbSpecific "{\"a\":false,\"b\":true}"
        , persistTextArray ["a","c"] ]
    it "creates sane SQL (chained)" $ do
      let obj = object ["a" .= [object ["b" .= True]]]
      createSaneSQL
        (jsonbVal obj #>. ["a","0"] ?&. ["b","c"])
        "SELECT ((? #> ?) ??& ?)\nFROM \"Json\"\n"
        [ PersistDbSpecific "{\"a\":[{\"b\":true}]}"
        , persistTextArray ["a","0"]
        , persistTextArray ["b","c"] ]
    it "works as expected" $ run $ do
      x <- selectJSONwhere (?&. ["test"])
      y <- selectJSONwhere (?&. ["a","b"])
      z <- selectJSONwhere $ \v -> v ->. "a" ?&. ["b"]
      w <- selectJSONwhere (?&. [])
      liftIO $ length x `shouldBe` 2
      liftIO $ length y `shouldBe` 1
      liftIO $ length z `shouldBe` 1
      liftIO $ length w `shouldBe` 9


testConcatDeleteOperators :: Spec
testConcatDeleteOperators = do
  describe "Concatenation Operator" testConcatenationOperator
  describe "Deletion Operators" $ do
    testMinusOperator
    testMinusOperatorV10
    testHashMinusOperator

testConcatenationOperator :: Spec
testConcatenationOperator =
  describe "Concatenation" $ do
    it "creates sane SQL" $
      createSaneSQL @JSONValue
        (jsonbVal (object ["a" .= False, "b" .= True])
            JSON.||. jsonbVal (object ["c" .= Null]))
        "SELECT (? || ?)\nFROM \"Json\"\n"
        [ PersistDbSpecific "{\"a\":false,\"b\":true}"
        , PersistDbSpecific "{\"c\":null}" ]
    it "creates sane SQL (chained)" $ do
      let obj = object ["a" .= [object ["b" .= True]]]
      createSaneSQL @JSONValue
        (jsonbVal obj ->. "a" JSON.||. jsonbVal (toJSON [Null]))
        "SELECT ((? -> ?) || ?)\nFROM \"Json\"\n"
        [ PersistDbSpecific "{\"a\":[{\"b\":true}]}"
        , PersistText "a"
        , PersistDbSpecific "[null]" ]
    it "works as expected" $ run $ do
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
      sqlFailWith "22023" $ selectJSONwhere $ \v ->
          v JSON.||. jsonbVal (toJSON $ String "test")
                 @>. jsonbVal (String "test")

testMinusOperator :: Spec
testMinusOperator =
  describe "Minus Operator" $ do
    it "creates sane SQL" $
      createSaneSQL @JSONValue
        (jsonbVal (object ["a" .= False, "b" .= True]) JSON.-. "a")
        "SELECT (? - ?)\nFROM \"Json\"\n"
        [ PersistDbSpecific "{\"a\":false,\"b\":true}"
        , PersistText "a" ]
    it "creates sane SQL (chained)" $ do
      let obj = object ["a" .= [object ["b" .= True]]]
      createSaneSQL @JSONValue
        (jsonbVal obj ->. "a" JSON.-. 0)
        "SELECT ((? -> ?) - ?)\nFROM \"Json\"\n"
        [ PersistDbSpecific "{\"a\":[{\"b\":true}]}"
        , PersistText "a"
        , PersistInt64 0 ]
    it "works as expected" $ run $ do
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
  where selectJSON_ f = selectJSON $ \v -> do
          where_ $ v @>. jsonbVal (object [])
               ||. v @>. jsonbVal (toJSON ([] :: [Int]))
          where_ $ f v

testMinusOperatorV10 :: Spec
testMinusOperatorV10 =
  describe "Minus Operator (PSQL >= v10)" $ do
    it "creates sane SQL" $
      createSaneSQL @JSONValue
        (jsonbVal (object ["a" .= False, "b" .= True]) --. ["a","b"])
        "SELECT (? - ?)\nFROM \"Json\"\n"
        [ PersistDbSpecific "{\"a\":false,\"b\":true}"
        , persistTextArray ["a","b"] ]
    it "creates sane SQL (chained)" $ do
      let obj = object ["a" .= [object ["b" .= True]]]
      createSaneSQL @JSONValue
        (jsonbVal obj #>. ["a","0"] --. ["b"])
        "SELECT ((? #> ?) - ?)\nFROM \"Json\"\n"
        [ PersistDbSpecific "{\"a\":[{\"b\":true}]}"
        , persistTextArray ["a","0"]
        , persistTextArray ["b"] ]
    it "works as expected" $ run $ do
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
  where selectJSON_ f = selectJSON $ \v -> do
          where_ $ v @>. jsonbVal (object [])
               ||. v @>. jsonbVal (toJSON ([] :: [Int]))
          where_ $ f v

testHashMinusOperator :: Spec
testHashMinusOperator =
  describe "Hash-Minus Operator" $ do
    it "creates sane SQL" $
      createSaneSQL @JSONValue
        (jsonbVal (object ["a" .= False, "b" .= True]) #-. ["a"])
        "SELECT (? #- ?)\nFROM \"Json\"\n"
        [ PersistDbSpecific "{\"a\":false,\"b\":true}"
        , persistTextArray ["a"] ]
    it "creates sane SQL (chained)" $ do
      let obj = object ["a" .= [object ["b" .= True]]]
      createSaneSQL @JSONValue
        (jsonbVal obj ->. "a" #-. ["0","b"])
        "SELECT ((? -> ?) #- ?)\nFROM \"Json\"\n"
        [ PersistDbSpecific "{\"a\":[{\"b\":true}]}"
        , PersistText "a"
        , persistTextArray ["0","b"] ]
    it "works as expected" $ run $ do
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

testInsertUniqueViolation :: Spec
testInsertUniqueViolation =
  describe "Unique Violation on Insert" $
    it "Unique throws exception" $ run (do
      _ <- insert u1
      _ <- insert u2
      insert u3) `shouldThrow` (==) exception
  where
    exception = SqlError {
      sqlState = "23505",
      sqlExecStatus = FatalError,
      sqlErrorMsg = "duplicate key value violates unique constraint \"UniqueValue\"",
      sqlErrorDetail = "Key (value)=(0) already exists.",
      sqlErrorHint = ""}

testUpsert :: Spec
testUpsert =
  describe "Upsert test" $ do
    it "Upsert can insert like normal" $ run $ do
      u1e <- EP.upsert u1 [OneUniqueName =. val "fifth"]
      liftIO $ entityVal u1e `shouldBe` u1
    it "Upsert performs update on collision" $ run $ do
      u1e <- EP.upsert u1 [OneUniqueName =. val "fifth"]
      liftIO $ entityVal u1e `shouldBe` u1
      u2e <- EP.upsert u2 [OneUniqueName =. val "fifth"]
      liftIO $ entityVal u2e `shouldBe` u2
      u3e <- EP.upsert u3 [OneUniqueName =. val "fifth"]
      liftIO $ entityVal u3e `shouldBe` u1{oneUniqueName="fifth"}

testInsertSelectWithConflict :: Spec
testInsertSelectWithConflict =
  describe "insertSelectWithConflict test" $ do
    it "Should do Nothing when no updates set" $ run $ do
      _ <- insert p1
      _ <- insert p2
      _ <- insert p3
      n1 <- EP.insertSelectWithConflictCount UniqueValue (
          from $ \p -> return $ OneUnique <# val "test" <&> (p ^. PersonFavNum)
        )
        (\current excluded -> [])
      uniques1 <- select $ from $ \u -> return u
      n2 <- EP.insertSelectWithConflictCount UniqueValue (
          from $ \p -> return $ OneUnique <# val "test" <&> (p ^. PersonFavNum)
        )
        (\current excluded -> [])
      uniques2 <- select $ from $ \u -> return u
      liftIO $ n1 `shouldBe` 3
      liftIO $ n2 `shouldBe` 0
      let test = map (OneUnique "test" . personFavNum) [p1,p2,p3]
      liftIO $ map entityVal uniques1 `shouldBe` test
      liftIO $ map entityVal uniques2 `shouldBe` test
    it "Should update a value if given an update on conflict" $ run $ do
        _ <- insert p1
        _ <- insert p2
        _ <- insert p3
        -- Note, have to sum 4 so that the update does not conflicts again with another row.
        n1 <- EP.insertSelectWithConflictCount UniqueValue (
            from $ \p -> return $ OneUnique <# val "test" <&> (p ^. PersonFavNum)
          )
          (\current excluded -> [OneUniqueValue =. val 4 +. (current ^. OneUniqueValue) +. (excluded ^. OneUniqueValue)])
        uniques1 <- select $ from $ \u -> return u
        n2 <- EP.insertSelectWithConflictCount UniqueValue (
            from $ \p -> return $ OneUnique <# val "test" <&> (p ^. PersonFavNum)
          )
          (\current excluded -> [OneUniqueValue =. val 4 +. (current ^. OneUniqueValue) +. (excluded ^. OneUniqueValue)])
        uniques2 <- select $ from $ \u -> return u
        liftIO $ n1 `shouldBe` 3
        liftIO $ n2 `shouldBe` 3
        let test = map (OneUnique "test" . personFavNum) [p1,p2,p3]
            test2 = map (OneUnique "test" . (+4) . (*2) . personFavNum) [p1,p2,p3]
        liftIO $ map entityVal uniques1 `shouldBe` test
        liftIO $ map entityVal uniques2 `shouldBe` test2

testFilterWhere :: Spec
testFilterWhere =
  describe "filterWhere" $ do
    it "adds a filter clause to count aggregation" $ run $ do
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

      usersByAge <- (fmap . fmap) (\(Value a, Value b, Value c) -> (a, b, c)) <$> select $ from $ \users -> do
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

      liftIO $ usersByAge `shouldMatchList`
        ( [ (Nothing, 2, 0)
          , (Just 36, 0, 1)
          , (Just 17, 2, 0)
          ] :: [(Maybe Int, Int, Int)]
        )

    it "adds a filter clause to sum aggregation" $ run $ do
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

      usersByAge <- (fmap . fmap) (\(Value a, Value b, Value c) -> (a, b, c)) <$> select $ from $ \users -> do
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

testCommonTableExpressions :: Spec
testCommonTableExpressions = do
  describe "You can run them" $ do
    it "will run" $ do
      run $ do

        void $ select $ do
          limitedLordsCte <-
            Experimental.with $ do
              lords <- Experimental.from $ Experimental.Table @Lord
              limit 10
              pure lords
          lords <- Experimental.from limitedLordsCte
          orderBy [asc $ lords ^. LordId]
          pure lords

      True `shouldBe` True

  it "can do multiple recursive queries" $ do
    vals <- run $ do
      let oneToTen = Experimental.withRecursive
                     (pure $ val (1 :: Int))
                     Experimental.unionAll_
                     (\self -> do
                         v <- Experimental.from self
                         where_ $ v <. val 10
                         pure $ v +. val 1
                     )

      select $ do
        cte <- oneToTen
        cte2 <- oneToTen
        res1 <- Experimental.from cte
        res2 <- Experimental.from cte2
        pure (res1, res2)
    vals `shouldBe` (((,) <$> fmap Value [1..10] <*> fmap Value [1..10]))

  it "passing previous query works" $
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
    in do
    vals <- run $ do

      select $ do
        cte <- oneToTen
        cte2 <- oneMore cte
        res <- Experimental.from cte2
        pure res
    vals `shouldBe` fmap Value [2..11]

-- Since lateral queries arent supported in Sqlite or older versions of mysql
-- the test is in the Postgres module
testLateralQuery :: Spec
testLateralQuery = do
  describe "Lateral queries" $ do
    it "supports CROSS JOIN LATERAL" $ do
      _ <- run $ do
        select $ do
            l :& c <-
              Experimental.from $ Table @Lord
              `CrossJoin` \lord -> do
                    deed <- Experimental.from $ Table @Deed
                    where_ $ lord ^. LordId ==. deed ^. DeedOwnerId
                    pure $ countRows @Int
            pure (l, c)
      True `shouldBe` True

    it "supports INNER JOIN LATERAL" $ do
      run $ do
        let subquery lord = do
                            deed <- Experimental.from $ Table @Deed
                            where_ $ lord ^. LordId ==. deed ^. DeedOwnerId
                            pure $ countRows @Int
        res <- select $ do
          l :& c <- Experimental.from $ Table @Lord
                          `InnerJoin` subquery
                          `Experimental.on` (const $ val True)
          pure (l, c)

        let _ = res :: [(Entity Lord, Value Int)]
        pure ()
      True `shouldBe` True

    it "supports LEFT JOIN LATERAL" $ do
      run $ do
        res <- select $ do
          l :& c <- Experimental.from $ Table @Lord
                          `LeftOuterJoin` (\lord -> do
                                    deed <- Experimental.from $ Table @Deed
                                    where_ $ lord ^. LordId ==. deed ^. DeedOwnerId
                                    pure $ countRows @Int)
                          `Experimental.on` (const $ val True)
          pure (l, c)

        let _ = res :: [(Entity Lord, Value (Maybe Int))]
        pure ()
      True `shouldBe` True

  {--
    it "compile error on RIGHT JOIN LATERAL" $ do
      run $ do
        res <- select $ do
          l :& c <- Experimental.from $ Table @Lord
                          `RightOuterJoin` (\lord -> do
                                      deed <- Experimental.from $ Table @Deed
                                      where_ $ lord ?. LordId ==. just (deed ^. DeedOwnerId)
                                      pure $ countRows @Int)
                          `Experimental.on` (const $ val True)
          pure (l, c)

        let _ = res :: [(Maybe (Entity Lord), Value Int)]
        pure ()
    it "compile error on FULL OUTER JOIN LATERAL" $ do
      run $ do
        res <- select $ do
          l :& c <- Experimental.from $ Table @Lord
                          `FullOuterJoin` (\lord -> do
                                      deed <- Experimental.from $ Table @Deed
                                      where_ $ lord ?. LordId ==. just (deed ^. DeedOwnerId)
                                      pure $ countRows @Int)
                          `Experimental.on` (const $ val True)
          pure (l, c)

        let _ = res :: [(Maybe (Entity Lord), Value (Maybe Int))]
        pure ()
    --}

type JSONValue = Maybe (JSONB A.Value)

createSaneSQL :: (PersistField a) => SqlExpr (Value a) -> T.Text -> [PersistValue] -> IO ()
createSaneSQL act q vals = run $ do
    (query, args) <- showQuery ES.SELECT $ fromValue act
    liftIO $ query `shouldBe` q
    liftIO $ args `shouldBe` vals

fromValue :: (PersistField a) => SqlExpr (Value a) -> SqlQuery (SqlExpr (Value a))
fromValue act = from $ \x -> do
    let _ = x :: SqlExpr (Entity Json)
    return act

persistTextArray :: [T.Text] -> PersistValue
persistTextArray = PersistArray . fmap PersistText

sqlFailWith :: (MonadCatch m, MonadIO m) => ByteString -> SqlPersistT (R.ResourceT m) a -> SqlPersistT (R.ResourceT m) ()
sqlFailWith errState f = do
    p <- (f >> return True) `catch` success
    when p failed
  where success SqlError{sqlState}
          | sqlState == errState = return False
          | otherwise = do
              liftIO $ expectationFailure $ T.unpack $ T.concat
                  [ "should fail with: ", errStateT
                  , ", but received: ", TE.decodeUtf8 sqlState
                  ]
              return False
        failed = liftIO $ expectationFailure $ "should fail with: " `mappend` T.unpack errStateT
        errStateT = TE.decodeUtf8 errState

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



main :: IO ()
main = do
    hspec $ do
        tests run

        describe "Test PostgreSQL locking" $ do
            testLocking withConn

        describe "PostgreSQL specific tests" $ do
            testAscRandom random_ run
            testRandomMath run
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
            describe "PostgreSQL JSON tests" $ do
                -- NOTE: We only clean the table once, so we
                -- can use its contents across all JSON tests
                it "MIGRATE AND CLEAN JSON TABLE" $ run $ do
                    void $ runMigrationSilent migrateJSON
                    cleanJSON
                testJSONInsertions
                testJSONOperators
            testLateralQuery

run, runSilent, runVerbose :: Run
runSilent  act = runNoLoggingT     $ run_worker act
runVerbose act = runStderrLoggingT $ run_worker act
run f = do
  verbose' <- lookupEnv "VERBOSE" >>= \case
    Nothing -> return verbose
    Just x | map Char.toLower x == "true" -> return True
           | null x -> return True
           | otherwise -> return False
  if verbose'
    then runVerbose f
    else runSilent f

verbose :: Bool
verbose = False

run_worker :: RunDbMonad m => SqlPersistT (R.ResourceT m) a -> m a
run_worker act = withConn $ runSqlConn (migrateIt >> act)

migrateIt :: RunDbMonad m => SqlPersistT (R.ResourceT m) ()
migrateIt = do
  void $ runMigrationSilent migrateAll
  void $ runMigrationSilent migrateUnique
  cleanDB
  cleanUniques

withConn :: RunDbMonad m => (SqlBackend -> R.ResourceT m a) -> m a
withConn =
  R.runResourceT . withPostgresqlConn "host=localhost port=5432 user=esqutest password=esqutest dbname=esqutest"

-- | Show the SQL generated by a query
showQuery :: (Monad m, ES.SqlSelect a r, BackendCompatible SqlBackend backend)
          => ES.Mode -> SqlQuery a -> ReaderT backend m (T.Text, [PersistValue])
showQuery mode query = do
  backend <- ask
  let (builder, values) = ES.toRawSql mode (backend, ES.initialIdentState) query
  return (ES.builderToText builder, values)
