{-# OPTIONS_GHC -fno-warn-unused-binds  #-}
{-# LANGUAGE FlexibleContexts
           , LambdaCase
           , OverloadedStrings
           , RankNTypes
           , ScopedTypeVariables
           , TypeFamilies
 #-}
module Main (main) where

import Control.Arrow ((&&&))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger (runStderrLoggingT, runNoLoggingT)
import Control.Monad.Trans.Reader (ReaderT, ask)
import qualified Control.Monad.Trans.Resource as R
import Data.Aeson hiding (Value)
import qualified Data.Aeson as A (Value)
import qualified Data.Char as Char
import qualified Data.List as L
import Data.Ord (comparing)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.Vector as V
import Database.Esqueleto hiding (random_)
import qualified Database.Esqueleto.Internal.Sql as ES
import Database.Esqueleto.PostgreSQL (random_)
import Database.Esqueleto.PostgreSQL.JSON hiding ((?.), (-.), (||.))
import qualified Database.Esqueleto.PostgreSQL.JSON as JSON
import qualified Database.Esqueleto.PostgreSQL   as EP
import Database.Persist.Postgresql (withPostgresqlConn)
import System.Environment
import Test.Hspec

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
  describe "PostgreSQL module" $ do
    describe "Aggregate functions" testAggregateFunctions
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
        insertIt $ Array $ V.fromList []
        insertIt $ Array $ V.fromList [Number 1, Bool True, Null]
        insertIt $ Array $ V.fromList [String "test",object ["a" .= Number 3.14]]
    it "adds objects" $ do
      run $ do
        insertIt $ object ["a" .= (1 :: Int), "b" .= False]
        insertIt $ object ["a" .= object ["b" .= object ["c" .= String "message"]]]
  where insertIt :: MonadIO m => A.Value -> SqlPersistT m ()
        insertIt = insert_ . Json


testJSONOperators :: Spec
testJSONOperators =
  describe "JSON Operators" $ do
    -- testArrowOperators
    testFilterOperators
    -- testConcatDeleteOperators

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
      createSaneSQL
        (val (Just $ object ["a" .= True]) ->. Right "a")
        "SELECT (? -> ?)\nFROM \"Json\"\n"
        [PersistDbSpecific "{\"a\":true}", PersistText "a"]
    it "creates sane SQL (chained)" $ do
      let obj = object ["a" .= [1 :: Int,2,3]]
      createSaneSQL
        (val (Just obj) ->. Right "a" ->. Left 1)
        "SELECT ((? -> ?) -> ?)\nFROM \"Json\"\n"
        [PersistDbSpecific "{\"a\":[1,2,3]}",PersistText "a",PersistInt64 1]
    it "works as expected" $ run $ do
      x <- selectJSONwhere $ \v -> v ->. Right "b" ==. val (Just $ Bool False)
      y <- selectJSONwhere $ \v -> v ->. Left 1 ==. val (Just $ Bool True)
      z <- selectJSONwhere $ \v -> not_ . isNothing $ v ->. Right "a" ->. Right "b" ->. Right "c"
      liftIO $ length x `shouldBe` 1
      liftIO $ length y `shouldBe` 1
      liftIO $ length z `shouldBe` 1

testArrowText :: Spec
testArrowText =
  describe "Single Arrow (Text)" $ do
    it "creates sane SQL" $
      createSaneSQL
        (val (Just $ object ["a" .= True]) ->>. Right "a")
        "SELECT (? ->> ?)\nFROM \"Json\"\n"
        [PersistDbSpecific "{\"a\":true}", PersistText "a"]
    it "creates sane SQL (chained)" $ do
      let obj = object ["a" .= [1 :: Int,2,3]]
      createSaneSQL
        (val (Just obj) ->. Right "a" ->>. Left 1)
        "SELECT ((? -> ?) ->> ?)\nFROM \"Json\"\n"
        [PersistDbSpecific "{\"a\":[1,2,3]}",PersistText "a",PersistInt64 1]
    it "works as expected" $ run $ do
      x <- selectJSONwhere $ \v -> v ->>. Right "b" ==. val (Just "false")
      y <- selectJSONwhere $ \v -> v ->>. Left 1 ==. val (Just "true")
      liftIO $ length x `shouldBe` 1
      liftIO $ length y `shouldBe` 1

testHashArrowJSONB :: Spec
testHashArrowJSONB =
  describe "Double Arrow (JSONB)" $ do
    it "creates sane SQL" $ do
      let list = ["a","b","c"]
      createSaneSQL
        (val (Just $ object ["a" .= True]) #>. list)
        "SELECT (? #> ?)\nFROM \"Json\"\n"
        [PersistDbSpecific "{\"a\":true}", PersistArray $ fmap PersistText list]
    it "creates sane SQL (chained)" $ do
      let obj = object ["a" .= [object ["b" .= True]]]
      createSaneSQL
        (val (Just obj) #>. ["a","1"] #>. ["b"])
        "SELECT ((? #> ?) #> ?)\nFROM \"Json\"\n"
        [ PersistDbSpecific "{\"a\":[{\"b\":true}]}"
          , PersistArray [PersistText "a", PersistText "1"]
          , PersistArray [PersistText "b"]
          ]
    it "works as expected" $ run $ do
      x <- selectJSONwhere $ \v -> v #>. ["a","b","c"] ==. val (Just $ String "message")
      y <- selectJSONwhere $ \v -> v #>. ["1","a"] ==. val (Just $ Number 3.14)
      liftIO $ length x `shouldBe` 1
      liftIO $ length y `shouldBe` 1


testHashArrowText :: Spec
testHashArrowText =
  describe "Double Arrow (Text)" $ do
    it "creates sane SQL" $ do
      let list = ["a","b","c"]
      createSaneSQL
        (val (Just $ object ["a" .= True]) #>>. list)
        "SELECT (? #>> ?)\nFROM \"Json\"\n"
        [PersistDbSpecific "{\"a\":true}", PersistArray $ fmap PersistText list]
    it "creates sane SQL (chained)" $ do
      let obj = object ["a" .= [object ["b" .= True]]]
      createSaneSQL
        (val (Just obj) #>. ["a","1"] #>>. ["b"])
        "SELECT ((? #> ?) #>> ?)\nFROM \"Json\"\n"
        [ PersistDbSpecific "{\"a\":[{\"b\":true}]}"
          , PersistArray [PersistText "a", PersistText "1"]
          , PersistArray [PersistText "b"]
          ]
    it "works as expected" $ run $ do
      x <- selectJSONwhere $ \v -> v #>>. ["a","b","c"] ==. val (Just "message")
      y <- selectJSONwhere $ \v -> v #>>. ["1","a"] ==. val (Just "3.14")
      liftIO $ length x `shouldBe` 1
      liftIO $ length y `shouldBe` 1


testFilterOperators :: Spec
testFilterOperators =
  describe "Filter Operators" $ do
    testInclusion
    -- testQMark
    -- testQMarkAny
    -- testQMarkAll

testInclusion :: Spec
testInclusion = do
  describe "@>" $ do
    it "creates sane SQL" $ do
      createSaneSQL
        (val (Just $ object ["a" .= False, "b" .= True]) @>. val (Just $ object ["a" .= False]))
        "SELECT (? @> ?)\nFROM \"Json\"\n"
        [PersistDbSpecific "{\"a\":false,\"b\":true}", PersistDbSpecific "{\"a\":false}"]
    it "creates sane SQL (chained)" $ do
      let obj = object ["a" .= [object ["b" .= True]]]
      createSaneSQL
        (val (Just obj) ->. Right "a" @>. val (Just $ object ["b" .= True]))
        "SELECT ((? -> ?) @> ?)\nFROM \"Json\"\n"
        [PersistDbSpecific "{\"a\":[{\"b\":true}]}", PersistText "a", PersistDbSpecific "{\"b\":true}"]
    it "works as expected" $ run $ do
      x <- selectJSONwhere $ \v -> v @>. val (Just $ Number 1)
      y <- selectJSONwhere $ \v -> v @>. val (Just $ Array $ V.fromList [object ["a" .= Number 3.14]])
      liftIO $ length x `shouldBe` 2
      liftIO $ length y `shouldBe` 1
  describe "<@" $ do
    it "creates sane SQL" $ do
      createSaneSQL
        (val (Just $ object ["a" .= False]) <@. val (Just $ object ["a" .= False, "b" .= True]))
        "SELECT (? <@ ?)\nFROM \"Json\"\n"
        [PersistDbSpecific "{\"a\":false}", PersistDbSpecific "{\"a\":false,\"b\":true}"]
    it "creates sane SQL (chained)" $ do
      let obj = object ["a" .= [object ["b" .= True]]]
      createSaneSQL
        (val (Just obj) ->. Right "a" <@. val (Just $ object ["b" .= True, "c" .= Null]))
        "SELECT ((? -> ?) <@ ?)\nFROM \"Json\"\n"
        [PersistDbSpecific "{\"a\":[{\"b\":true}]}", PersistText "a", PersistDbSpecific "{\"b\":true,\"c\":null}"]
    it "works as expected" $ run $ do
      x <- selectJSONwhere $ \v -> v <@. val (Just $ Array $ V.fromList [Number 1])
      y <- selectJSONwhere $ \v -> v <@. val (Just $ object ["a" .= (1 :: Int), "b" .= False, "c" .= Null])
      liftIO $ length x `shouldBe` 2
      liftIO $ length y `shouldBe` 1

{-
testQMark :: Spec
testQMark =
  describe "Question Mark" $ do

testQMarkAny :: Spec
testQMarkAny =
  describe "Question Mark (Any)" $ do

testQMarkAll :: Spec
testQMarkAll =
  describe "Question Mark (All)" $ do


testConcatDeleteOperators :: Spec
testConcatDeleteOperators = do
  describe "Concatenation Operator"
    testConcatenationOperator
  describe "Deletion Operators" $ do
    testMinusOperator
    testHashMinusOperator

testConcatenationOperator :: Spec
testConcatenationOperator =
  describe "Concatenation"

testMinusOperator :: Spec
testMinusOperator =
  describe "Minus Operator"

testHashMinusOperator :: Spec
testHashMinusOperator =
  describe "Hash-Minus Operator"

-}

createSaneSQL :: (PersistField a) => SqlExpr (Value a) -> T.Text -> [PersistValue] -> IO ()
createSaneSQL act q vals = run $ do
    (query, args) <- showQuery ES.SELECT $ fromValue act
    liftIO $ query `shouldBe` q
    liftIO $ args `shouldBe` vals

fromValue :: (PersistField a) => SqlExpr (Value a) -> SqlQuery (SqlExpr (Value a))
fromValue act = from $ \x -> do
    let _ = x :: SqlExpr (Entity Json)
    return act

selectJSONwhere
  :: MonadIO m
  => (SqlExpr (Value (Maybe A.Value)) -> SqlExpr (Value Bool))
  -> SqlPersistT m [Entity Json]
selectJSONwhere f = select $ from $ \v -> do
    where_ $ f $ just (v ^. JsonValue)
    return v
--------------- JSON --------------- JSON --------------- JSON ---------------
--------------- JSON --------------- JSON --------------- JSON ---------------
--------------- JSON --------------- JSON --------------- JSON ---------------



main :: IO ()
main = do
  hspec $ do
    -- tests run

    -- describe "Test PostgreSQL locking" $ do
      -- testLocking withConn

    describe "PostgreSQL specific tests" $ do
      -- testAscRandom random_ run
      -- testRandomMath run
      -- testSelectDistinctOn
      -- testPostgresModule
      -- testPostgresqlOneAscOneDesc
      -- testPostgresqlTwoAscFields
      -- testPostgresqlSum
      -- testPostgresqlRandom
      -- testPostgresqlUpdate
      -- testPostgresqlCoalesce
      -- testPostgresqlTextFunctions
      describe "PostgreSQL JSON tests" $ do
        -- NOTE: We only clean the table once, so we
        -- can use its contents across all JSON tests
        it "MIGRATE AND CLEAN JSON TABLE" $ run $ do
          void $ runMigrationSilent migrateJSON
          cleanJSON
        testJSONInsertions
        testJSONOperators


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
  cleanDB

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
