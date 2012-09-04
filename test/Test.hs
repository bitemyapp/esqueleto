{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, GADTs, FlexibleContexts, EmptyDataDecls #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Database.Esqueleto
import Database.Persist.Sqlite
import Database.Persist.TH
import Test.Hspec
import Test.Hspec.Expectations
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


runDB :: SqlPersist (C.ResourceT IO) a -> IO a
runDB =
  C.runResourceT .
  withSqliteConn ":memory:" .
  runSqlConn .
  (runMigrationSilent migrateAll >>)


main :: IO ()
main = do
  let p1 = Person "John" (Just 36)
      p2 = Person "Rachel" Nothing
      p3 = Person "Mike" (Just 17)
  hspec $ do
    describe "select" $ do
      it "works for single value" $
        runDB $ do
          ret <- select $ return $ val (3 :: Int)
          liftIO $ ret `shouldBe` [ Single 3 ]
    describe "select/from" $ do
      it "works on a most simple example" $
        runDB $ do
          p1k <- insert p1
          ret <- select $
                 from $ \person ->
                 return person
          liftIO $ ret `shouldBe` [ Entity p1k p1 ]
