{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Main
    ( main
    ) where

import Blog
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger, MonadLoggerIO)
import Control.Monad.Reader (MonadReader(..), runReaderT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Database.Esqueleto.Experimental
import Database.Persist.Postgresql (ConnectionString, withPostgresqlConn)
import qualified Database.Persist.Sql as Persistent
import Database.Persist.TH
       ( mkMigrate
       , mkPersist
       , persistLowerCase
       , share
       , sqlSettings
       )

share [ mkPersist sqlSettings
      , mkMigrate "migrateAll"] [persistLowerCase|
  Person
    name String
    age Int Maybe
    deriving Eq Show
  BlogPost
    title String
    authorId PersonId OnDeleteCascade
    deriving Eq Show
  Follow
    follower PersonId OnDeleteCascade
    followed PersonId OnDeleteCascade
    deriving Eq Show
|]

putPersons :: (MonadIO m, MonadLogger m)
           => SqlPersistT m ()
putPersons = do
  -- | Select all values from the `person` table
  people <- select $ from $ table @Person

  -- | entityVal extracts the Person value, which we then extract
  -- | the person name from the record and print it
  liftIO $ mapM_ (putStrLn . ("Name: " ++) . personName . entityVal) people


getJohns :: (MonadIO m, MonadLogger m)
         => SqlReadT m [Entity Person]
getJohns =
  -- | Select all persons where their name is equal to "John"
  select $ do
  people <- from $ table @Person
  where_ (people ^. PersonName ==. val "John")
  return people


getAdults :: (MonadIO m, MonadLogger m)
          => SqlReadT m [Entity Person]
getAdults =
  -- | Select any Person where their age is >= 18 and NOT NULL
  select $ do
  people <- from $ table @Person
  where_ (people ^. PersonAge >=. just (val 18))
  return people


getBlogPostsByAuthors :: (MonadIO m, MonadLogger m)
                      => SqlReadT m [(Entity BlogPost, Entity Person)]
getBlogPostsByAuthors =
  -- | Select all persons and their blogposts, ordering by title
  select $ do
  (people :& blogPosts) <-
      from $ table @Person
      `innerJoin` table @BlogPost
      `on` (\(people :& blogPosts) ->
              people ^. PersonId ==. blogPosts ^. BlogPostAuthorId)
  orderBy [asc (blogPosts ^. BlogPostTitle)]
  pure (blogPosts, people)


getAuthorMaybePosts :: (MonadIO m, MonadLogger m)
                    => SqlReadT m [(Entity Person, Maybe (Entity BlogPost))]
getAuthorMaybePosts =
  -- | Select all persons doing a left outer join on blogposts
  -- | Since a person may not have any blogposts the BlogPost Entity is wrapped
  -- | in a Maybe
  select $ do
    p :& mb <- from $
      table @Person
      `leftJoin`
      table @BlogPost
      `on` (do
        \(p :& mb) -> just (p ^. PersonId) ==. mb ?. BlogPostAuthorId)
    orderBy [asc (p ^. PersonName), asc (mb ?. BlogPostTitle)]
    return (p, mb)


followers :: (MonadIO m, MonadLogger m)
          => SqlReadT m [(Entity Person, Entity Follow, Entity Person)]
followers =
  -- | Select mutual follow relationships
  -- | Note carefully that the order of the ON clauses is reversed!
  -- | You're required to write your ons in reverse order because that helps composability
  -- | (see the documentation of on for more details).
  select $ do
    p1 :& f :& p2 <- from $
      table @Person
      `innerJoin`
      table @Follow
      `on` (\(p1 :& f) -> p1 ^. PersonId ==. f ^. FollowFollowed)
      `innerJoin`
      table @Person
      `on` (\(_ :& f :& p2) -> f ^. FollowFollower ==. p2 ^. PersonId)
    pure (p1, f, p2)

  -- from $ \(p1 `InnerJoin` f `InnerJoin` p2) -> do
    -- on (p2 ^. PersonId ==. f ^. FollowFollowed)
    -- on (p1 ^. PersonId ==. f ^. FollowFollower)
    -- return (p1, f, p2)


updateJoao :: (MonadIO m, MonadLogger m)
           => SqlWriteT m ()
updateJoao =
  -- Update the name of any Joao in our person table to João
  update $ \p -> do
    set p [ PersonName =. val "João" ]
    where_ (p ^. PersonName ==. val "Joao")


deleteYoungsters :: (MonadIO m, MonadLogger m)
                 => SqlPersistT m ()
deleteYoungsters = do
  -- | Delete any persons under the age of 14
  delete $ do
    p <- from $ table @Person
    where_ (p ^. PersonAge <. just (val 14))


insertBlogPosts :: (MonadIO m, MonadLogger m)
                => SqlWriteT m ()
insertBlogPosts =
  -- | Insert a new blogpost for every person
  insertSelect $ do
    p <- from $ table @Person
    return $ BlogPost <# (val "Group Blog Post") <&> (p ^. PersonId)


runDB :: (MonadReader ConnectionString m,
          MonadIO m,
          MonadBaseControl IO m,
          MonadUnliftIO m,
          MonadLoggerIO m,
          MonadLogger m)
      => SqlPersistT m a -> m a
runDB query = do
  -- | Helper for running a query
  conn <- ask
  withPostgresqlConn conn $ \backend -> runReaderT query backend


setupDb :: (MonadIO m, MonadLogger m)
          => SqlPersistT m ()
setupDb = do
  -- | Run migrations and create the test database entries
  runMigration migrateAll
  createDb
  where
    createDb :: (MonadIO m, MonadLogger m)
           => SqlPersistT m ()
    createDb = do
      john <- insert $ Person "John" (Just 24)
      sean <- insert $ Person "Seán" (Just 70)
      joao <- insert $ Person "Joao" (Just 13)
      void $ insertMany [ BlogPost "How to play a bodhrán" sean
                        , BlogPost "Haskell for the working class hero" john
                        ]
      void $ insert $ Follow john sean
      void $ insert $ Follow sean john
      void $ insert $ Follow joao sean


cleanDb :: (MonadIO m, MonadLogger m)
        => SqlPersistT m ()
cleanDb = do
  -- | Drop the tables so we can re-run the script again if needed
  dropTable "follow"
  dropTable "blog_post"
  dropTable "person"
  where
    dropTable tableName = rawExecute ("DROP TABLE " <> tableName) []

main :: IO ()
main = do
  -- Connection string for the postrgesql database
  runBlogT connection . runDB $ do
    setupDb
    putPersons

    johns <- getJohns
    mapM_ say johns

    adults <- getAdults
    mapM_ say adults

    authorBlogPosts <- getBlogPostsByAuthors
    mapM_ say authorBlogPosts

    authoMaybePosts <- getAuthorMaybePosts
    mapM_ say authoMaybePosts

    mutualFollowers <- followers
    mapM_ say mutualFollowers

    updateJoao
    deleteYoungsters
    insertBlogPosts
    cleanDb
  where
    say :: (MonadIO m, Show a) => a -> m ()
    say = liftIO . print
    connection = "host=localhost port=5432 user=postgres dbname=esqueleto_blog_example password=***"
