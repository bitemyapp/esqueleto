{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


import           Control.Monad           (void)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Data.Text               (Text)
import           Database.Esqueleto
import           Database.Persist.Sqlite (runMigration, runSqlite)
import           Database.Persist.TH     (mkMigrate, mkPersist,
                                          persistLowerCase, share, sqlSettings)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
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

putPersons :: (MonadIO m)
           => SqlPersistT m ()
putPersons = do
  people <- select $
              from $ \person -> do
              return person
  liftIO $ mapM_ (putStrLn . personName . entityVal) people

getJohns :: (MonadIO m)
         => SqlReadT m [Entity Person]
getJohns =
  select $
  from $ \p -> do
    where_ (p ^. PersonName ==. val "John")
    return p

getJaoas :: (MonadIO m)
         => SqlReadT m [Entity Person]
getJaoas =
  select $
  from $ \p -> do
    where_ (p ^. PersonName ==. val "João" ||. p ^. PersonName ==. val "Joao")
    return p

getAdults :: (MonadIO m)
          => SqlReadT m [Entity Person]
getAdults =
  select $
  from $ \p -> do
    where_ (p ^. PersonAge >=. just (val 18))
    return p

getBlogPostsByAuthors :: (MonadIO m)
                      => SqlReadT m [(Entity BlogPost, Entity Person)]
getBlogPostsByAuthors =
  select $
  from $ \(b, p) -> do
    where_ (b ^. BlogPostAuthorId ==. p ^. PersonId)
    orderBy [asc (b ^. BlogPostTitle)]
    return (b, p)

getAuthorMaybePosts :: (MonadIO m)
                    => SqlReadT m [(Entity Person, Maybe (Entity BlogPost))]
getAuthorMaybePosts =
  select $
  from $ \(p `LeftOuterJoin` mb) -> do
    on (just (p ^. PersonId) ==. mb ?. BlogPostAuthorId)
    orderBy [asc (p ^. PersonName), asc (mb ?. BlogPostTitle)]
    return (p, mb)

followers :: (MonadIO m)
          => SqlReadT m [(Entity Person, Entity Follow, Entity Person)]
followers =
  select $
  from $ \(p1 `InnerJoin` f `InnerJoin` p2) -> do
    on (p2 ^. PersonId ==. f ^. FollowFollowed)
    on (p1 ^. PersonId ==. f ^. FollowFollower)
    return (p1, f, p2)

updateJoao :: (MonadIO m)
           => SqlWriteT m ()
updateJoao =
  update $ \p -> do
    set p [ PersonName =. val "João" ]
    where_ (p ^. PersonName ==. val "Joao")

deleteYoungsters :: (MonadIO m)
                 => SqlWriteT m ()
deleteYoungsters =
  delete $
    from $ \p -> do
    where_ (p ^. PersonAge <. just (val 14))

insertBlogPosts :: (MonadIO m)
                => SqlWriteT m ()
insertBlogPosts =
  insertSelect $ from $ \p ->
    return $ BlogPost <# (val "Group Blog Post") <&> (p ^. PersonId)

testDb :: (MonadIO m)
       => SqlWriteT m ()
testDb = do
  john <- insert $ Person "John" (Just 24)
  sean <- insert $ Person "Seán" (Just 70)
  joao <- insert $ Person "Joao" (Just 13)
  void $ insertMany [ BlogPost "How to play a bodhrán" sean
                    , BlogPost "Haskell for the working class hero" john
                    ]
  void $ insert $ Follow john sean
  void $ insert $ Follow sean john
  void $ insert $ Follow joao sean

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i a = (f a) + fromInteger i

main :: IO ()
main = runSqlite ":memory:" $ do
  runMigration migrateAll
  testDb

  printMessage "Listing all names of the people in the database"
  printMessage "==============================================="
  putPersons
  printMessage "==============================================="

  printMessage "Listing all the people with the name John:"
  printMessage "==============================================="
  getJohns >>= printVals
  printMessage "==============================================="

  printMessage "Listing all people of the age 18 or over"
  printMessage "==============================================="
  getAdults >>= printVals
  printMessage "==============================================="

  printMessage "Listing all Blog Posts and their Authors"
  printMessage "==============================================="
  getBlogPostsByAuthors >>= printVals2
  printMessage "==============================================="

  printMessage "Listing all Authors and their possible Blog Posts"
  printMessage "==============================================="
  getAuthorMaybePosts >>= mapM_ print'
  printMessage "==============================================="

  printMessage "Listing all mutual Followers"
  printMessage "==============================================="
  followers >>= mapM_ print'
  printMessage "==============================================="

  printMessage "Updating Jaoa and checking the update"
  printMessage "==============================================="
  updateJoao
  getJaoas >>= printVals
  printMessage "==============================================="

  printMessage "Deleting poor Jaoa because he is too young"
  printMessage "==============================================="
  deleteYoungsters
  getJaoas >>= printVals
  printMessage "==============================================="
  where
    -- | Helper for print Text and getting rid of pesky warnings to default
    -- | literals to [Char]
    printMessage :: (MonadIO m) => Text -> m ()
    printMessage = liftIO . print

    -- | Helper function for printing in our DB environment
    print' :: (MonadIO m, Show a) => a -> m ()
    print' = liftIO . print

    -- | Helper to extract the entity values and print each one
    printVals = liftIO . mapM_ (print . entityVal)

    -- | TODO: Scrap this for something better
    printVals2 = liftIO . mapM_ (print . both entityVal entityVal)
    both f g (a, b) = (f a, g b)
