{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Main
    ( main
    ) where

-------------------------------------------------------------------------------
import           Control.Monad               (void)
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger        (MonadLogger, NoLoggingT (..))
import           Control.Monad.Reader
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..),
                                              defaultLiftBaseWith,
                                              defaultRestoreM)
import           Data.Text                   (Text)
import           Database.Esqueleto
import           Database.Persist.Postgresql (ConnectionString,
                                              withPostgresqlConn)
import           Database.Persist.TH         (mkDeleteCascade, mkMigrate,
                                              mkPersist, persistLowerCase,
                                              share, sqlSettings)
import           Lens.Micro                  ((&), (.~))
-------------------------------------------------------------------------------


share [mkPersist sqlSettings, mkDeleteCascade sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
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


-------------------------------------------------------------------------------
putPersons :: (MonadIO m, MonadLogger m)
           => SqlPersistT m ()
putPersons = do
  people <- select $
              from $ \person -> do
              return person
  liftIO $ mapM_ (putStrLn . ("Name: " ++) . personName . entityVal) people


-------------------------------------------------------------------------------
getJohns :: (MonadIO m, MonadLogger m)
         => SqlReadT m [Entity Person]
getJohns =
  select $
  from $ \p -> do
    where_ (p ^. PersonName ==. val "John")
    return p


-------------------------------------------------------------------------------
getJaoas :: (MonadIO m, MonadLogger m)
         => SqlReadT m [Entity Person]
getJaoas =
  select $
  from $ \p -> do
    where_ (p ^. PersonName ==. val "João" ||. p ^. PersonName ==. val "Joao")
    return p


-------------------------------------------------------------------------------
getAdults :: (MonadIO m, MonadLogger m)
          => SqlReadT m [Entity Person]
getAdults =
  select $
  from $ \p -> do
    where_ (p ^. PersonAge >=. just (val 18))
    return p


-------------------------------------------------------------------------------
getBlogPostsByAuthors :: (MonadIO m, MonadLogger m)
                      => SqlReadT m [(Entity BlogPost, Entity Person)]
getBlogPostsByAuthors =
  select $
  from $ \(b, p) -> do
    where_ (b ^. BlogPostAuthorId ==. p ^. PersonId)
    orderBy [asc (b ^. BlogPostTitle)]
    return (b, p)


-------------------------------------------------------------------------------
getAuthorMaybePosts :: (MonadIO m, MonadLogger m)
                    => SqlReadT m [(Entity Person, Maybe (Entity BlogPost))]
getAuthorMaybePosts =
  select $
  from $ \(p `LeftOuterJoin` mb) -> do
    on (just (p ^. PersonId) ==. mb ?. BlogPostAuthorId)
    orderBy [asc (p ^. PersonName), asc (mb ?. BlogPostTitle)]
    return (p, mb)


-------------------------------------------------------------------------------
followers :: (MonadIO m, MonadLogger m)
          => SqlReadT m [(Entity Person, Entity Follow, Entity Person)]
followers =
  select $
  from $ \(p1 `InnerJoin` f `InnerJoin` p2) -> do
    on (p2 ^. PersonId ==. f ^. FollowFollowed)
    on (p1 ^. PersonId ==. f ^. FollowFollower)
    return (p1, f, p2)


-------------------------------------------------------------------------------
updateJoao :: (MonadIO m, MonadLogger m)
           => SqlWriteT m ()
updateJoao =
  update $ \p -> do
    set p [ PersonName =. val "João" ]
    where_ (p ^. PersonName ==. val "Joao")


-------------------------------------------------------------------------------
deleteYoungsters :: (MonadIO m, MonadLogger m)
                 => SqlWriteT m ()
deleteYoungsters = do
  delete $
    from $ \p -> do
    where_ (p ^. PersonAge <. just (val 14))


-------------------------------------------------------------------------------
insertBlogPosts :: (MonadIO m, MonadLogger m)
                => SqlWriteT m ()
insertBlogPosts =
  insertSelect $ from $ \p ->
    return $ BlogPost <# (val "Group Blog Post") <&> (p ^. PersonId)


-------------------------------------------------------------------------------
cleanDB :: (MonadIO m, MonadLogger m)
        => SqlPersistT m ()
cleanDB = do
  rawExecute "DROP TABLE follow" []
  rawExecute "DROP TABLE blog_post" []
  rawExecute "DROP TABLE person" []

testDb :: (MonadIO m, MonadLogger m)
       => SqlPersistT m ()
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


-------------------------------------------------------------------------------
newtype BlogT m a = BlogT { unBlogT :: NoLoggingT (ReaderT ConnectionString m) a }
  deriving (Functor, Applicative, Monad, MonadLogger, MonadReader ConnectionString, MonadIO)

instance MonadTrans BlogT where
  lift = BlogT . lift . lift

deriving instance (MonadBase b m) => MonadBase b (BlogT m)

instance MonadBaseControl b m => MonadBaseControl b (BlogT m) where
  type StM (BlogT m) a = ComposeSt BlogT m a
  liftBaseWith     = defaultLiftBaseWith
  restoreM         = defaultRestoreM

instance MonadTransControl BlogT where
  type StT BlogT a = StT NoLoggingT (StT (ReaderT ConnectionString) a)
  liftWith f = BlogT $ liftWith $ \run ->
                                    liftWith $ \run' ->
                                                f (run' . run . unBlogT)
  restoreT = BlogT . restoreT . restoreT


runBlogT :: ConnectionString -> BlogT m a -> m a
runBlogT backend (BlogT m) =
  runReaderT (runNoLoggingT m) backend

type Blog a = BlogT IO a

runDB :: (MonadReader ConnectionString m, MonadIO m, MonadBaseControl IO m, Monad m, MonadLogger m)
      => SqlPersistT m a -> m a
runDB query = do
  conn <- ask
  withPostgresqlConn conn $ \backend -> runReaderT query backend

setupBlog :: (MonadIO m, MonadLogger m)
          => SqlPersistT m ()
setupBlog = do
  runMigration migrateAll
  testDb

main :: IO ()
main =
  let connection = "host=localhost port=5433 user=postgres dbname=esqueleto_blog_example password=***"
  in runBlogT connection . runDB $ do
    setupBlog
    putPersons
    cleanDB
