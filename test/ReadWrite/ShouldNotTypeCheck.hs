{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# LANGUAGE FlexibleContexts
           , ConstraintKinds
           , OverloadedStrings
           , RankNTypes
           , ScopedTypeVariables
           , TypeApplications
           , TypeFamilies
 #-}

module ReadWrite.ShouldNotTypeCheck where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logger
import Control.Monad.Trans.Reader (ReaderT(..), withReaderT)
-- import Data.Pool
import Database.Esqueleto hiding (random_)
import Database.Persist.Postgresql (withPostgresqlConn)
import Test.Hspec
import Test.ShouldNotTypecheck
import qualified Control.Monad.Trans.Resource as R
import System.IO.Unsafe
import Common.Test

readCannotWrite =
  "Could not deduce (PersistStoreWrite SqlReadBackend)"

testWriteFailsInRead :: Spec
testWriteFailsInRead =
  it "fails when we insert under a `RunRead`" $ do
    -- res <- runRead $ do
    --   void $ insert p1
    -- shouldNotTypecheck res
    shouldNotTypecheckWith readCannotWrite $ unsafePerformIO $ runRead $ do
    -- shouldNotTypecheck $ unsafePerformIO $ runRead $ do
      void $ insert p1
    -- runRead $ do
    --   void $ insert p1

  -- it "fails when we delete under a `RunRead`" $
  --   runWrite $ do
  --     void $ delete $
  --              from $ \person ->
  --              where_ (person ^. PersonId ==. val (toSqlKey 1))
  -- it "fails when we update under a `RunRead`" $
  --   runWrite $ do
  --     void $ update $ \person -> do
  --              set person [ PersonName =. val "João" ]
  --              where_ (person ^. PersonId ==. val (toSqlKey 1))

-- insert''
--   :: ( -- PersistEntityBackend record ~ SqlBackend,
--       PersistEntityBackend record ~ BaseBackend backend,
--       -- BaseBackend SqlBackend ~ SqlBackend,
--       -- BackendCompatible SqlBackend backend,
--       PersistEntity record, PersistStoreWrite backend,
--       Control.Monad.IO.Class.MonadIO m) =>
--      record -> ReaderT backend m (Key record)
-- insert'' = undefined

-- update''
--   :: (MonadIO m,
--       -- BackendCompatible SqlBackend (PersistEntityBackend val),
--       PersistEntityBackend val ~ BaseBackend backend,
--       PersistStoreWrite backend,
--       -- PersistEntity val,
--       -- PersistQueryWrite (PersistEntityBackend val),
--       -- PersistUniqueWrite (PersistEntityBackend val),
--       PersistEntity val) =>
--      (SqlExpr (Entity val) -> SqlQuery ())
--      -> ReaderT backend m ()
-- update'' = undefined

-- type SqlWithBackend backend a =
--   forall m . (BackendCompatible backend SqlBackend, MonadLogger m, RunDbMonad m) => ReaderT backend (R.ResourceT (NoLoggingT m)) a

-- runRead :: (MonadLogger m, RunDbMonad m) => SqlWithBackend SqlReadBackend a
--         -> m a
runRead :: (RunDbMonad m) => ReaderT SqlReadBackend m a -> m a
runRead act = run_worker act
-- runRead act = runDB act

  -- withConn $ runSqlConn (migrateIt >> (withReaderT projectBackend act))

-- runWrite :: (RunDbMonad m)
--          => ReaderT SqlWriteBackend (R.ResourceT m) ()
--          -> m ()
-- runWrite act = logAction $ run_worker act


-- run, runSilent, runVerbose :: Run
-- run :: (RunDbMonad m) => ReaderT backend (R.ResourceT m) a -> m a
-- run :: (RunDbMonad m) => ReaderT backend (R.ResourceT (NoLoggingT m)) a -> m a
-- run act = logAction $ run_worker act

-- logAction :: NoLoggingT m a -> m a
-- logAction :: LoggingT m a -> m a
-- logAction = runNoLoggingT -- runStderrLoggingT

type BackendConstraints backend =
  ( BackendCompatible SqlBackend backend
  , IsPersistBackend backend
  )
  -- (IsPersistBackend backend, BaseBackend backend ~ SqlBackend)
-- migrateIt :: RunDbMonad m => SqlPersistT (R.ResourceT m) ()
-- migrateIt :: ( BackendConstraints backend
--              , RunDbMonad m
--              )
--           => ReaderT backend m ()
migrateIt :: (RunDbMonad m, Functor m) => ReaderT SqlBackend m ()
-- migrateIt :: a
-- migrateIt :: ( RunDbMonad m
--              , IsSqlBackend backend
--              )
--           => ReaderT backend m ()
migrateIt = do
  void $ runMigrationSilent migrateAll
  -- cleanDB

run_worker
  :: ( -- BackendCompatible backend SqlBackend
       -- BackendCompatible SqlBackend backend
     --   IsPersistBackend backend
       RunDbMonad m
     , IsSqlBackend backend
     -- , BaseBackend backend ~ SqlBackend
     )
  => ReaderT backend m a
  -> m a
run_worker act = do
  runDB migrateIt
  runDB act
  -- runDB (migrateIt >> (withReaderT persistBackend act))
  -- runDB (migrateIt >> (withReaderT projectBackend act))
  -- (withReaderT projectBackend act))

-- withConn :: RunDbMonad m => (SqlBackend -> R.ResourceT m a) -> m a
-- withConn =
--   R.runResourceT . withPostgresqlConn "host=localhost port=5432 user=esqutest password=esqutest dbname=esqutest"

-- runDB :: SqlPersistM a -> IO a
-- runDB query =
--     R.runResourceT
--   $ runNoLoggingT
--   -- $ NoLoggingT
--   $ withPostgresqlConn "host=localhost port=5432 user=esqutest password=esqutest dbname=esqutest"
--   $ runReaderT
--   $ withReaderT persistBackend query

runDB
  :: ( RunDbMonad m
     , IsSqlBackend backend
     )
  => ReaderT backend m a
  -- :: (RunDbMonad m) => ReaderT SqlBackend m a
  -> m a
runDB action = runNoLoggingT $ NoLoggingT $
  withPostgresqlConn "host=localhost port=5432 user=esqutest password=esqutest dbname=esqutest" $ runReaderT action
