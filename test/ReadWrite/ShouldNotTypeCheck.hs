-- {-# OPTIONS_GHC -fdefer-type-errors #-}
{-# LANGUAGE FlexibleContexts
           , OverloadedStrings
           , RankNTypes
           , ScopedTypeVariables
           , TypeApplications
           , TypeFamilies
 #-}

module ReadWrite.ShouldNotTypeCheck where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (runStderrLoggingT, runNoLoggingT)
import Control.Monad.Trans.Reader (ReaderT, withReaderT)
import Database.Esqueleto hiding (random_)
import Database.Persist.Postgresql (withPostgresqlConn)
import Test.Hspec
import Test.ShouldNotTypecheck (shouldNotTypecheck)
import qualified Control.Monad.Trans.Resource as R

import Common.Test

testWriteFailsInRead :: Spec
testWriteFailsInRead =
  it "fails when we insert under a `RunRead`" $ do
    res <- runRead $ do
      void $ insert p1
    shouldNotTypecheck res
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

runRead :: RunRead
runRead = run

runWrite :: RunWrite
runWrite = run

run, runSilent, runVerbose :: Run
runSilent  act = runNoLoggingT     $ run_worker act
runVerbose act = runStderrLoggingT $ run_worker act
run =
  if verbose
  then runVerbose
  else runSilent

verbose :: Bool
verbose = False

migrateIt :: RunDbMonad m => SqlPersistT (R.ResourceT m) ()
migrateIt = do
  void $ runMigrationSilent migrateAll
  -- cleanDB

run_worker
  :: ( BackendCompatible backend SqlBackend
     , RunDbMonad m
     )
  => ReaderT backend (R.ResourceT m) a -> m a
run_worker act =
  withConn $ runSqlConn (migrateIt >> (withReaderT projectBackend act))

withConn :: RunDbMonad m => (SqlBackend -> R.ResourceT m a) -> m a
withConn =
  R.runResourceT . withPostgresqlConn "host=localhost port=5432 user=esqutest password=esqutest dbname=esqutest"
