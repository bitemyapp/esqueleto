{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -ddump-ds -ddump-to-file -Wno-deferred-type-errors #-}

{-# LANGUAGE FlexibleContexts
           , ConstraintKinds
           , OverloadedStrings
           , RankNTypes
           , ScopedTypeVariables
           , TypeApplications
           , TypeFamilies
 #-}

module ReadWrite.ShouldNotTypeCheck where

import Control.DeepSeq
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logger
import Control.Monad.Trans.Reader (ReaderT(..), withReaderT)
-- import Data.Pool
import Database.Esqueleto hiding (random_)
import Database.Persist.Postgresql (withPostgresqlConn)
import Test.Hspec
import Test.ShouldNotTypecheck (shouldNotTypecheck, shouldNotTypecheckWith)
import qualified Control.Monad.Trans.Resource as R
import System.IO.Unsafe
import Common.Test

readCannotInsert :: String
readCannotInsert =
  "Could not deduce (PersistStoreWrite SqlReadBackend)"

readCannotUpdate :: String
readCannotUpdate =
  "Could not deduce (PersistQueryWrite SqlReadBackend)"

testWriteFailsInRead :: Spec
testWriteFailsInRead = do
  it "fails when we insert under a `RunRead`" $ do
    shouldNotTypecheckWith readCannotInsert $ unsafePerformIO $ runRead $ do
      void $ insert p1

  -- it "fails when we delete under a `RunRead`" $
  --   shouldNotTypecheck $ unsafePerformIO $ do
  --     res <- runRead $ do
  --       void $ delete $
  --                from $ \person ->
  --                where_ (person ^. PersonId ==. val (toSqlKey 1))
  --     deepseq res (return ())

  it "fails when we update under a `RunRead`" $
    shouldNotTypecheckWith readCannotUpdate $ unsafePerformIO $ do
      -- We have to force the underlying type error
      -- or it erroneously succeeds. Buy me (@bitemyapp)
      -- a beer and I'll explain in more detail.
      runRead $ do
        let q = update $ \person -> do
                  set person [ PersonName =. val "João" ]
                  where_ (person ^. PersonId ==. val (toSqlKey 1))
        deepseq q (return ())
      -- deepseq res (return ())

type DBM = NoLoggingT (R.ResourceT IO)

runRead :: ReaderT SqlReadBackend DBM a
        -> IO a
runRead act = run_worker act

-- runRead act = runNoLoggingT $ run_worker act

runWrite :: ReaderT SqlWriteBackend DBM a -> IO a
runWrite act = run_worker act

-- runWrite :: (RunDbMonad m) => ReaderT SqlWriteBackend m a -> m a
-- runWrite act = run_worker act

-- migrateIt :: ( RunDbMonad m
--              , Functor m
--              )
--           => ReaderT SqlBackend m ()
-- migrateIt = do
--   void $ runMigrationSilent migrateAll

migrateIt :: ReaderT SqlBackend DBM ()
migrateIt = do
  void $ runMigrationSilent migrateAll

run_worker
  :: ( -- RunDbMonad m
     -- , LogIO m
       IsSqlBackend backend
     , BackendCompatible SqlBackend backend
     )
  => ReaderT backend DBM a
  -> IO a
run_worker act = do
  runDB $ do
    withReaderT projectBackend migrateIt
    act

-- run_worker
--   :: ( RunDbMonad m
--      -- , LogIO m
--      , IsSqlBackend backend
--      , BackendCompatible SqlBackend backend
--      )
--   => ReaderT backend m a
--   -> m a
-- run_worker act = do
--   runDB $ do
--     withReaderT projectBackend migrateIt
--     act


runDB
  :: ( -- RunDbMonad m
     -- , LogIO m
       IsSqlBackend backend
     )
  => ReaderT backend DBM a
  -> IO a
runDB action = R.runResourceT $ runNoLoggingT $
  withPostgresqlConn "host=localhost port=5432 user=esqutest password=esqutest dbname=esqutest" $ runReaderT action

-- runDB
--   :: ( RunDbMonad m
--      -- , LogIO m
--      , IsSqlBackend backend
--      )
--   => ReaderT backend m a
--   -> m a
-- runDB action =
--   withPostgresqlConn "host=localhost port=5432 user=esqutest password=esqutest dbname=esqutest" $ runReaderT action

  -- -- R.runResourceT $
  --   -- runNoLoggingT $
  --   withPostgresqlPool "host=localhost port=5432 user=esqutest password=esqutest dbname=esqutest" 1
  -- $ \pool -> liftIO $ runSqlPersistMPool action pool

  -- runNoLoggingT $
  --   withPostgresqlPool devConn 3
  --     $ \pool -> liftIO $ runSqlPersistMPool a pool
