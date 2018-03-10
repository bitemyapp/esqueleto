{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE ScopedTypeVariables
           , FlexibleContexts
           , RankNTypes
           , TypeFamilies
           , OverloadedStrings
 #-}
module Main (main) where

import Control.Arrow ((&&&))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger (runStderrLoggingT, runNoLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Ord (comparing)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Database.Esqueleto hiding (random_)
import Database.Esqueleto.PostgreSQL (random_)
import Database.Persist.Postgresql (withPostgresqlConn)
import Test.Hspec
import qualified Control.Monad.Trans.Resource as R
import qualified Data.List as L
import qualified Database.Esqueleto.PostgreSQL as EP

import Common.Test
import ReadWrite.ShouldNotTypeCheck (testWriteFailsInRead)

main :: IO ()
main = do
  hspec $ do
    describe "Read specific tests" $ do
      testWriteFailsInRead
    -- describe "Write specific tests" $ do
    --   undefined

-- run, runSilent, runVerbose :: Run
-- runSilent  act = runNoLoggingT     $ run_worker act
-- runVerbose act = runStderrLoggingT $ run_worker act
-- run =
--   if verbose
--   then runVerbose
--   else runSilent


-- verbose :: Bool
-- verbose = False

-- migrateIt :: RunDbMonad m => SqlPersistT (R.ResourceT m) ()
-- migrateIt = do
--   void $ runMigrationSilent migrateAll
--   cleanDB

-- run_worker :: RunDbMonad m => SqlPersistT (R.ResourceT m) a -> m a
-- run_worker act = withConn $ runSqlConn (migrateIt >> act)

-- withConn :: RunDbMonad m => (SqlBackend -> R.ResourceT m a) -> m a
-- withConn =
--   R.runResourceT . withPostgresqlConn "host=localhost port=5432 user=esqutest password=esqutest dbname=esqutest"
