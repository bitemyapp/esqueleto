{-# LANGUAGE CPP, AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.Test.Import
    ( module Common.Test.Import
    , module X
    ) where

import System.Environment
import Control.Applicative
import Common.Test.Models as X
import Database.Esqueleto.Experimental as X hiding (random_)
import Test.Hspec as X
import UnliftIO as X
import Control.Monad
import Test.QuickCheck
import Data.Text  as X (Text)
import Control.Monad.Trans.Reader as X (ReaderT, mapReaderT, ask)

type SpecDb = SpecWith ConnectionPool

asserting :: MonadIO f => IO () -> SqlPersistT f ()
asserting a = liftIO a

noExceptions :: Expectation
noExceptions = pure ()

itDb
    :: (HasCallStack)
    => String
    -> SqlPersistT IO x
    -> SpecDb
itDb message action = do
    it message $ \connection -> do
        void $ testDb connection action

propDb
    :: (HasCallStack, Testable a)
    => String
    -> ((SqlPersistT IO () -> IO ()) -> a )
    -> SpecDb
propDb message action = do
    it message $ \connection -> do
        property (action (testDb connection))

testDb :: ConnectionPool -> SqlPersistT IO a -> IO a
testDb conn action =
    liftIO $ flip runSqlPool conn $ do
        a <- action
        transactionUndo
        pure a

setDatabaseState
    :: SqlPersistT IO a
    -> SqlPersistT IO ()
    -> SpecWith ConnectionPool
    -> SpecWith ConnectionPool
setDatabaseState create clean test =
    beforeWith (\conn -> runSqlPool create conn >> pure conn) $
    after (\conn -> runSqlPool clean conn) $
    test

isCI :: IO Bool
isCI =  do
    env <- getEnvironment
    return $ case lookup "TRAVIS" env <|> lookup "CI" env of
        Just "true" -> True
        _ -> False
