{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- Tests for `Database.Esqueleto.Record`.
module Common.Record (testDeriveEsqueletoRecord) where

import Common.Test.Import hiding (from, on)
import Data.List (sortOn)
import Database.Esqueleto.Experimental
import Database.Esqueleto.Record (deriveEsqueletoRecord)

data MyRecord =
    MyRecord
        { myName :: Text
        , myAge :: Maybe Int
        , myUser :: Entity User
        , myAddress :: Maybe (Entity Address)
        }
  deriving (Show, Eq)

$(deriveEsqueletoRecord ''MyRecord)

data MyNestedRecord =
    MyNestedRecord
        { myName :: Text
        , myRecord :: MyRecord
        }
  deriving (Show, Eq)

$(deriveEsqueletoRecord ''MyNestedRecord)

myRecordQuery :: SqlQuery SqlMyRecord
myRecordQuery = do
  user :& address <- from $
    table @User
      `leftJoin`
      table @Address
      `on` (do \(user :& address) -> user ^. #address ==. address ?. #id)
  pure
    SqlMyRecord
      { myName = castString $ user ^. #name
      , myAge = val $ Just 10
      , myUser = user
      , myAddress = address
      }

testDeriveEsqueletoRecord :: SpecDb
testDeriveEsqueletoRecord = describe "deriveEsqueletoRecord" $ do
    let setup :: MonadIO m => SqlPersistT m ()
        setup = do
          _ <- insert $ User { userAddress = Nothing, userName = "Rebecca" }
          addr <- insert $ Address { addressAddress = "30-50 Feral Hogs Rd" }
          _ <- insert $ User { userAddress = Just addr, userName = "Some Guy" }
          pure ()

    itDb "can select records" $ do
        setup
        records <- select myRecordQuery
        let sortedRecords = sortOn (\MyRecord {myName} -> myName) records
        liftIO $ sortedRecords !! 0
          `shouldSatisfy`
          (\case MyRecord { myName = "Rebecca"
                          , myAge = Just 10
                          , myUser = Entity _ User { userAddress  = Nothing
                                                   , userName = "Rebecca"
                                                   }
                          , myAddress = Nothing
                          } -> True
                 _ -> False)
        liftIO $ sortedRecords !! 1
          `shouldSatisfy`
          (\case MyRecord { myName = "Some Guy"
                          , myAge = Just 10
                          , myUser = Entity _ User { userAddress  = Just addr1
                                                   , userName = "Some Guy"
                                                   }
                          , myAddress = Just (Entity addr2 Address {addressAddress = "30-50 Feral Hogs Rd"})
                          } -> addr1 == addr2 -- The keys should match.
                 _ -> False)
