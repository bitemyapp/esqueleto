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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- Tests for `Database.Esqueleto.Record`.
module Common.Record (testDeriveEsqueletoRecord) where

import Common.Test.Import hiding (from, on)
import Control.Monad.Trans.State.Strict (StateT(..), evalStateT)
import Data.Bifunctor (first)
import Data.List (sortOn)
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy(..))
import Database.Esqueleto.Experimental
import Database.Esqueleto.Internal.Internal (SqlSelect(..))
import Database.Esqueleto.Record (
  DeriveEsqueletoRecordSettings(..),
  defaultDeriveEsqueletoRecordSettings,
  deriveEsqueletoRecord,
  deriveEsqueletoRecordWith,
  takeColumns,
  takeMaybeColumns,
 )
import GHC.Records

data MyRecord =
    MyRecord
        { myName :: Text
        , myAge :: Maybe Int
        , myUser :: Entity User
        , myAddress :: Maybe (Entity Address)
        }
  deriving (Show, Eq)

$(deriveEsqueletoRecord ''MyRecord)

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

data MyNestedRecord = MyNestedRecord
  { myName :: Text
  , myRecord :: MyRecord
  }
  deriving (Show, Eq)

$(deriveEsqueletoRecord ''MyNestedRecord)

myNestedRecordQuery :: SqlQuery SqlMyNestedRecord
myNestedRecordQuery = do
  user :& address <-
    from $
      table @User
        `leftJoin` table @Address
        `on` (do \(user :& address) -> user ^. #address ==. address ?. #id)
  pure
    SqlMyNestedRecord
      { myName = castString $ user ^. #name
      , myRecord =
          SqlMyRecord
            { myName = castString $ user ^. #name
            , myAge = val $ Just 10
            , myUser = user
            , myAddress = address
            }
      }

data MyModifiedRecord =
    MyModifiedRecord
        { myModifiedName :: Text
        , myModifiedAge :: Maybe Int
        , myModifiedUser :: Entity User
        , myModifiedAddress :: Maybe (Entity Address)
        }
  deriving (Show, Eq)

$(deriveEsqueletoRecordWith (defaultDeriveEsqueletoRecordSettings
    { sqlNameModifier = (++ "Sql")
    , sqlFieldModifier = (++ "Sql")
    })
    ''MyModifiedRecord)

myModifiedRecordQuery :: SqlQuery MyModifiedRecordSql
myModifiedRecordQuery = do
  user :& address <- from $
    table @User
      `leftJoin`
      table @Address
      `on` (do \(user :& address) -> user ^. #address ==. address ?. #id)
  pure
    MyModifiedRecordSql
      { myModifiedNameSql = castString $ user ^. #name
      , myModifiedAgeSql = val $ Just 10
      , myModifiedUserSql = user
      , myModifiedAddressSql = address
      }

mySubselectRecordQuery :: SqlQuery (SqlExpr (Maybe (Entity Address)))
mySubselectRecordQuery = do
  _ :& record <- from $
    table @User
      `leftJoin`
      myRecordQuery
      `on` (do \(user :& record) -> just (user ^. #id) ==. getField @"myUser" record ?. #id)
  pure $ getField @"myAddress" record

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

    itDb "can select nested records" $ do
        setup
        records <- select myNestedRecordQuery
        let sortedRecords = sortOn (\MyNestedRecord {myName} -> myName) records
        liftIO $ sortedRecords !! 0
          `shouldSatisfy`
          (\case MyNestedRecord
                   { myName = "Rebecca"
                   , myRecord =
                       MyRecord { myName = "Rebecca"
                                , myAge = Just 10
                                , myUser = Entity _ User { userAddress  = Nothing
                                                         , userName = "Rebecca"
                                                         }
                                , myAddress = Nothing
                                }
                   } -> True
                 _ -> False)

        liftIO $ sortedRecords !! 1
          `shouldSatisfy`
          (\case MyNestedRecord
                   { myName = "Some Guy"
                   , myRecord =
                       MyRecord { myName = "Some Guy"
                                , myAge = Just 10
                                , myUser = Entity _ User { userAddress  = Just addr1
                                                         , userName = "Some Guy"
                                                         }
                                , myAddress = Just (Entity addr2 Address {addressAddress = "30-50 Feral Hogs Rd"})
                                }
                   } -> addr1 == addr2 -- The keys should match.
                 _ -> False)

    itDb "can be used in a CTE" $ do
        setup
        records <- select $ do
            recordCTE <- with myRecordQuery
            record <- from recordCTE
            pure record
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

    itDb "can select user-modified records" $ do
        setup
        records <- select myModifiedRecordQuery
        let sortedRecords = sortOn (\MyModifiedRecord {myModifiedName} -> myModifiedName) records
        liftIO $ sortedRecords !! 0
          `shouldSatisfy`
          (\case MyModifiedRecord
                  { myModifiedName = "Rebecca"
                  , myModifiedAge = Just 10
                  , myModifiedUser = Entity _ User { userAddress  = Nothing
                                           , userName = "Rebecca"
                                           }
                  , myModifiedAddress = Nothing
                  } -> True
                 _ -> False)
        liftIO $ sortedRecords !! 1
          `shouldSatisfy`
          (\case MyModifiedRecord
                    { myModifiedName = "Some Guy"
                    , myModifiedAge = Just 10
                    , myModifiedUser = Entity _ User { userAddress  = Just addr1
                                             , userName = "Some Guy"
                                             }
                    , myModifiedAddress = Just (Entity addr2 Address {addressAddress = "30-50 Feral Hogs Rd"})
                    } -> addr1 == addr2 -- The keys should match.
                 _ -> False)

    itDb "can left join on records" $ do
        setup
        records <- select $ do
          from
            ( table @User
                `leftJoin` myRecordQuery `on` (do \(user :& record) -> just (user ^. #id) ==. getField @"myUser" record ?. #id)
            )
        let sortedRecords = sortOn (\(Entity _ user :& _) -> getField @"userName" user) records
        liftIO $ sortedRecords !! 0
          `shouldSatisfy`
          (\case (_ :& Just (MyRecord {myName = "Rebecca", myAddress = Nothing})) -> True
                 _ -> False)
        liftIO $ sortedRecords !! 1
          `shouldSatisfy`
          (\case ( _ :& Just ( MyRecord { myName = "Some Guy"
                                        , myAddress = (Just (Entity addr2 Address {addressAddress = "30-50 Feral Hogs Rd"}))
                                        }
                              )) -> True
                 _ -> False)

    itDb "can can handle joins on records with Nothing" $ do
        setup
        records <- select $ do
          from
            ( table @User
                `leftJoin` myRecordQuery `on` (do \(user :& record) -> user ^. #address ==. getField @"myAddress" record ?. #id)
            )
        let sortedRecords = sortOn (\(Entity _ user :& _) -> getField @"userName" user) records
        liftIO $ sortedRecords !! 0
          `shouldSatisfy`
          (\case (_ :& Nothing) -> True
                 _ -> False)
        liftIO $ sortedRecords !! 1
          `shouldSatisfy`
          (\case ( _ :& Just ( MyRecord { myName = "Some Guy"
                                        , myAddress = (Just (Entity addr2 Address {addressAddress = "30-50 Feral Hogs Rd"}))
                                        }
                              )) -> True
                 _ -> False)

    itDb "can left join on nested records" $ do
        setup
        records <- select $ do
          from
            ( table @User
                `leftJoin` myNestedRecordQuery
                `on` (do \(user :& record) -> just (user ^. #id) ==. getField @"myUser" (getField @"myRecord" record) ?. #id)
            )
        let sortedRecords = sortOn (\(Entity _ user :& _) -> getField @"userName" user) records
        liftIO $ sortedRecords !! 0
          `shouldSatisfy`
          (\case (_ :& Just (MyNestedRecord {myRecord = MyRecord {myName = "Rebecca", myAddress = Nothing}})) -> True
                 _ -> False)
        liftIO $ sortedRecords !! 1
          `shouldSatisfy`
          (\case ( _ :& Just ( MyNestedRecord { myRecord = MyRecord { myName = "Some Guy"
                                                                    , myAddress = (Just (Entity addr2 Address {addressAddress = "30-50 Feral Hogs Rd"}))
                                                                    }
                                              })) -> True
                 _ -> False)

    itDb "can handle multiple left joins on the same record" $ do
        setup
        records <- select $ do
          from
            ( table @User
                `leftJoin` myNestedRecordQuery
                `on` (do \(user :& record) -> just (user ^. #id) ==. getField @"myUser" (getField @"myRecord" record) ?. #id)
                `leftJoin` myNestedRecordQuery
                `on` (do \(user :& record1 :& record2) -> getField @"myUser" (getField @"myRecord" record1) ?. #id !=. getField @"myUser" (getField @"myRecord" record2) ?. #id)
            )
        let sortedRecords = sortOn (\(Entity _ user :& _ :& _) -> getField @"userName" user) records
        liftIO $ sortedRecords !! 0
          `shouldSatisfy`
          (\case ( _ :& _ :& Just ( MyNestedRecord { myRecord = MyRecord { myName = "Some Guy"
                                                                    , myAddress = (Just (Entity addr2 Address {addressAddress = "30-50 Feral Hogs Rd"}))
                                                                    }
                                              })) -> True
                 _ -> False)
        liftIO $ sortedRecords !! 1
          `shouldSatisfy`
          (\case (_ :& _ :& Just (MyNestedRecord {myRecord = MyRecord {myName = "Rebecca", myAddress = Nothing}})) -> True
                 _ -> False)
