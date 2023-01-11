{-# OPTIONS_GHC -ddump-splices #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
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

import Database.Esqueleto.Experimental.ToMaybe
import Data.Coerce
import GHC.Records
import Data.Proxy
import Database.Esqueleto.Internal.Internal hiding (from, on)
import Common.Test.Import hiding (from, on)
import Data.List (sortOn)
import Database.Esqueleto.Experimental
import Database.Esqueleto.Record

data MyRecord =
    MyRecord
        { myName :: Text
        , myAge :: Maybe Int
        , myUser :: Entity User
        , myAddress :: Maybe (Entity Address)
        }
  deriving (Show, Eq)

$(deriveEsqueletoRecord ''MyRecord)

doesThisWork :: SqlMyRecord -> SqlExpr (Value Text)
doesThisWork = getField @"myName"

whatAboutThis :: Maybe SqlMyRecord -> SqlExpr (Value (Maybe Text))
whatAboutThis = getField @"myName"

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

data MyNestedRecordMaybe = MyNestedRecordMaybe
    { myName :: Text
    , myMaybeRecord :: Maybe MyRecord
    }
    deriving (Show, Eq)

deriveEsqueletoRecord ''MyNestedRecordMaybe

data MaybeNestedTwice = MaybeNestedTwice
    { blah :: Int
    , nestedTwice :: Maybe MyNestedRecordMaybe
    }
    deriving (Show, Eq)

deriveEsqueletoRecord ''MaybeNestedTwice

myNestedRecordMaybeQuery :: SqlQuery SqlMyNestedRecordMaybe
myNestedRecordMaybeQuery = do
    user :& address <-
        from $
            table @User
            `leftJoin` table @Address
                `on` do
                    \(user :& address) ->
                        user ^. #address ==. address ?. #id
    pure
        SqlMyNestedRecordMaybe
            { myName = castString $ user ^. #name
            , myMaybeRecord =
                Just SqlMyRecord
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

    itDb "can select nested Maybe records" $ do
        setup
        records <- select myNestedRecordMaybeQuery
        let sortedRecords = sortOn (\MyNestedRecordMaybe {myName} -> myName) records
        liftIO $ sortedRecords !! 0
          `shouldSatisfy`
          (\case MyNestedRecordMaybe
                   { myName = "Rebecca"
                   , myMaybeRecord =
                       Just MyRecord { myName = "Rebecca"
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
          (\case MyNestedRecordMaybe
                   { myName = "Some Guy"
                   , myMaybeRecord =
                       Just MyRecord { myName = "Some Guy"
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

    itDb "can be used in a left join" $ do
        setup
        records <- select $ do
            from $ table @User `leftJoin` myRecordQuery
                `on` do
                    \(u :& myRecord) ->
                        just (castString @String @Text (u ^. UserName)) ==.
                            (getField @"myName" myRecord)
        pure ()
