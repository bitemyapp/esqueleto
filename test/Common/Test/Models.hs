{-# LANGUAGE CPP #-}
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

module Common.Test.Models where

import Data.Time
import Database.Esqueleto
import Database.Persist.Sql
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
  Foo
    name Int
    Primary name
    deriving Show Eq Ord
  Bar
    quux FooId
    deriving Show Eq Ord
  Baz
    blargh FooId
    deriving Show Eq
  Shoop
    baz BazId
    deriving Show Eq
  Asdf
    shoop ShoopId
    deriving Show Eq
  Another
    why BazId
  YetAnother
    argh ShoopId

  Person
    name String
    age Int Maybe
    weight Int Maybe
    favNum Int
    deriving Eq Show Ord
  BlogPost
    title String
    authorId PersonId
    deriving Eq Show
  Comment
    body String
    blog BlogPostId
    deriving Eq Show
  CommentReply
    body String
    comment CommentId
  Profile
    name String
    person PersonId
    deriving Eq Show
  Reply
    guy PersonId
    body String
    deriving Eq Show

  Lord
    county String maxlen=100
    dogs Int Maybe
    Primary county
    deriving Eq Show

  Deed
    contract String maxlen=100
    ownerId LordId maxlen=100
    Primary contract
    deriving Eq Show

  Follow
    follower PersonId
    followed PersonId
    deriving Eq Show

  CcList
    names [String]

  Frontcover
    number Int
    title String
    Primary number
    deriving Eq Show
  Article
    title String
    frontcoverNumber Int
    Foreign Frontcover fkfrontcover frontcoverNumber
    deriving Eq Show
  ArticleMetadata
    articleId ArticleId
    Primary articleId
    deriving Eq Show
  Tag
    name String maxlen=100
    Primary name
    deriving Eq Show
  ArticleTag
    articleId ArticleId
    tagId     TagId maxlen=100
    Primary   articleId tagId
    deriving Eq Show
  Article2
    title String
    frontcoverId FrontcoverId
    deriving Eq Show
  Point
    x Int
    y Int
    name String
    Primary x y
    deriving Eq Show
  Circle
    centerX Int
    centerY Int
    name String
    Foreign Point fkpoint centerX centerY
    deriving Eq Show
  Numbers
    int    Int
    double Double
    deriving Eq Show

  JoinOne
    name    String
    deriving Eq Show

  JoinTwo
    joinOne JoinOneId
    name    String
    deriving Eq Show

  JoinThree
    joinTwo JoinTwoId
    name    String
    deriving Eq Show

  JoinFour
    name    String
    joinThree JoinThreeId
    deriving Eq Show

  JoinOther
    name    String
    deriving Eq Show

  JoinMany
    name      String
    joinOther JoinOtherId
    joinOne   JoinOneId
    deriving Eq Show

  DateTruncTest
    created   UTCTime
    deriving Eq Show

  User
    address AddressId Maybe
    name String
    deriving Show
    deriving Eq

  Address
    address String
    deriving Show
    deriving Eq
|]

-- Unique Test schema
share [mkPersist sqlSettings, mkMigrate "migrateUnique"] [persistUpperCase|
  OneUnique
    name String
    value Int
    UniqueValue value
    deriving Eq Show
|]


instance ToBaseId ArticleMetadata where
    type BaseEnt ArticleMetadata = Article
    toBaseIdWitness articleId = ArticleMetadataKey articleId

