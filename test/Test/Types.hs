module Test.Types where

import Database.Persist.TH

-- Test schema
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
  Foo
    name Int
    Primary name
  Bar
    quux FooId

  Person
    name String
    age Int Maybe
    weight Int Maybe
    favNum Int
    deriving Eq Show

  BlogPost
    title String
    authorId PersonId
    deriving Eq Show

  Lord
    county String maxlen=100
    dogs Int Maybe
    Primary county
    deriving Show

  Deed
    contract String maxlen=100
    ownerId LordId maxlen=100
    Primary contract
    deriving Show

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
|]
