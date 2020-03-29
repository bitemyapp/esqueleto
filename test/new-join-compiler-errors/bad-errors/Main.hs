{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import           Control.Monad.IO.Class               (MonadIO)
import           Database.Esqueleto hiding (from,on)
import           Database.Esqueleto.Experimental
import           Database.Esqueleto.Internal.Language (Insertion)
import           Database.Persist.Sql                 (SqlWriteT)
import           Database.Persist.TH                  (mkDeleteCascade,
                                                       mkMigrate, mkPersist,
                                                       persistLowerCase, share,
                                                       sqlSettings)

import           Lib

main :: IO ()
main = pure ()

-- Missing on condition leads to an unintelligeable error and points to the wrong spot
missingOnConditionShouldFail :: MonadIO m => SqlPersistT m [(Entity Person, Entity BlogPost)]
missingOnConditionShouldFail = select $ do
    (people :& blogPosts) <-
      from $ Table @Person
      `LeftOuterJoin` Table @BlogPost
    pure (people, blogPosts)

-- Mismatched union when one part is returning a different shape than the other 
mismatchedUnion :: MonadIO m => SqlPersistT m [(Value String, Value (Maybe Int))]
mismatchedUnion = select . from $ 
  (SelectQuery $ do
    people <- from $ Table @Person
    pure (people ^. PersonName, people ^. PersonAge))
  `Union`
  (SelectQuery $ do
    people <- from $ Table @Person
    pure (people ^. PersonName))

incorrectNumberOfOnElements = select . from $
  Table @Person
  `LeftOuterJoin` Table @Follow
  `on` (\(people :& follows) -> just (people ^. PersonId) ==. follows ?. FollowFollowed)
  `LeftOuterJoin` Table @Person
  `on` (\(follows :& followers) -> followers ?. PersonId ==. follows ?. FollowFollower)

