module Lib where

import Control.Monad.IO.Class (MonadIO)
import Database.Persist.Sql (SqlReadT)
import           Database.Esqueleto                   (SqlExpr, SqlQuery, from,
                                                       val, (<#), insertSelect, (<&>), (^.))
import           Database.Esqueleto.Internal.Language (Insertion)
import           Database.Persist.TH                  (mkDeleteCascade,
                                                       mkMigrate, mkPersist,
                                                       persistLowerCase, share,
                                                       sqlSettings)

share [ mkPersist sqlSettings
      , mkDeleteCascade sqlSettings
      , mkMigrate "migrateAll"] [persistLowerCase|
  Person
    name String
    age Int Maybe
    born Int Maybe
    deriving Eq Show
  BlogPost
    title String
    authorId PersonId
    deriving Eq Show
  Follow
    follower PersonId
    followed PersonId
    deriving Eq Show
|]


