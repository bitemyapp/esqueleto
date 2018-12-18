{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Main where

import Control.Monad.IO.Class (MonadIO)
import Database.Persist.Sql (SqlReadT)
import           Database.Esqueleto                   (SqlExpr, SqlQuery, from,
                                                       val, (<#), insertSelect, (<&>), (^.))
import           Database.Esqueleto.Internal.Language (Insertion)
import           Database.Persist.TH                  (mkDeleteCascade,
                                                       mkMigrate, mkPersist,
                                                       persistLowerCase, share,
                                                       sqlSettings)

main :: IO ()
main = pure ()

share [ mkPersist sqlSettings
      , mkDeleteCascade sqlSettings
      , mkMigrate "migrateAll"] [persistLowerCase|
  Person
    name String
    age Int Maybe
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

writeQuery :: SqlQuery (SqlExpr (Insertion BlogPost))
writeQuery =
  from $ \p ->
  return $ BlogPost <# (val "Group Blog Post") <&> (p ^. PersonId)

shouldFail :: MonadIO m => SqlReadT m ()
shouldFail = insertSelect writeQuery
