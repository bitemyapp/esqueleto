{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, GADTs, FlexibleContexts, EmptyDataDecls, GeneralizedNewtypeDeriving, Rank2Types, ConstraintKinds, MultiParamTypeClasses #-}

module Main (main) where

import Control.Applicative (Applicative(..))
import Control.Monad.Base (MonadBase(..))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger (MonadLogger(..), LogLevel(..))
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Database.Esqueleto
import Database.Persist.Sqlite
import Database.Persist.TH
import Language.Haskell.TH (Loc(..))
import System.IO (stderr)
import Test.Hspec
import Test.Hspec.Expectations

import qualified Control.Monad.Trans.Reader as R
import qualified Data.Conduit as C
import qualified Data.Text as T
import qualified System.Log.FastLogger as FL


-- Test schema
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
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


main :: IO ()
main = do
  let p1 = Person "John" (Just 36)
      p2 = Person "Rachel" Nothing
      p3 = Person "Mike" (Just 17)
  hspec $ do
    describe "select" $ do
      it "works for a single value" $
        run $ do
          ret <- select $ return $ val (3 :: Int)
          liftIO $ ret `shouldBe` [ Single 3 ]
    describe "select/from" $ do
      it "works for a simple example" $
        run $ do
          p1k <- insert p1
          ret <- select $
                 from $ \person ->
                 return person
          liftIO $ ret `shouldBe` [ Entity p1k p1 ]
      it "works for a simple self-join (one entity)" $
        run $ do
          p1k <- insert p1
          ret <- select $
                 from $ \(person1, person2) ->
                 return (person1, person2)
          liftIO $ ret `shouldBe` [ (Entity p1k p1, Entity p1k p1) ]
      it "works for a simple self-join (two entities)" $
        run $ do
          p1k <- insert p1
          p2k <- insert p2
          ret <- select $
                 from $ \(person1, person2) ->
                 return (person1, person2)
          liftIO $ ret `shouldBe` [ (Entity p1k p1, Entity p1k p1)
                                  , (Entity p1k p1, Entity p2k p2)
                                  , (Entity p2k p2, Entity p1k p1)
                                  , (Entity p2k p2, Entity p2k p2) ]
      it "works for a simple projection" $
        run $ do
          p1k <- insert p1
          p2k <- insert p2
          ret <- select $
                 from $ \p ->
                 return (p ^. PersonId, p ^. PersonName)
          liftIO $ ret `shouldBe` [ (Single p1k, Single (personName p1))
                                  , (Single p2k, Single (personName p2)) ]


----------------------------------------------------------------------


type RunDbMonad m = ( MonadBaseControl IO m, MonadIO m, MonadLogger m
                    , C.MonadUnsafeIO m, C.MonadThrow m )


run, runSilent, runVerbose :: (forall m. RunDbMonad m => SqlPersist (C.ResourceT m) a) -> IO a
runSilent  act = run_worker act
runVerbose act = execVerbose $ run_worker act
run =
   runSilent
-- runVerbose


run_worker :: RunDbMonad m => SqlPersist (C.ResourceT m) a -> m a
run_worker =
  C.runResourceT .
  withSqliteConn ":memory:" .
  runSqlConn .
  (runMigrationSilent migrateAll >>)


newtype Verbose a = Verbose { unVerbose :: R.ReaderT FL.Logger IO a }
  deriving (Functor, Applicative, Monad, MonadIO, C.MonadUnsafeIO, C.MonadThrow)

instance MonadBase IO Verbose where
  liftBase = Verbose . liftBase

instance MonadBaseControl IO Verbose where
  newtype StM Verbose a = StMV { unStMV :: StM (R.ReaderT FL.Logger IO) a }
  liftBaseWith f = Verbose . liftBaseWith $ \r -> f (fmap StMV . r . unVerbose)
  restoreM       = Verbose . restoreM . unStMV

instance MonadLogger Verbose where
  monadLoggerLog loc level msg =
    Verbose $ do
      logger <- R.ask
      liftIO $ FL.loggerPutStr logger $
        [ FL.LB "["
        , FL.LS $ case level of
                    LevelOther t -> T.unpack t
                    _ -> drop 5 $ show level
        , FL.LB "] "
        , FL.toLogStr msg
        , FL.LB " @("
        , FL.LS $ (loc_package loc) ++
               ':' : (loc_module loc) ++
               ' ' : (loc_filename loc) ++
               ':' : (show . fst $ loc_start loc) ++
               ':' : (show . snd $ loc_start loc)
        , FL.LB ")\n"
        ]


execVerbose :: Verbose a -> IO a
execVerbose (Verbose act) = do
  logger <- FL.mkLogger True stderr
  x <- R.runReaderT act logger
  FL.loggerFlush logger
  return x
