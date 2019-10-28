{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings
           , GADTs, CPP, Rank2Types
           , ScopedTypeVariables
           , FlexibleInstances
 #-}
 {-# LANGUAGE TypeFamilies #-}
-- | This module contain PostgreSQL-specific functions.
--
-- /Since: 2.2.8/
module Database.Esqueleto.PostgreSQL
  ( AggMode(..)
  , arrayAggDistinct
  , arrayAgg
  , arrayAggWith
  , arrayRemove
  , arrayRemoveNull
  , stringAgg
  , stringAggWith
  , maybeArray
  , chr
  , now_
  , random_
  , upsert
  , upsertBy
  , insertSelectWithConflict
  , insertSelectWithConflictCount
  -- * Internal
  , unsafeSqlAggregateFunction
  , toUniqueDef
  ) where

#if __GLASGOW_HASKELL__ < 804
import           Data.Semigroup
#endif
import qualified Data.Text.Internal.Builder                   as TLB
import           Data.Time.Clock                              (UTCTime)
import           Database.Esqueleto.Internal.Language         hiding (random_)
import           Database.Esqueleto.Internal.PersistentImport hiding (upsert, upsertBy)
import           Database.Esqueleto.Internal.Sql
import           Database.Esqueleto.Internal.Internal         (EsqueletoError(..), CompositeKeyError(..), 
                                                              UnexpectedCaseError(..), SetClause, Ident(..),
                                                              uncommas)
import           Database.Persist.Class                       (OnlyOneUniqueKey)
import           Data.List.NonEmpty                           ( NonEmpty( (:|) ) )
import           Data.Int                                     (Int64)
import           Data.Proxy                                   (Proxy(..))
import           Control.Arrow                                ((***), first)
import           Control.Exception                            (Exception, throw, throwIO)
import           Control.Monad                                (void)
import           Control.Monad.IO.Class                       (MonadIO (..))
import qualified Control.Monad.Trans.Reader                   as R

-- | (@random()@) Split out into database specific modules
-- because MySQL uses `rand()`.
--
-- /Since: 2.6.0/
random_ :: (PersistField a, Num a) => SqlExpr (Value a)
random_ = unsafeSqlValue "RANDOM()"

-- | Empty array literal. (@val []@) does unfortunately not work
emptyArray :: SqlExpr (Value [a])
emptyArray = unsafeSqlValue "'{}'"

-- | Coalesce an array with an empty default value
maybeArray ::
     (PersistField a, PersistField [a])
  => SqlExpr (Value (Maybe [a]))
  -> SqlExpr (Value [a])
maybeArray x = coalesceDefault [x] (emptyArray)

-- | Aggregate mode
data AggMode = AggModeAll -- ^ ALL
             | AggModeDistinct -- ^ DISTINCT
  deriving (Show)

-- | (Internal) Create a custom aggregate functions with aggregate mode
--
-- /Do/ /not/ use this function directly, instead define a new function and give
-- it a type (see `unsafeSqlBinOp`)
unsafeSqlAggregateFunction ::
     UnsafeSqlFunctionArgument a
  => TLB.Builder
  -> AggMode
  -> a
  -> [OrderByClause]
  -> SqlExpr (Value b)
unsafeSqlAggregateFunction name mode args orderByClauses =
  ERaw Never $ \info ->
    let (orderTLB, orderVals) = makeOrderByNoNewline info orderByClauses
        -- Don't add a space if we don't have order by clauses
        orderTLBSpace = case orderByClauses of
                          [] -> ""
                          (_:_) -> " "
        (argsTLB, argsVals) =
          uncommas' $ map (\(ERaw _ f) -> f info) $ toArgList args
        aggMode = case mode of
                    AggModeAll -> "" -- ALL is the default, so we don't need to
                                     -- specify it
                    AggModeDistinct -> "DISTINCT "
    in ( name <> parens (aggMode <> argsTLB <> orderTLBSpace <> orderTLB)
       , argsVals <> orderVals
       )

--- | (@array_agg@) Concatenate input values, including @NULL@s,
--- into an array.
arrayAggWith ::
     AggMode
  -> SqlExpr (Value a)
  -> [OrderByClause]
  -> SqlExpr (Value (Maybe [a]))
arrayAggWith = unsafeSqlAggregateFunction "array_agg"

--- | (@array_agg@) Concatenate input values, including @NULL@s,
--- into an array.
arrayAgg :: (PersistField a) => SqlExpr (Value a) -> SqlExpr (Value (Maybe [a]))
arrayAgg x = arrayAggWith AggModeAll x []

-- | (@array_agg@) Concatenate distinct input values, including @NULL@s, into
-- an array.
--
-- /Since: 2.5.3/
arrayAggDistinct ::
     (PersistField a, PersistField [a])
  => SqlExpr (Value a)
  -> SqlExpr (Value (Maybe [a]))
arrayAggDistinct x = arrayAggWith AggModeDistinct x []


-- | (@array_remove@) Remove all elements equal to the given value from the
-- array.
--
-- /Since: 2.5.3/
arrayRemove :: SqlExpr (Value [a]) -> SqlExpr (Value a) -> SqlExpr (Value [a])
arrayRemove arr elem' = unsafeSqlFunction "array_remove" (arr, elem')

-- | Remove @NULL@ values from an array
arrayRemoveNull :: SqlExpr (Value [Maybe a]) -> SqlExpr (Value [a])
-- This can't be a call to arrayRemove because it changes the value type
arrayRemoveNull x = unsafeSqlFunction "array_remove" (x, unsafeSqlValue "NULL")


-- | (@string_agg@) Concatenate input values separated by a
-- delimiter.
stringAggWith ::
     SqlString s
  => AggMode -- ^ Aggregate mode (ALL or DISTINCT)
  -> SqlExpr (Value s) -- ^ Input values.
  -> SqlExpr (Value s) -- ^ Delimiter.
  -> [OrderByClause] -- ^ ORDER BY clauses
  -> SqlExpr (Value (Maybe s)) -- ^ Concatenation.
stringAggWith mode expr delim os =
  unsafeSqlAggregateFunction "string_agg" mode (expr, delim) os

-- | (@string_agg@) Concatenate input values separated by a
-- delimiter.
--
-- /Since: 2.2.8/
stringAgg ::
     SqlString s
  => SqlExpr (Value s) -- ^ Input values.
  -> SqlExpr (Value s) -- ^ Delimiter.
  -> SqlExpr (Value (Maybe s)) -- ^ Concatenation.
stringAgg expr delim = stringAggWith AggModeAll expr delim []

-- | (@chr@) Translate the given integer to a character. (Note the result will
-- depend on the character set of your database.)
--
-- /Since: 2.2.11/
chr :: SqlString s => SqlExpr (Value Int) -> SqlExpr (Value s)
chr = unsafeSqlFunction "chr"

now_ :: SqlExpr (Value UTCTime)
now_ = unsafeSqlValue "NOW()"

upsert :: (MonadIO m,
      PersistEntity record,
      OnlyOneUniqueKey record,
      PersistRecordBackend record SqlBackend,
      IsPersistBackend (PersistEntityBackend record))
    => record
    -- ^ new record to insert
    -> [SqlExpr (Update record)]
    -- ^ updates to perform if the record already exists
    -> R.ReaderT SqlBackend m (Entity record)
    -- ^ the record in the database after the operation
upsert record updates = do
    uniqueKey <- onlyUnique record
    upsertBy uniqueKey record updates

upsertBy :: (MonadIO m,
    PersistEntity record,
    IsPersistBackend (PersistEntityBackend record))
  => Unique record
  -- ^ uniqueness constraint to find by
  -> record
  -- ^ new record to insert
  -> [SqlExpr (Update record)]
  -- ^ updates to perform if the record already exists
  -> R.ReaderT SqlBackend m (Entity record)
  -- ^ the record in the database after the operation
upsertBy uniqueKey record updates = do
  sqlB <- R.ask
  maybe
    (throw (UnexpectedCaseErr OperationNotSupported)) -- Postgres backend should have connUpsertSql, if this error is thrown, check changes on persistent 
    (handler sqlB)
    (connUpsertSql sqlB)
  where
    addVals l = map toPersistValue (toPersistFields record) ++ l ++ persistUniqueToValues uniqueKey
    entDef = entityDef (Just record)
    uDef = toUniqueDef uniqueKey
    updatesText conn = first builderToText $ renderUpdates conn updates
    handler conn f = fmap head $ uncurry rawSql $
      (***) (f entDef (uDef :| [])) addVals $ updatesText conn

-- | Render postgres updates to be use in a SET clause.
-- 
-- @since 3.1.3
renderUpdates :: (BackendCompatible SqlBackend backend) => 
    backend
    -> [SqlExpr (Update val)]
    -> (TLB.Builder, [PersistValue])
renderUpdates conn = uncommas' . concatMap renderUpdate
    where
      mk :: SqlExpr (Value ()) -> [(TLB.Builder, [PersistValue])]
      mk (ERaw _ f)        = [f info]
      mk (ECompositeKey _) = throw (CompositeKeyErr MakeSetError) -- FIXME
      renderUpdate :: SqlExpr (Update val) -> [(TLB.Builder, [PersistValue])]
      renderUpdate (ESet f) = mk (f undefined) -- second parameter of f is always unused
      info = (projectBackend conn, initialIdentState)

type family KnowResult a where
  KnowResult (i -> o) = KnowResult o
  KnowResult a = a

class FinalResult a where
  finalR :: a -> KnowResult a

instance FinalResult (Unique val) where
  finalR = id

instance (FinalResult b) => FinalResult (a -> b) where
  finalR f = finalR (f undefined)

toUniqueDef :: forall a val. (KnowResult a ~ (Unique val), PersistEntity val,FinalResult a) => 
  a -> UniqueDef
toUniqueDef uniqueConstructor = uniqueDef
  where
    proxy :: Proxy val
    proxy = Proxy
    unique :: Unique val
    unique = finalR uniqueConstructor
    -- there must be a better way to get the constrain name from a unique, make this not a list search
    filterF = (==) (persistUniqueToFieldNames unique) . uniqueFields
    uniqueDef = head . filter filterF . entityUniques . entityDef $ proxy

-- | Inserts into a table the results of a query similar to 'insertSelect' but allows
-- to update values that violate a constraint during insertions.
--
-- Example of usage:
--
-- @
-- share [ mkPersist sqlSettings
--       , mkDeleteCascade sqlSettings
--       , mkMigrate "migrate"
--       ] [persistLowerCase|
--   Bar
--     num Int
--     deriving Eq Show
--   Foo
--     num Int
--     UniqueFoo num
--     deriving Eq Show
-- |]
--
-- insertSelectWithConflict 
--   UniqueFoo -- (UniqueFoo undefined) or (UniqueFoo anyNumber) would also work
--   (from $ \b ->
--     return $ Foo <# (b ^. BarNum)
--   )
--   (\current excluded ->
--     [FooNum =. (current ^. FooNum) +. (excluded ^. FooNum)]
--   )
-- @
--
-- Inserts to table Foo all Bar.num values and in case of conflict SomeFooUnique, 
-- the conflicting value is updated to the current plus the excluded.
--
-- @since 3.1.3
insertSelectWithConflict :: forall a m val. (
    FinalResult a,
    KnowResult a ~ (Unique val), 
    MonadIO m, 
    PersistEntity val) => 
  a
  -- ^ Unique constructor or a unique, this is used just to get the name of the postgres constraint, the value(s) is(are) never used, so if you have a unique "MyUnique 0", "MyUnique undefined" would work as well.
  -> SqlQuery (SqlExpr (Insertion val)) 
  -- ^ Insert query.
  -> (SqlExpr (Entity val) -> SqlExpr (Entity val) -> [SqlExpr (Update val)])
  -- ^ A list of updates to be applied in case of the constraint being violated. The expression takes the current and excluded value to produce the updates.
  -> SqlWriteT m ()
insertSelectWithConflict unique query = void . insertSelectWithConflictCount unique query

-- | Same as 'insertSelectWithConflict' but returns the number of rows affected.
insertSelectWithConflictCount :: forall a val m. (
    FinalResult a,
    KnowResult a ~ (Unique val), 
    MonadIO m, 
    PersistEntity val) => 
  a
  -> SqlQuery (SqlExpr (Insertion val)) 
  -> (SqlExpr (Entity val) -> SqlExpr (Entity val) -> [SqlExpr (Update val)])
  -> SqlWriteT m Int64
insertSelectWithConflictCount unique query conflictQuery = do
  conn <- R.ask
  uncurry rawExecuteCount $
    combine
      (toRawSql INSERT_INTO (conn, initialIdentState) (fmap EInsertFinal query))
      (conflict conn)
  where
    proxy :: Proxy val
    proxy = Proxy
    updates = conflictQuery entCurrent entExcluded
    combine (tlb1,vals1) (tlb2,vals2) = (builderToText (tlb1 `mappend` tlb2), vals1 ++ vals2)
    entExcluded = EEntity $ I "excluded"
    tableName = unDBName . entityDB . entityDef
    entCurrent = EEntity $ I (tableName proxy)
    uniqueDef = toUniqueDef unique
    constraint = TLB.fromText . unDBName . uniqueDBName $ uniqueDef
    renderedUpdates :: (BackendCompatible SqlBackend backend) => backend -> (TLB.Builder, [PersistValue])
    renderedUpdates conn = renderUpdates conn updates
    conflict conn = (foldr1 mappend ([
        TLB.fromText "ON CONFLICT ON CONSTRAINT \"",
        constraint,
        TLB.fromText "\" DO "
      ] ++ if null updates then [TLB.fromText "NOTHING"] else [      
        TLB.fromText "UPDATE SET ",
        updatesTLB
      ]),values)
      where
        (updatesTLB,values) = renderedUpdates conn
