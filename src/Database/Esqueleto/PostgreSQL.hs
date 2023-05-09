{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contain PostgreSQL-specific functions.
--
-- @since: 2.2.8
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
    , updateReturningAll
    , upsert
    , upsertBy
    , insertSelectWithConflict
    , insertSelectWithConflictCount
    , noWait
    , wait
    , skipLocked
    , forUpdateOf
    , forShareOf
    , filterWhere
    , values
    -- * Internal
    , unsafeSqlAggregateFunction
    ) where

#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup
#endif
import Conduit (withAcquire)
import Control.Arrow (first)
import Control.Exception (throw)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Control.Monad.Trans.Reader as R
import Data.Int (Int64)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Proxy (Proxy(..))
import qualified Data.Text.Internal.Builder as TLB
import qualified Data.Text.Lazy as TL
import Data.Time.Clock (UTCTime)
import qualified Database.Esqueleto.Experimental as Ex
import Database.Esqueleto.Internal.Internal hiding (random_)
import Database.Esqueleto.Internal.PersistentImport hiding
       (uniqueFields, upsert, upsertBy)
import Database.Persist.SqlBackend

-- | (@random()@) Split out into database specific modules
-- because MySQL uses `rand()`.
--
-- @since 2.6.0
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
data AggMode
    = AggModeAll -- ^ ALL
    | AggModeDistinct -- ^ DISTINCT
    deriving (Show)

-- | (Internal) Create a custom aggregate functions with aggregate mode
--
-- /Do/ /not/ use this function directly, instead define a new function and give
-- it a type (see `unsafeSqlBinOp`)
unsafeSqlAggregateFunction
    :: UnsafeSqlFunctionArgument a
    => TLB.Builder
    -> AggMode
    -> a
    -> [OrderByClause]
    -> SqlExpr (Value b)
unsafeSqlAggregateFunction name mode args orderByClauses = ERaw noMeta $ \_ info ->
    let (orderTLB, orderVals) = makeOrderByNoNewline info orderByClauses
        -- Don't add a space if we don't have order by clauses
        orderTLBSpace =
            case orderByClauses of
                []    -> ""
                (_:_) -> " "
        (argsTLB, argsVals) =
            uncommas' $ map (\(ERaw _ f) -> f Never info) $ toArgList args
        aggMode =
            case mode of
                AggModeAll      -> ""
                -- ALL is the default, so we don't need to
                -- specify it
                AggModeDistinct -> "DISTINCT "
    in ( name <> parens (aggMode <> argsTLB <> orderTLBSpace <> orderTLB)
       , argsVals <> orderVals
       )

--- | (@array_agg@) Concatenate input values, including @NULL@s,
--- into an array.
arrayAggWith
    :: AggMode
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
-- @since 2.5.3
arrayAggDistinct
    :: (PersistField a, PersistField [a])
    => SqlExpr (Value a)
    -> SqlExpr (Value (Maybe [a]))
arrayAggDistinct x = arrayAggWith AggModeDistinct x []

-- | (@array_remove@) Remove all elements equal to the given value from the
-- array.
--
-- @since 2.5.3
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
-- @since 2.2.8
stringAgg ::
     SqlString s
  => SqlExpr (Value s) -- ^ Input values.
  -> SqlExpr (Value s) -- ^ Delimiter.
  -> SqlExpr (Value (Maybe s)) -- ^ Concatenation.
stringAgg expr delim = stringAggWith AggModeAll expr delim []

-- | (@chr@) Translate the given integer to a character. (Note the result will
-- depend on the character set of your database.)
--
-- @since 2.2.11
chr :: SqlString s => SqlExpr (Value Int) -> SqlExpr (Value s)
chr = unsafeSqlFunction "chr"

now_ :: SqlExpr (Value UTCTime)
now_ = unsafeSqlFunction "NOW" ()

upsert
    ::
    ( MonadIO m
    , PersistEntity record
    , OnlyOneUniqueKey record
    , PersistRecordBackend record SqlBackend
    , IsPersistBackend (PersistEntityBackend record)
    )
    => record
    -- ^ new record to insert
    -> [SqlExpr (Entity record) -> SqlExpr Update]
    -- ^ updates to perform if the record already exists
    -> R.ReaderT SqlBackend m (Entity record)
    -- ^ the record in the database after the operation
upsert record updates = do
    uniqueKey <- onlyUnique record
    upsertBy uniqueKey record updates

upsertBy
    ::
    (MonadIO m
    , PersistEntity record
    , IsPersistBackend (PersistEntityBackend record)
    )
    => Unique record
    -- ^ uniqueness constraint to find by
    -> record
    -- ^ new record to insert
    -> [SqlExpr (Entity record) -> SqlExpr Update]
    -- ^ updates to perform if the record already exists
    -> R.ReaderT SqlBackend m (Entity record)
    -- ^ the record in the database after the operation
upsertBy uniqueKey record updates = do
    sqlB <- R.ask
    case getConnUpsertSql sqlB of
        Nothing ->
            -- Postgres backend should have connUpsertSql, if this error is
            -- thrown, check changes on persistent
            throw (UnexpectedCaseErr OperationNotSupported)
        Just upsertSql ->
            handler sqlB upsertSql
  where
    addVals l = map toPersistValue (toPersistFields record) ++ l ++ persistUniqueToValues uniqueKey
    entDef = entityDef (Just record)
    updatesText conn = first builderToText $ renderUpdates conn updates
#if MIN_VERSION_persistent(2,11,0)
    uniqueFields = persistUniqueToFieldNames uniqueKey
    handler sqlB upsertSql = do
        let (updateText, updateVals) =
                updatesText sqlB
            queryText =
                upsertSql entDef uniqueFields updateText
            queryVals =
                addVals updateVals
        xs <- rawSql queryText queryVals
        pure (head xs)
#else
    uDef = toUniqueDef uniqueKey
    handler conn f = fmap head $ uncurry rawSql $
        (***) (f entDef (uDef :| [])) addVals $ updatesText conn
#endif

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
insertSelectWithConflict
    :: forall a m val backend
     . (FinalResult a, KnowResult a ~ Unique val, MonadIO m, PersistEntity val, SqlBackendCanWrite backend)
    => a
    -- ^ Unique constructor or a unique, this is used just to get the name of
    -- the postgres constraint, the value(s) is(are) never used, so if you have
    -- a unique "MyUnique 0", "MyUnique undefined" would work as well.
    -> SqlQuery (SqlExpr (Insertion val))
    -- ^ Insert query.
    -> (SqlExpr (Entity val) -> SqlExpr (Entity val) -> [SqlExpr (Entity val) -> SqlExpr Update])
    -- ^ A list of updates to be applied in case of the constraint being
    -- violated. The expression takes the current and excluded value to produce
    -- the updates.
    -> R.ReaderT backend m ()
insertSelectWithConflict unique query a =
    void $ insertSelectWithConflictCount unique query a

-- | Same as 'insertSelectWithConflict' but returns the number of rows affected.
--
-- @since 3.1.3
insertSelectWithConflictCount
    :: forall a val m backend
     . (FinalResult a, KnowResult a ~ Unique val, MonadIO m, PersistEntity val,
     SqlBackendCanWrite backend)
    => a
    -> SqlQuery (SqlExpr (Insertion val))
    -> (SqlExpr (Entity val) -> SqlExpr (Entity val) -> [SqlExpr (Entity val) -> SqlExpr Update])
    -> R.ReaderT backend m Int64
insertSelectWithConflictCount unique query conflictQuery = do
    conn <- R.ask
    uncurry rawExecuteCount $
        combine
            (toRawSql INSERT_INTO (conn, initialIdentState) query)
            (conflict conn)
  where
    proxy :: Proxy val
    proxy = Proxy
    updates = conflictQuery entCurrent entExcluded
    combine (tlb1,vals1) (tlb2,vals2) = (builderToText (tlb1 `mappend` tlb2), vals1 ++ vals2)
    entExcluded = unsafeSqlEntity (I "excluded")
    tableName = unEntityNameDB . getEntityDBName . entityDef
    entCurrent = unsafeSqlEntity (I (tableName proxy))
    uniqueDef = toUniqueDef unique
    constraint = TLB.fromText . unConstraintNameDB . uniqueDBName $ uniqueDef
    renderedUpdates :: (BackendCompatible SqlBackend backend) => backend -> (TLB.Builder, [PersistValue])
    renderedUpdates conn = renderUpdates conn updates
    conflict conn = (mconcat ([
        TLB.fromText "ON CONFLICT ON CONSTRAINT \"",
        constraint,
        TLB.fromText "\" DO "
      ] ++ if null updates then [TLB.fromText "NOTHING"] else [
        TLB.fromText "UPDATE SET ",
        updatesTLB
      ]),values')
      where
        (updatesTLB,values') = renderedUpdates conn

-- | Allow aggregate functions to take a filter clause.
--
-- Example of usage:
--
-- @
-- share [mkPersist sqlSettings] [persistLowerCase|
--   User
--     name Text
--     deriving Eq Show
--   Task
--     userId UserId
--     completed Bool
--     deriving Eq Show
-- |]
--
-- select $ from $ \(users `InnerJoin` tasks) -> do
--   on $ users ^. UserId ==. tasks ^. TaskUserId
--   groupBy $ users ^. UserId
--   return
--    ( users ^. UserId
--    , count (tasks ^. TaskId) `filterWhere` (tasks ^. TaskCompleted ==. val True)
--    , count (tasks ^. TaskId) `filterWhere` (tasks ^. TaskCompleted ==. val False)
--    )
-- @
--
-- @since 3.3.3.3
filterWhere
    :: SqlExpr (Value a)
    -- ^ Aggregate function
    -> SqlExpr (Value Bool)
    -- ^ Filter clause
    -> SqlExpr (Value a)
filterWhere aggExpr clauseExpr = ERaw noMeta $ \_ info ->
    let (aggBuilder, aggValues) = case aggExpr of
            ERaw _ aggF     -> aggF Never info
        (clauseBuilder, clauseValues) = case clauseExpr of
            ERaw _ clauseF  -> clauseF Never info
    in ( aggBuilder <> " FILTER (WHERE " <> clauseBuilder <> ")"
       , aggValues <> clauseValues
       )


-- | Allows to use `VALUES (..)` in-memory set of values
-- in RHS of `from` expressions. Useful for JOIN's on
-- known values which also can be additionally preprocessed
-- somehow on db side with usage of inner PostgreSQL capabilities.
--
--
-- Example of usage:
--
-- @
-- share [mkPersist sqlSettings] [persistLowerCase|
--   User
--     name Text
--     age Int
--     deriving Eq Show
--
-- select $ do
--  bound :& user <- from $
--      values (   (val (10 :: Int), val ("ten" :: Text))
--            :| [ (val 20, val "twenty")
--               , (val 30, val "thirty") ]
--            )
--      `InnerJoin` table User
--      `on` (\((bound, _boundName) :& user) -> user^.UserAge >=. bound)
--  groupBy bound
--  pure (bound, count @Int $ user^.UserName)
-- @
--
-- @since 3.5.2.3
values :: (ToSomeValues a, Ex.ToAliasReference a, Ex.ToAlias a) => NE.NonEmpty a -> Ex.From a
values exprs = Ex.From $ do
    ident <- newIdentFor $ DBName "vq"
    alias <- Ex.toAlias $ NE.head exprs
    ref   <- Ex.toAliasReference ident alias
    let aliasIdents = mapMaybe (\someVal -> case someVal of
            SomeValue (ERaw aliasMeta _) -> sqlExprMetaAlias aliasMeta
            ) $ toSomeValues ref
    pure (ref, const $ mkExpr ident aliasIdents)
  where
    someValueToSql :: IdentInfo -> SomeValue -> (TLB.Builder, [PersistValue])
    someValueToSql info (SomeValue expr) = materializeExpr info expr

    mkValuesRowSql :: IdentInfo -> [SomeValue] -> (TLB.Builder, [PersistValue])
    mkValuesRowSql info vs =
        let materialized = someValueToSql info <$> vs
            valsSql = TLB.toLazyText . fst <$> materialized
            params = concatMap snd materialized
        in (TLB.fromLazyText $ "(" <> TL.intercalate "," valsSql <> ")", params)

    -- (VALUES (v11, v12,..), (v21, v22,..)) as "vq"("v1", "v2",..)
    mkExpr :: Ident -> [Ident] -> IdentInfo -> (TLB.Builder, [PersistValue])
    mkExpr valsIdent colIdents info =
        let materialized = mkValuesRowSql info . toSomeValues <$> NE.toList exprs
            (valsSql, params) =
                ( TL.intercalate "," $ map (TLB.toLazyText . fst) materialized
                , concatMap snd materialized
                )
            colsAliases = TL.intercalate "," (map (TLB.toLazyText . useIdent info) colIdents)
        in
            ( "(VALUES " <> TLB.fromLazyText valsSql <> ") AS "
            <> useIdent info valsIdent
            <> "(" <> TLB.fromLazyText colsAliases <> ")"
            , params
            )

-- | `NOWAIT` syntax for postgres locking
-- error will be thrown if locked rows are attempted to be selected
--
-- @since 3.5.9.0
noWait :: OnLockedBehavior
noWait = NoWait

-- | `SKIP LOCKED` syntax for postgres locking
-- locked rows will be skipped
--
-- @since 3.5.9.0
skipLocked :: OnLockedBehavior
skipLocked = SkipLocked

-- | default behaviour of postgres locks. will attempt to wait for locks to expire
--
-- @since 3.5.9.0
wait :: OnLockedBehavior
wait = Wait

-- | `FOR UPDATE OF` syntax for postgres locking
-- allows locking of specific tables with an update lock in a view or join
--
-- @since 3.5.9.0
forUpdateOf :: LockableEntity a => a -> OnLockedBehavior -> SqlQuery ()
forUpdateOf lockableEntities onLockedBehavior =
  putLocking $ PostgresLockingClauses [PostgresLockingKind PostgresForUpdate (Just $ LockingOfClause lockableEntities) onLockedBehavior]

-- | `FOR SHARE OF` syntax for postgres locking
-- allows locking of specific tables with a share lock in a view or join
--
-- @since 3.5.9.0

forShareOf :: LockableEntity a => a -> OnLockedBehavior -> SqlQuery ()
forShareOf lockableEntities onLockedBehavior =
  putLocking $ PostgresLockingClauses [PostgresLockingKind PostgresForShare (Just $ LockingOfClause lockableEntities) onLockedBehavior]

updateReturningAll :: (MonadIO m, PersistEntity ent, SqlBackendCanWrite backend, backend ~ PersistEntityBackend ent)
                   => (SqlExpr (Entity ent) -> SqlQuery (SqlExpr (Entity ent)))
                   -> R.ReaderT backend m [Entity ent]
updateReturningAll block = do
  conn <- R.ask
  conduit <- rawSelectSource UPDATE_RETSTAR (tellReturning ReturningStar >> from block)
  liftIO . withAcquire conduit $ flip R.runReaderT conn . runSource
