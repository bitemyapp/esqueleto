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
-- @since 2.2.8
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
    , upsertMaybe
    , upsertBy
    , upsertMaybeBy
    , insertSelectWithConflict
    , insertSelectWithConflictCount
    , noWait
    , wait
    , skipLocked
    , forUpdateOf
    , forNoKeyUpdateOf
    , forShareOf
    , forKeyShareOf
    , filterWhere
    , values
    , ilike
    , distinctOn
    , distinctOnOrderBy
    , withMaterialized
    , withNotMaterialized
    , ascNullsFirst
    , ascNullsLast
    , descNullsFirst
    , descNullsLast
    -- * Internal
    , unsafeSqlAggregateFunction
    ) where

import Control.Arrow (first)
import Control.Exception (throw)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.Writer as W
import Data.Int (Int64)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Proxy (Proxy(..))
import qualified Data.Text as Text
import qualified Data.Text.Internal.Builder as TLB
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Data.Time.Clock (UTCTime)
import qualified Database.Esqueleto.Experimental as Ex
import qualified Database.Esqueleto.Experimental.From as Ex
import Database.Esqueleto.Experimental.From.CommonTableExpression
import Database.Esqueleto.Experimental.From.SqlSetOperation
import Database.Esqueleto.Experimental.ToAlias
import Database.Esqueleto.Experimental.ToAliasReference
import Database.Esqueleto.Internal.Internal hiding
       (From(..), ilike, distinctOn, distinctOnOrderBy, from, on, random_)
import Database.Esqueleto.Internal.PersistentImport hiding
       (uniqueFields, upsert, upsertBy)
import Database.Persist (ConstraintNameDB(..), EntityNameDB(..))
import Database.Persist.Class (OnlyOneUniqueKey)
import Database.Persist.SqlBackend
import GHC.Stack

-- | (@random()@) Split out into database specific modules
-- because MySQL uses `rand()`.
--
-- @since 2.6.0
random_ :: (PersistField a, Num a) => SqlExpr (Value a)
random_ = unsafeSqlValue "RANDOM()"

-- | @DISTINCT ON@.  Change the current @SELECT@ into
-- @SELECT DISTINCT ON (SqlExpressions)@.  For example:
--
-- @
-- select $ do
--   foo <- 'from' $ table \@Foo
--   'distinctOn' ['don' (foo ^. FooName), 'don' (foo ^. FooState)]
--   pure foo
-- @
--
-- You can also chain different calls to 'distinctOn'.  The
-- above is equivalent to:
--
-- @
-- select $ do
--   foo <- 'from' $ table \@Foo
--   'distinctOn' ['don' (foo ^. FooName)]
--   'distinctOn' ['don' (foo ^. FooState)]
--   pure foo
-- @
--
-- Each call to 'distinctOn' adds more SqlExpressions.  Calls to
-- 'distinctOn' override any calls to 'distinct'.
--
-- Note that PostgreSQL requires the SqlExpressions on @DISTINCT
-- ON@ to be the first ones to appear on a @ORDER BY@.  This is
-- not managed automatically by esqueleto, keeping its spirit
-- of trying to be close to raw SQL.
--
-- @since 3.6.0
distinctOn :: [SqlExpr DistinctOn] -> SqlQuery ()
distinctOn exprs = Q (W.tell mempty { sdDistinctClause = DistinctOn exprs })

-- | A convenience function that calls both 'distinctOn' and
-- 'orderBy'.  In other words,
--
-- @
-- 'distinctOnOrderBy' [asc foo, desc bar, desc quux]
-- @
--
-- is the same as:
--
-- @
-- 'distinctOn' [don foo, don  bar, don  quux]
-- 'orderBy'  [asc foo, desc bar, desc quux]
--   ...
-- @
--
-- @since 3.6.0
distinctOnOrderBy :: [SqlExpr OrderBy] -> SqlQuery ()
distinctOnOrderBy exprs = do
    distinctOn (toDistinctOn <$> exprs)
    orderBy exprs
  where
    toDistinctOn :: SqlExpr OrderBy -> SqlExpr DistinctOn
    toDistinctOn (ERaw m f) = ERaw m $ \p info ->
        let (b, vals) = f p info
        in  ( TLB.fromLazyText
              $ TL.replace " DESC" ""
              $ TL.replace " ASC" ""
              $ TLB.toLazyText b
            , vals )
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

-- | Perform an @upsert@ operation on the given record.
--
-- If the record exists in the database already, then the updates will be
-- performed on that record. If the record does not exist, then the
-- provided record will be inserted.
--
-- If you wish to provide an empty list of updates (ie "if the record
-- exists, do nothing"), then you will need to call 'upsertMaybe'. Postgres
-- will not return anything if there are no modifications or inserts made.
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
    -> NE.NonEmpty (SqlExpr (Entity record) -> SqlExpr Update)
    -- ^ updates to perform if the record already exists
    -> R.ReaderT SqlBackend m (Entity record)
    -- ^ the record in the database after the operation
upsert record =
    upsertBy (onlyUniqueP record) record

-- | Like 'upsert', but permits an empty list of updates to be performed.
--
-- If no updates are provided and the record already was present in the
-- database, then this will return 'Nothing'. If you want to fetch the
-- record out of the database, you can write:
--
-- @
--  mresult <- upsertMaybe record []
--  case mresult of
--      Nothing ->
--          'getBy' ('onlyUniqueP' record)
--      Just res ->
--          pure (Just res)
-- @
--
-- @since 3.6.0.0
upsertMaybe
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
    -> R.ReaderT SqlBackend m (Maybe (Entity record))
    -- ^ the record in the database after the operation
upsertMaybe rec upds = do
    upsertMaybeBy (onlyUniqueP rec) rec upds

-- | Attempt to insert a @record@ into the database. If the @record@
-- already exists for the given @'Unique' record@, then a list of updates
-- will be performed.
--
-- If you provide an empty list of updates, then this function will return
-- 'Nothing' if the record already exists in the database.
--
-- @since 3.6.0.0
upsertMaybeBy
    ::
    ( MonadIO m
    , PersistEntity record
    , IsPersistBackend (PersistEntityBackend record)
    )
    => Unique record
    -- ^ uniqueness constraint to find by
    -> record
    -- ^ new record to insert
    -> [SqlExpr (Entity record) -> SqlExpr Update]
    -- ^ updates to perform if the record already exists
    -> R.ReaderT SqlBackend m (Maybe (Entity record))
    -- ^ the record in the database after the operation
upsertMaybeBy uniqueKey record updates = do
    sqlB <- R.ask
    case getConnUpsertSql sqlB of
        Nothing ->
            -- Postgres backend should have connUpsertSql, if this error is
            -- thrown, check changes on persistent
            throw (UnexpectedCaseErr OperationNotSupported)
        Just upsertSql ->
            handler sqlB upsertSql
  where
    addVals l =
        map toPersistValue (toPersistFields record) ++ l ++ case updates of
            [] ->
                []
            _ ->
                persistUniqueToValues uniqueKey
    entDef =
        entityDef (Just record)
    updatesText conn =
        first builderToText $ renderUpdates conn updates
    uniqueFields = persistUniqueToFieldNames uniqueKey
    handler sqlB upsertSql = do
        let (updateText, updateVals) =
                updatesText sqlB
            queryTextUnmodified =
                upsertSql entDef uniqueFields updateText
            queryText =
                case updates of
                    [] ->
                        let
                            (okay, _bad) =
                                Text.breakOn "DO UPDATE" queryTextUnmodified
                            good =
                                okay <> "DO NOTHING RETURNING ??"
                        in
                            good
                    _ ->
                        queryTextUnmodified

            queryVals =
                addVals updateVals
        xs <- rawSql queryText queryVals
        pure (listToMaybe xs)

upsertBy
    ::
    ( MonadIO m
    , PersistEntity record
    , IsPersistBackend (PersistEntityBackend record)
    , HasCallStack
    )
    => Unique record
    -- ^ uniqueness constraint to find by
    -> record
    -- ^ new record to insert
    -> NE.NonEmpty (SqlExpr (Entity record) -> SqlExpr Update)
    -- ^ updates to perform if the record already exists
    -> R.ReaderT SqlBackend m (Entity record)
    -- ^ the record in the database after the operation
upsertBy uniqueKey record updates = do
    mrec <- upsertMaybeBy uniqueKey record (NE.toList updates)
    case mrec of
        Nothing ->
            error "non-empty list of updates should have resulted in a row being returned"
        Just rec ->
            pure rec

-- | Inserts into a table the results of a query similar to 'insertSelect' but allows
-- to update values that violate a constraint during insertions.
--
-- Example of usage:
--
-- @
-- 'mkPersist' 'sqlSettings' ['persistLowerCase'|
--   Bar
--     num Int
--     deriving Eq Show
--   Foo
--     num Int
--     UniqueFoo num
--     deriving Eq Show
-- |]
--
-- action = do
--     'insertSelectWithConflict'
--         UniqueFoo -- (UniqueFoo undefined) or (UniqueFoo anyNumber) would also work
--         (do
--             b <- from $ table \@Bar
--             return $ Foo <# (b ^. BarNum)
--         )
--         (\\current excluded ->
--             [FooNum =. (current ^. FooNum) +. (excluded ^. FooNum)]
--         )
-- @
--
-- Inserts to table @Foo@ all @Bar.num@ values and in case of conflict @SomeFooUnique@,
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

-- | `FOR NO KEY UPDATE OF` syntax for postgres locking
-- allows locking of specific tables with a no key update lock in a view or join
--
-- @since 3.5.13.0
forNoKeyUpdateOf :: LockableEntity a => a -> OnLockedBehavior -> SqlQuery ()
forNoKeyUpdateOf lockableEntities onLockedBehavior =
  putLocking $ PostgresLockingClauses [PostgresLockingKind PostgresForNoKeyUpdate (Just $ LockingOfClause lockableEntities) onLockedBehavior]

-- | `FOR SHARE OF` syntax for postgres locking
-- allows locking of specific tables with a share lock in a view or join
--
-- @since 3.5.9.0
forShareOf :: LockableEntity a => a -> OnLockedBehavior -> SqlQuery ()
forShareOf lockableEntities onLockedBehavior =
  putLocking $ PostgresLockingClauses [PostgresLockingKind PostgresForShare (Just $ LockingOfClause lockableEntities) onLockedBehavior]

-- | `FOR KEY SHARE OF` syntax for postgres locking
-- allows locking of specific tables with a key share lock in a view or join
--
-- @since 3.5.13.0
forKeyShareOf :: LockableEntity a => a -> OnLockedBehavior -> SqlQuery ()
forKeyShareOf lockableEntities onLockedBehavior =
  putLocking $ PostgresLockingClauses [PostgresLockingKind PostgresForKeyShare (Just $ LockingOfClause lockableEntities) onLockedBehavior]

-- | @ILIKE@ operator (case-insensitive @LIKE@).
--
-- @since 2.2.3
ilike :: SqlString s => SqlExpr (Value s) -> SqlExpr (Value s) -> SqlExpr (Value Bool)
ilike   = unsafeSqlBinOp    " ILIKE "

-- | @WITH@ @MATERIALIZED@ clause is used to introduce a
-- [Common Table Expression (CTE)](https://en.wikipedia.org/wiki/Hierarchical_and_recursive_queries_in_SQL#Common_table_expression)
-- with the MATERIALIZED keyword. The MATERIALIZED keyword is only supported in PostgreSQL >= version 12.
-- In Esqueleto, CTEs should be used as a subquery memoization tactic. PostgreSQL treats a materialized CTE as an optimization fence.
-- A materialized CTE is always fully calculated, and is not "inlined" with other table joins.
-- Without the MATERIALIZED keyword, PostgreSQL >= 12 may "inline" the CTE as though it was any other join.
-- You should always verify that using a materialized CTE will in fact improve your performance
-- over a regular subquery.
--
-- @
-- select $ do
-- cte <- withMaterialized subQuery
-- cteResult <- from cte
-- where_ $ cteResult ...
-- pure cteResult
-- @
--
--
-- For more information on materialized CTEs, see the PostgreSQL manual documentation on
-- [Common Table Expression Materialization](https://www.postgresql.org/docs/14/queries-with.html#id-1.5.6.12.7).
--
-- @since 3.5.14.0
withMaterialized :: ( ToAlias a
                    , ToAliasReference a
                    , SqlSelect a r
                    ) => SqlQuery a -> SqlQuery (Ex.From a)
withMaterialized query = do
    (ret, sideData) <- Q $ W.censor (\_ -> mempty) $ W.listen $ unQ query
    aliasedValue <- toAlias ret
    let aliasedQuery = Q $ W.WriterT $ pure (aliasedValue, sideData)
    ident <- newIdentFor (DBName "cte")
    let clause = CommonTableExpressionClause NormalCommonTableExpression (\_ _ -> "MATERIALIZED ") ident (\info -> toRawSql SELECT info aliasedQuery)
    Q $ W.tell mempty{sdCteClause = [clause]}
    ref <- toAliasReference ident aliasedValue
    pure $ Ex.From $ pure (ref, (\_ info -> (useIdent info ident, mempty)))

-- | @WITH@ @NOT@ @MATERIALIZED@ clause is used to introduce a
-- [Common Table Expression (CTE)](https://en.wikipedia.org/wiki/Hierarchical_and_recursive_queries_in_SQL#Common_table_expression)
-- with the NOT MATERIALIZED keywords. These are only supported in PostgreSQL >=
-- version 12. In Esqueleto, CTEs should be used as a subquery memoization
-- tactic. PostgreSQL treats a materialized CTE as an optimization fence. A
-- MATERIALIZED CTE is always fully calculated, and is not "inlined" with other
-- table joins. Sometimes, this is undesirable, so postgres provides the NOT
-- MATERIALIZED modifier to prevent this behavior, thus enabling it to possibly
-- decide to treat the CTE as any other join.
--
-- Given the above, it is unlikely that this function will be useful, as a
-- normal join should be used instead, but is provided for completeness.
--
-- @
-- select $ do
-- cte <- withNotMaterialized subQuery
-- cteResult <- from cte
-- where_ $ cteResult ...
-- pure cteResult
-- @
--
--
-- For more information on materialized CTEs, see the PostgreSQL manual documentation on
-- [Common Table Expression Materialization](https://www.postgresql.org/docs/14/queries-with.html#id-1.5.6.12.7).
--
-- @since 3.5.14.0
withNotMaterialized :: ( ToAlias a
                    , ToAliasReference a
                    , SqlSelect a r
                    ) => SqlQuery a -> SqlQuery (Ex.From a)
withNotMaterialized query = do
    (ret, sideData) <- Q $ W.censor (\_ -> mempty) $ W.listen $ unQ query
    aliasedValue <- toAlias ret
    let aliasedQuery = Q $ W.WriterT $ pure (aliasedValue, sideData)
    ident <- newIdentFor (DBName "cte")
    let clause = CommonTableExpressionClause NormalCommonTableExpression (\_ _ -> "NOT MATERIALIZED ") ident (\info -> toRawSql SELECT info aliasedQuery)
    Q $ W.tell mempty{sdCteClause = [clause]}
    ref <- toAliasReference ident aliasedValue
    pure $ Ex.From $ pure (ref, (\_ info -> (useIdent info ident, mempty)))

-- | Ascending order of this field or SqlExpression with nulls coming first.
--
-- @since 3.5.14.0
ascNullsFirst :: PersistField a => SqlExpr (Value a) -> SqlExpr OrderBy
ascNullsFirst = orderByExpr " ASC NULLS FIRST"

-- | Ascending order of this field or SqlExpression with nulls coming last.
-- Note that this is the same as normal ascending ordering in Postgres, but it has been included for completeness.
--
-- @since 3.5.14.0
ascNullsLast :: PersistField a => SqlExpr (Value a) -> SqlExpr OrderBy
ascNullsLast = orderByExpr " ASC NULLS LAST"

-- | Descending order of this field or SqlExpression with nulls coming first.
-- Note that this is the same as normal ascending ordering in Postgres, but it has been included for completeness.
--
-- @since 3.5.14.0
descNullsFirst :: PersistField a => SqlExpr (Value a) -> SqlExpr OrderBy
descNullsFirst = orderByExpr " DESC NULLS FIRST"

-- | Descending order of this field or SqlExpression with nulls coming last.
--
-- @since 3.5.14.0
descNullsLast :: PersistField a => SqlExpr (Value a) -> SqlExpr OrderBy
descNullsLast = orderByExpr " DESC NULLS LAST"
