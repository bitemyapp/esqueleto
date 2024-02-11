{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# language DerivingStrategies, GeneralizedNewtypeDeriving #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | This is an internal module, anything exported by this module
-- may change without a major version bump.  Please use only
-- "Database.Esqueleto" if possible.
--
-- If you use this module, please report what your use case is on the issue
-- tracker so we can safely support it.
module Database.Esqueleto.Internal.Internal where

import Control.Applicative ((<|>))
import Control.Arrow (first, (***))
import Control.Exception (Exception, throw, throwIO)
import Control.Monad (MonadPlus(..), guard, void)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (MonadResource, release)
import Data.Acquire (Acquire, allocateAcquire, with)
import Data.Int (Int64)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Maybe as Maybe
#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup
#endif
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State as S
import qualified Control.Monad.Trans.Writer as W
import qualified Data.ByteString as B
import Data.Coerce (coerce)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.HashSet as HS
import Data.Kind (Type)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import Data.Proxy (Proxy(..))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Data.Typeable (Typeable)
import Database.Esqueleto.Internal.ExprParser (TableAccess(..), parseOnExpr)
import Database.Esqueleto.Internal.PersistentImport
import Database.Persist (EntityNameDB(..), FieldNameDB(..), SymbolToField(..))
import qualified Database.Persist
import Database.Persist.Sql.Util
       ( entityColumnCount
       , isIdField
       , keyAndEntityColumnNames
       , parseEntityValues
       )
import Database.Persist.SqlBackend
import GHC.Records
import GHC.TypeLits
import Text.Blaze.Html (Html)

-- | (Internal) Start a 'from' query with an entity. 'from'
-- does two kinds of magic using 'fromStart', 'fromJoin' and
-- 'fromFinish':
--
--   1.  The simple but tedious magic of allowing tuples to be
--   used.
--
--   2.  The more advanced magic of creating @JOIN@s.  The
--   @JOIN@ is processed from right to left.  The rightmost
--   entity of the @JOIN@ is created with 'fromStart'.  Each
--   @JOIN@ step is then translated into a call to 'fromJoin'.
--   In the end, 'fromFinish' is called to materialize the
--   @JOIN@.
fromStart
    :: forall a.
    ( PersistEntity a
    , BackendCompatible SqlBackend (PersistEntityBackend a)
    )
    => SqlQuery (PreprocessedFrom (SqlExpr (Entity a)))
fromStart = do
    let ed = entityDef (Proxy :: Proxy a)
    ident <- newIdentFor (coerce $ getEntityDBName ed)
    let ret = unsafeSqlEntity ident
        f' = FromStart ident ed
    return (PreprocessedFrom ret f')

-- | Copied from @persistent@
newtype DBName = DBName { unDBName :: T.Text }

-- | (Internal) Same as 'fromStart', but entity may be missing.
fromStartMaybe
    :: ( PersistEntity a
       , BackendCompatible SqlBackend (PersistEntityBackend a)
       )
    => SqlQuery (PreprocessedFrom (SqlExpr (Maybe (Entity a))))
fromStartMaybe = maybelize <$> fromStart
  where
    maybelize
        :: PreprocessedFrom (SqlExpr (Entity a))
        -> PreprocessedFrom (SqlExpr (Maybe (Entity a)))
    maybelize (PreprocessedFrom e f') = PreprocessedFrom (coerce e) f'

-- | (Internal) Do a @JOIN@.
fromJoin
    :: IsJoinKind join
    => PreprocessedFrom a
    -> PreprocessedFrom b
    -> SqlQuery (PreprocessedFrom (join a b))
fromJoin (PreprocessedFrom lhsRet lhsFrom)
         (PreprocessedFrom rhsRet rhsFrom) = Q $ do
    let ret = smartJoin lhsRet rhsRet
        from' =
            FromJoin
                lhsFrom             -- LHS
                (reifyJoinKind ret) -- JOIN
                rhsFrom             -- RHS
                Nothing             -- ON
    return (PreprocessedFrom ret from')

-- | (Internal) Finish a @JOIN@.
fromFinish
  :: PreprocessedFrom a
  -> SqlQuery a
fromFinish (PreprocessedFrom ret f') = Q $ do
    W.tell mempty { sdFromClause = [f'] }
    return ret

-- | @WHERE@ clause: restrict the query's result.
where_ :: SqlExpr (Value Bool) -> SqlQuery ()
where_ expr = Q $ W.tell mempty { sdWhereClause = Where expr }

-- | An @ON@ clause, useful to describe how two tables are related. Cross joins
-- and tuple-joins do not need an 'on' clause, but 'InnerJoin' and the various
-- outer joins do.
--
-- "Database.Esqueleto.Experimental" in version 4.0.0.0 of the library. The
-- @Experimental@ module has a dramatically improved means for introducing
-- tables and entities that provides more power and less potential for runtime
-- errors.
--
-- If you don't include an 'on' clause (or include too many!) then a runtime
-- exception will be thrown.
--
-- As an example, consider this simple join:
--
-- @
-- 'select' $
-- 'from' $ \\(foo `'InnerJoin`` bar) -> do
--   'on' (foo '^.' FooId '==.' bar '^.' BarFooId)
--   ...
-- @
--
-- We need to specify the clause for joining the two columns together. If we had
-- this:
--
-- @
-- 'select' $
-- 'from' $ \\(foo `'CrossJoin`` bar) -> do
--   ...
-- @
--
-- Then we can safely omit the 'on' clause, because the cross join will make
-- pairs of all records possible.
--
-- You can do multiple 'on' clauses in a query. This query joins three tables,
-- and has two 'on' clauses:
--
-- @
-- 'select' $
-- 'from' $ \\(foo `'InnerJoin`` bar `'InnerJoin`` baz) -> do
--   'on' (baz '^.' BazId '==.' bar '^.' BarBazId)
--   'on' (foo '^.' FooId '==.' bar '^.' BarFooId)
--   ...
-- @
--
-- Old versions of esqueleto required that you provide the 'on' clauses in
-- reverse order. This restriction has been lifted - you can now provide 'on'
-- clauses in any order, and the SQL should work itself out. The above query is
-- now totally equivalent to this:
--
-- @
-- 'select' $
-- 'from' $ \\(foo `'InnerJoin`` bar `'InnerJoin`` baz) -> do
--   'on' (foo '^.' FooId '==.' bar '^.' BarFooId)
--   'on' (baz '^.' BazId '==.' bar '^.' BarBazId)
--   ...
-- @
on :: SqlExpr (Value Bool) -> SqlQuery ()
on expr = Q $ W.tell mempty { sdFromClause = [OnClause expr] }

-- | @GROUP BY@ clause. You can enclose multiple columns
-- in a tuple.
--
-- @
-- select $ 'from' \\(foo `'InnerJoin`` bar) -> do
--   'on' (foo '^.' FooBarId '==.' bar '^.' BarId)
--   'groupBy' (bar '^.' BarId, bar '^.' BarName)
--   return (bar '^.' BarId, bar '^.' BarName, countRows)
-- @
--
-- With groupBy you can sort by aggregate functions, like so
-- (we used @let@ to restrict the more general 'countRows' to
-- @SqlSqlExpr (Value Int)@ to avoid ambiguity---the second use of
-- 'countRows' has its type restricted by the @:: Int@ below):
--
-- @
-- r \<- select $ 'from' \\(foo `'InnerJoin`` bar) -> do
--   'on' (foo '^.' FooBarId '==.' bar '^.' BarId)
--   'groupBy' $ bar '^.' BarName
--   let countRows' = 'countRows'
--   'orderBy' ['asc' countRows']
--   return (bar '^.' BarName, countRows')
-- forM_ r $ \\('Value' name, 'Value' count) -> do
--   print name
--   print (count :: Int)
-- @
--
-- === Need more columns?
--
-- The 'ToSomeValues' class is defined for 'SqlExpr' and tuples of 'SqlExpr's.
-- We only have definitions for up to 8 elements in a tuple right now, so it's
-- possible that you may need to have more than 8 elements.
--
-- For example, consider a query with a 'groupBy' call like this:
--
-- @
-- groupBy (e0, e1, e2, e3, e4, e5, e6, e7)
-- @
--
-- This is the biggest you can get with a single tuple. However, you can easily
-- nest the tuples to add more:
--
-- @
-- groupBy ((e0, e1, e2, e3, e4, e5, e6, e7), e8, e9)
-- @
groupBy :: (ToSomeValues a) => a -> SqlQuery ()
groupBy expr = Q $ W.tell mempty { sdGroupByClause = GroupBy $ toSomeValues expr }

-- | An alias for 'groupBy' that avoids conflict with the term from "Data.List"
-- 'Data.List.groupBy'.
--
-- @since 3.5.10.0
groupBy_ :: (ToSomeValues a) => a -> SqlQuery ()
groupBy_  = groupBy

-- | @ORDER BY@ clause. See also 'asc' and 'desc'.
--
-- Multiple calls to 'orderBy' get concatenated on the final
-- query, including 'distinctOnOrderBy'.
orderBy :: [SqlExpr OrderBy] -> SqlQuery ()
orderBy exprs = Q $ W.tell mempty { sdOrderByClause = exprs }

-- | Ascending order of this field or SqlExpression.
asc :: PersistField a => SqlExpr (Value a) -> SqlExpr OrderBy
asc = orderByExpr " ASC"

-- | Descending order of this field or SqlExpression.
desc :: PersistField a => SqlExpr (Value a) -> SqlExpr OrderBy
desc = orderByExpr " DESC"

orderByExpr :: TLB.Builder -> SqlExpr (Value a) -> SqlExpr OrderBy
orderByExpr orderByType (ERaw m f)
  | Just fields <- sqlExprMetaCompositeFields m =
        ERaw noMeta $ \_ info ->
            let fs = fields info
                vals = repeat []
            in uncommas' $ zip (map (<> orderByType) fs) vals
  | otherwise =
      ERaw noMeta $ \_ info ->
          first (<> orderByType) $ f Never info

-- | @LIMIT@.  Limit the number of returned rows.
limit :: Int64 -> SqlQuery ()
limit  n = Q $ W.tell mempty { sdLimitClause = Limit (Just n) Nothing  }

-- | @OFFSET@.  Usually used with 'limit'.
offset :: Int64 -> SqlQuery ()
offset n = Q $ W.tell mempty { sdLimitClause = Limit Nothing  (Just n) }

-- | @DISTINCT@.  Change the current @SELECT@ into @SELECT
-- DISTINCT@.  For example:
--
-- @
-- select $ distinct $
--   'from' \\foo -> do
--   ...
-- @
--
-- Note that this also has the same effect:
--
-- @
-- select $
--   'from' \\foo -> do
--   distinct (return ())
--   ...
-- @
--
-- @since 2.2.4
distinct :: SqlQuery a -> SqlQuery a
distinct act = Q (W.tell mempty { sdDistinctClause = DistinctStandard }) >> act

-- | @DISTINCT ON@.  Change the current @SELECT@ into
-- @SELECT DISTINCT ON (SqlExpressions)@.  For example:
--
-- @
-- select $
--   'from' \\foo ->
--   'distinctOn' ['don' (foo ^. FooName), 'don' (foo ^. FooState)] $ do
--   ...
-- @
--
-- You can also chain different calls to 'distinctOn'.  The
-- above is equivalent to:
--
-- @
-- select $
--   'from' \\foo ->
--   'distinctOn' ['don' (foo ^. FooName)] $
--   'distinctOn' ['don' (foo ^. FooState)] $ do
--   ...
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
-- Supported by PostgreSQL only.
--
-- @since 2.2.4
distinctOn :: [SqlExpr DistinctOn] -> SqlQuery a -> SqlQuery a
distinctOn exprs act = Q (W.tell mempty { sdDistinctClause = DistinctOn exprs }) >> act

-- | Erase an SqlExpression's type so that it's suitable to
-- be used by 'distinctOn'.
--
-- @since 2.2.4
don :: SqlExpr (Value a) -> SqlExpr DistinctOn
don = coerce

-- | A convenience function that calls both 'distinctOn' and
-- 'orderBy'.  In other words,
--
-- @
-- 'distinctOnOrderBy' [asc foo, desc bar, desc quux] $ do
--   ...
-- @
--
-- is the same as:
--
-- @
-- 'distinctOn' [don foo, don  bar, don  quux] $ do
--   'orderBy'  [asc foo, desc bar, desc quux]
--   ...
-- @
--
-- @since 2.2.4
distinctOnOrderBy :: [SqlExpr OrderBy] -> SqlQuery a -> SqlQuery a
distinctOnOrderBy exprs act =
    distinctOn (toDistinctOn <$> exprs) $ do
        orderBy exprs
        act
  where
    toDistinctOn :: SqlExpr OrderBy -> SqlExpr DistinctOn
    toDistinctOn (ERaw m f) = ERaw m $ \p info ->
        let (b, vals) = f p info
        in  ( TLB.fromLazyText
              $ TL.replace " DESC" ""
              $ TL.replace " ASC" ""
              $ TLB.toLazyText b
            , vals )

-- | @ORDER BY random()@ clause.
--
-- @since 1.3.10
rand :: SqlExpr OrderBy
rand = ERaw noMeta $ \_ _ -> ("RANDOM()", [])

-- | @HAVING@.
--
-- @since 1.2.2
having :: SqlExpr (Value Bool) -> SqlQuery ()
having expr = Q $ W.tell mempty { sdHavingClause = Where expr }

-- | Add a locking clause to the query.  Please read
-- 'LockingKind' documentation and your RDBMS manual.
-- Unsafe since not all locking clauses are implemented for every RDBMS
--
-- If multiple calls to 'locking' are made on the same query,
-- the last one is used.
--
-- @since 2.2.7
locking :: LockingKind -> SqlQuery ()
locking kind = putLocking $ LegacyLockingClause kind

-- | Helper to add a any type of locking clause to a query
--
-- @since 3.5.9.0
putLocking :: LockingClause -> SqlQuery ()
putLocking clause = Q $ W.tell mempty { sdLockingClause = clause }

{-#
  DEPRECATED
    sub_select
    "sub_select \n \
sub_select is an unsafe function to use. If used with a SqlQuery that \n \
returns 0 results, then it may return NULL despite not mentioning Maybe \n \
in the return type. If it returns more than 1 result, then it will throw a \n \
SQL error.\n\n Instead, consider using one of the following alternatives: \n \
- subSelect: attaches a LIMIT 1 and the Maybe return type, totally safe.  \n \
- subSelectMaybe: Attaches a LIMIT 1, useful for a query that already \n \
  has a Maybe in the return type. \n \
- subSelectCount: Performs a count of the query - this is always safe. \n \
- subSelectUnsafe: Performs no checks or guarantees. Safe to use with \n \
  countRows and friends."
  #-}
-- | Execute a subquery @SELECT@ in an SqlExpression.  Returns a
-- simple value so should be used only when the @SELECT@ query
-- is guaranteed to return just one row.
--
-- Deprecated in 3.2.0.
sub_select :: PersistField a => SqlQuery (SqlExpr (Value a)) -> SqlExpr (Value a)
sub_select         = sub SELECT

-- | Execute a subquery @SELECT@ in a 'SqlExpr'. The query passed to this
-- function will only return a single result - it has a @LIMIT 1@ passed in to
-- the query to make it safe, and the return type is 'Maybe' to indicate that
-- the subquery might result in 0 rows.
--
-- If you find yourself writing @'joinV' . 'subSelect'@, then consider using
-- 'subSelectMaybe'.
--
-- If you're performing a 'countRows', then you can use 'subSelectCount' which
-- is safe.
--
-- If you know that the subquery will always return exactly one row (eg
-- a foreign key constraint guarantees that you'll get exactly one row), then
-- consider 'subSelectUnsafe', along with a comment explaining why it is safe.
--
-- @since 3.2.0
subSelect
  :: PersistField a
  => SqlQuery (SqlExpr (Value a))
  -> SqlExpr (Value (Maybe a))
subSelect query = just (subSelectUnsafe (query <* limit 1))

-- | Execute a subquery @SELECT@ in a 'SqlExpr'. This function is a shorthand
-- for the common @'joinV' . 'subSelect'@ idiom, where you are calling
-- 'subSelect' on an expression that would be 'Maybe' already.
--
-- As an example, you would use this function when calling 'sum_' or 'max_',
-- which have 'Maybe' in the result type (for a 0 row query).
--
-- @since 3.2.0
subSelectMaybe
    :: PersistField a
    => SqlQuery (SqlExpr (Value (Maybe a)))
    -> SqlExpr (Value (Maybe a))
subSelectMaybe = joinV . subSelect

-- | Performs a @COUNT@ of the given query in a @subSelect@ manner. This is
-- always guaranteed to return a result value, and is completely safe.
--
-- @since 3.2.0
subSelectCount
    :: (Num a, PersistField a)
    => SqlQuery ignored
    -> SqlExpr (Value a)
subSelectCount query =
    subSelectUnsafe $ do
        _ <- query
        pure countRows

-- | Execute a subquery @SELECT@ in a 'SqlExpr' that returns a list. This is an
-- alias for 'subList_select' and is provided for symmetry with the other safe
-- subselect functions.
--
-- @since 3.2.0
subSelectList
    :: PersistField a
    => SqlQuery (SqlExpr (Value a))
    -> SqlExpr (ValueList a)
subSelectList = subList_select

-- | Performs a sub-select using the given foreign key on the entity. This is
-- useful to extract values that are known to be present by the database schema.
--
-- As an example, consider the following persistent definition:
--
-- @
-- User
--   profile ProfileId
--
-- Profile
--   name    Text
-- @
--
-- The following query will return the name of the user.
--
-- @
-- getUserWithName =
--     'select' $
--     'from' $ \user ->
--     'pure' (user, 'subSelectForeign' user UserProfile (^. ProfileName)
-- @
--
-- @since 3.2.0
subSelectForeign
    ::
    ( BackendCompatible SqlBackend (PersistEntityBackend val1)
    , PersistEntity val1, PersistEntity val2, PersistField a
    )
    => SqlExpr (Entity val2)
    -- ^ An expression representing the table you have access to now.
    -> EntityField val2 (Key val1)
    -- ^ The foreign key field on the table.
    -> (SqlExpr (Entity val1) -> SqlExpr (Value a))
    -- ^ A function to extract a value from the foreign reference table.
    -> SqlExpr (Value a)
subSelectForeign expr foreignKey k =
    subSelectUnsafe $
    from $ \table -> do
    where_ $ expr ^. foreignKey ==. table ^. persistIdField
    pure (k table)

-- | Execute a subquery @SELECT@ in a 'SqlExpr'. This function is unsafe,
-- because it can throw runtime exceptions in two cases:
--
-- 1. If the query passed has 0 result rows, then it will return a @NULL@ value.
--    The @persistent@ parsing operations will fail on an unexpected @NULL@.
-- 2. If the query passed returns more than one row, then the SQL engine will
--    fail with an error like "More than one row returned by a subquery used as
--    an expression".
--
-- This function is safe if you guarantee that exactly one row will be returned,
-- or if the result already has a 'Maybe' type for some reason.
--
-- For variants with the safety encoded already, see 'subSelect' and
-- 'subSelectMaybe'. For the most common safe use of this, see 'subSelectCount'.
--
-- @since 3.2.0
subSelectUnsafe :: PersistField a => SqlQuery (SqlExpr (Value a)) -> SqlExpr (Value a)
subSelectUnsafe = sub SELECT

-- | Project a field of an entity.
(^.) :: forall typ val . (PersistEntity val, PersistField typ)
    => SqlExpr (Entity val)
    -> EntityField val typ
    -> SqlExpr (Value typ)
ERaw m f ^. field
    | isIdField field = idFieldValue
    | Just alias <- sqlExprMetaAlias m =
        ERaw noMeta $ \_ info ->
            f Never info <> ("." <> useIdent info (aliasedEntityColumnIdent alias fieldDef), [])
    | otherwise = ERaw noMeta $ \_ info -> (dot info $ persistFieldDef field, [])
  where
    fieldDef =
        if isIdField field then
            -- TODO what about composite natural keys in a join this will ignore them
            NEL.head $ getEntityKeyFields ed
        else
            persistFieldDef field
    idFieldValue =
        case getEntityKeyFields ed of
            idField :| [] ->
                ERaw noMeta $ \_ info -> (dot info idField, [])

            idFields ->
                let renderedFields info = dot info <$> NEL.toList idFields
                in ERaw noMeta{ sqlExprMetaCompositeFields = Just renderedFields} $
                    \p info -> (parensM p $ uncommas $ renderedFields info, [])

    ed = entityDef $ getEntityVal (Proxy :: Proxy (SqlExpr (Entity val)))

    dot info fieldDef' =
        sourceIdent info <> "." <> fieldIdent
      where
        sourceIdent = fmap fst $ f Never
        fieldIdent
            | Just baseI <- sqlExprMetaAlias m =
                useIdent info $ aliasedEntityColumnIdent baseI fieldDef'
            | otherwise =
                fromDBName info (coerce $ fieldDB fieldDef')

-- | Project an SqlExpression that may be null, guarding against null cases.
withNonNull
    :: PersistField typ
    => SqlExpr (Value (Maybe typ))
    -> (SqlExpr (Value typ) -> SqlQuery a)
    -> SqlQuery a
withNonNull field f = do
    where_ $ not_ $ isNothing field
    f $ veryUnsafeCoerceSqlExprValue field

-- | Project a field of an entity that may be null.
(?.) :: ( PersistEntity val , PersistField typ)
    => SqlExpr (Maybe (Entity val))
    -> EntityField val typ
    -> SqlExpr (Value (Maybe typ))
ERaw m f ?. field = just (ERaw m f ^. field)

-- | Lift a constant value from Haskell-land to the query.
val  :: PersistField typ => typ -> SqlExpr (Value typ)
val v = ERaw noMeta $ \_ _ -> ("?", [toPersistValue v])

-- | @IS NULL@ comparison.
--
-- For @IS NOT NULL@, you can negate this with 'not_', as in @not_ (isNothing (person ^. PersonAge))@
--
-- Warning: Persistent and Esqueleto have different behavior for @!= Nothing@:
--
-- +----------------+----------------------------------+---------------+
-- |                | Haskell                          | SQL           |
-- +================+==================================+===============+
-- | __Persistent__ | @'Database.Persist.!=.' Nothing@ | @IS NOT NULL@ |
-- +----------------+----------------------------------+---------------+
-- | __Esqueleto__  | @'!=.' Nothing@                  | @!= NULL@     |
-- +----------------+----------------------------------+---------------+
--
-- In SQL, @= NULL@ and @!= NULL@ return NULL instead of true or false. For this reason, you very likely do not want to use @'!=.' Nothing@ in Esqueleto.
-- You may find these @hlint@ rules helpful to enforce this:
--
-- > - error: {lhs: v Database.Esqueleto.==. Database.Esqueleto.nothing, rhs: Database.Esqueleto.isNothing v, name: Use Esqueleto's isNothing}
-- > - error: {lhs: v Database.Esqueleto.==. Database.Esqueleto.val Nothing, rhs: Database.Esqueleto.isNothing v, name: Use Esqueleto's isNothing}
-- > - error: {lhs: v Database.Esqueleto.!=. Database.Esqueleto.nothing, rhs: not_ (Database.Esqueleto.isNothing v), name: Use Esqueleto's not isNothing}
-- > - error: {lhs: v Database.Esqueleto.!=. Database.Esqueleto.val Nothing, rhs: not_ (Database.Esqueleto.isNothing v), name: Use Esqueleto's not isNothing}
isNothing :: PersistField typ => SqlExpr (Value (Maybe typ)) -> SqlExpr (Value Bool)
isNothing v =
    case v of
        ERaw m f ->
            case sqlExprMetaCompositeFields m of
                Just fields ->
                    ERaw noMeta $ \p info ->
                        first (parensM p) . flip (,) [] . (intersperseB " AND " . map (<> " IS NULL")) $ fields info
                Nothing ->
                    ERaw noMeta $ \p info ->
                        first (parensM p) . isNullExpr $ f Never info
  where
    isNullExpr :: (TLB.Builder, a) -> (TLB.Builder, a)
    isNullExpr = first ((<> " IS NULL"))

-- | An alias for 'isNothing' that avoids clashing with the function from
-- "Data.Maybe" 'Data.Maybe.isNothing'.
--
-- @since 3.5.10.0
isNothing_ :: PersistField typ => SqlExpr (Value (Maybe typ)) -> SqlExpr (Value Bool)
isNothing_ = isNothing

-- | Analogous to 'Just', promotes a value of type @typ@ into
-- one of type @Maybe typ@.  It should hold that @'val' . Just
-- === just . 'val'@.
just :: SqlExpr (Value typ) -> SqlExpr (Value (Maybe typ))
just = veryUnsafeCoerceSqlExprValue

-- | @NULL@ value.
nothing :: SqlExpr (Value (Maybe typ))
nothing = unsafeSqlValue "NULL"

-- | Join nested 'Maybe's in a 'Value' into one. This is useful when
-- calling aggregate functions on nullable fields.
joinV :: SqlExpr (Value (Maybe (Maybe typ))) -> SqlExpr (Value (Maybe typ))
joinV = veryUnsafeCoerceSqlExprValue


countHelper :: Num a => TLB.Builder -> TLB.Builder -> SqlExpr (Value typ) -> SqlExpr (Value a)
countHelper open close v =
    case v of
        ERaw meta f ->
            if hasCompositeKeyMeta meta then
                countRows
            else
                countRawSql (f Never)
  where
    countRawSql :: (IdentInfo -> (TLB.Builder, [PersistValue])) -> SqlExpr (Value a)
    countRawSql x = ERaw noMeta $ \_ -> first (\b -> "COUNT" <> open <> parens b <> close) . x

-- | @COUNT(*)@ value.
countRows :: Num a => SqlExpr (Value a)
countRows = unsafeSqlValue "COUNT(*)"

-- | @COUNT@.
count :: Num a => SqlExpr (Value typ) -> SqlExpr (Value a)
count = countHelper ""           ""

-- | @COUNT(DISTINCT x)@.
--
-- @since 2.4.1
countDistinct :: Num a => SqlExpr (Value typ) -> SqlExpr (Value a)
countDistinct = countHelper "(DISTINCT " ")"

not_ :: SqlExpr (Value Bool) -> SqlExpr (Value Bool)
not_ v = ERaw noMeta (const $ first ("NOT " <>) . x)
  where
    x info =
        case v of
            ERaw m f ->
                if hasCompositeKeyMeta m then
                    throw (CompositeKeyErr NotError)
                else
                    f Parens info

(==.) :: PersistField typ => SqlExpr (Value typ) -> SqlExpr (Value typ) -> SqlExpr (Value Bool)
(==.) = unsafeSqlBinOpComposite " = " " AND "

(>=.) :: PersistField typ => SqlExpr (Value typ) -> SqlExpr (Value typ) -> SqlExpr (Value Bool)
(>=.) = unsafeSqlBinOp " >= "

(>.)  :: PersistField typ => SqlExpr (Value typ) -> SqlExpr (Value typ) -> SqlExpr (Value Bool)
(>.)  = unsafeSqlBinOp " > "

(<=.) :: PersistField typ => SqlExpr (Value typ) -> SqlExpr (Value typ) -> SqlExpr (Value Bool)
(<=.) = unsafeSqlBinOp " <= "

(<.)  :: PersistField typ => SqlExpr (Value typ) -> SqlExpr (Value typ) -> SqlExpr (Value Bool)
(<.)  = unsafeSqlBinOp " < "
(!=.) :: PersistField typ => SqlExpr (Value typ) -> SqlExpr (Value typ) -> SqlExpr (Value Bool)
(!=.) = unsafeSqlBinOpComposite " != " " OR "

(&&.) :: SqlExpr (Value Bool) -> SqlExpr (Value Bool) -> SqlExpr (Value Bool)
(&&.) = unsafeSqlBinOp " AND "

(||.) :: SqlExpr (Value Bool) -> SqlExpr (Value Bool) -> SqlExpr (Value Bool)
(||.) = unsafeSqlBinOp " OR "

(+.)  :: PersistField a => SqlExpr (Value a) -> SqlExpr (Value a) -> SqlExpr (Value a)
(+.)  = unsafeSqlBinOp " + "

(-.)  :: PersistField a => SqlExpr (Value a) -> SqlExpr (Value a) -> SqlExpr (Value a)
(-.)  = unsafeSqlBinOp " - "

(/.)  :: PersistField a => SqlExpr (Value a) -> SqlExpr (Value a) -> SqlExpr (Value a)
(/.)  = unsafeSqlBinOp " / "

(*.)  :: PersistField a => SqlExpr (Value a) -> SqlExpr (Value a) -> SqlExpr (Value a)
(*.)  = unsafeSqlBinOp " * "

-- | @BETWEEN@.
--
-- @since: 3.1.0
between :: PersistField a => SqlExpr (Value a) -> (SqlExpr (Value a), SqlExpr (Value a)) -> SqlExpr (Value Bool)
a `between` (b, c) = a >=. b &&. a <=. c

random_  :: (PersistField a, Num a) => SqlExpr (Value a)
random_  = unsafeSqlValue "RANDOM()"

round_   :: (PersistField a, Num a, PersistField b, Num b) => SqlExpr (Value a) -> SqlExpr (Value b)
round_   = unsafeSqlFunction "ROUND"

ceiling_ :: (PersistField a, Num a, PersistField b, Num b) => SqlExpr (Value a) -> SqlExpr (Value b)
ceiling_ = unsafeSqlFunction "CEILING"

floor_   :: (PersistField a, Num a, PersistField b, Num b) => SqlExpr (Value a) -> SqlExpr (Value b)
floor_   = unsafeSqlFunction "FLOOR"

sum_     :: (PersistField a, PersistField b) => SqlExpr (Value a) -> SqlExpr (Value (Maybe b))
sum_     = unsafeSqlFunction "SUM"
min_     :: (PersistField a) => SqlExpr (Value a) -> SqlExpr (Value (Maybe a))
min_     = unsafeSqlFunction "MIN"
max_     :: (PersistField a) => SqlExpr (Value a) -> SqlExpr (Value (Maybe a))
max_     = unsafeSqlFunction "MAX"
avg_     :: (PersistField a, PersistField b) => SqlExpr (Value a) -> SqlExpr (Value (Maybe b))
avg_     = unsafeSqlFunction "AVG"

-- | Allow a number of one type to be used as one of another
-- type via an implicit cast.  An explicit cast is not made,
-- this function changes only the types on the Haskell side.
--
-- /Caveat/: Trying to use @castNum@ from @Double@ to @Int@
-- will not result in an integer, the original fractional
-- number will still be used!  Use 'round_', 'ceiling_' or
-- 'floor_' instead.
--
-- /Safety/: This operation is mostly safe due to the 'Num'
-- constraint between the types and the fact that RDBMSs
-- usually allow numbers of different types to be used
-- interchangeably.  However, there may still be issues with
-- the query not being accepted by the RDBMS or @persistent@
-- not being able to parse it.
--
-- @since 2.2.9
castNum :: (Num a, Num b) => SqlExpr (Value a) -> SqlExpr (Value b)
castNum  = veryUnsafeCoerceSqlExprValue

-- | Same as 'castNum', but for nullable values.
--
-- @since 2.2.9
castNumM :: (Num a, Num b) => SqlExpr (Value (Maybe a)) -> SqlExpr (Value (Maybe b))
castNumM = veryUnsafeCoerceSqlExprValue

-- | @COALESCE@ function. Evaluates the arguments in order and
-- returns the value of the first non-NULL SqlExpression, or NULL
-- (Nothing) otherwise. Some RDBMSs (such as SQLite) require
-- at least two arguments; please refer to the appropriate
-- documentation.
--
-- @since 1.4.3
coalesce :: PersistField a => [SqlExpr (Value (Maybe a))] -> SqlExpr (Value (Maybe a))
coalesce              = unsafeSqlFunctionParens "COALESCE"

-- | Like @coalesce@, but takes a non-nullable SqlExpression
-- placed at the end of the SqlExpression list, which guarantees
-- a non-NULL result.
--
-- @since 1.4.3
coalesceDefault :: PersistField a => [SqlExpr (Value (Maybe a))] -> SqlExpr (Value a) -> SqlExpr (Value a)
coalesceDefault exprs = unsafeSqlFunctionParens "COALESCE" . (exprs ++) . return . just

-- | @LOWER@ function.
lower_ :: SqlString s => SqlExpr (Value s) -> SqlExpr (Value s)
lower_  = unsafeSqlFunction "LOWER"

-- | @UPPER@ function.
-- @since 3.3.0
upper_ :: SqlString s => SqlExpr (Value s) -> SqlExpr (Value s)
upper_  = unsafeSqlFunction "UPPER"

-- | @TRIM@ function.
-- @since 3.3.0
trim_ :: SqlString s => SqlExpr (Value s) -> SqlExpr (Value s)
trim_  = unsafeSqlFunction "TRIM"

-- | @RTRIM@ function.
-- @since 3.3.0
rtrim_ :: SqlString s => SqlExpr (Value s) -> SqlExpr (Value s)
rtrim_  = unsafeSqlFunction "RTRIM"

-- | @LTRIM@ function.
-- @since 3.3.0
ltrim_ :: SqlString s => SqlExpr (Value s) -> SqlExpr (Value s)
ltrim_  = unsafeSqlFunction "LTRIM"

-- | @LENGTH@ function.
-- @since 3.3.0
length_ :: (SqlString s, Num a) => SqlExpr (Value s) -> SqlExpr (Value a)
length_ = unsafeSqlFunction "LENGTH"

-- | @LEFT@ function.
-- @since 3.3.0
left_ :: (SqlString s, Num a) => (SqlExpr (Value s), SqlExpr (Value a)) -> SqlExpr (Value s)
left_ = unsafeSqlFunction "LEFT"

-- | @RIGHT@ function.
-- @since 3.3.0
right_ :: (SqlString s, Num a) => (SqlExpr (Value s), SqlExpr (Value a)) -> SqlExpr (Value s)
right_ = unsafeSqlFunction "RIGHT"

-- | @LIKE@ operator.
like :: SqlString s => SqlExpr (Value s) -> SqlExpr (Value s) -> SqlExpr (Value Bool)
like    = unsafeSqlBinOp    " LIKE "

-- | @ILIKE@ operator (case-insensitive @LIKE@).
--
-- Supported by PostgreSQL only.
--
-- @since 2.2.3
ilike :: SqlString s => SqlExpr (Value s) -> SqlExpr (Value s) -> SqlExpr (Value Bool)
ilike   = unsafeSqlBinOp    " ILIKE "

-- | The string @'%'@.  May be useful while using 'like' and
-- concatenation ('concat_' or '++.', depending on your
-- database).  Note that you always have to type the parenthesis,
-- for example:
--
-- @
-- name `'like`` (%) ++. 'val' \"John\" ++. (%)
-- @
(%) :: SqlString s => SqlExpr (Value s)
(%)     = unsafeSqlValue    "'%'"

-- | The @CONCAT@ function with a variable number of
-- parameters.  Supported by MySQL and PostgreSQL.
concat_ :: SqlString s => [SqlExpr (Value s)] -> SqlExpr (Value s)
concat_ = unsafeSqlFunction "CONCAT"

-- | The @||@ string concatenation operator (named after
-- Haskell's '++' in order to avoid naming clash with '||.').
-- Supported by SQLite and PostgreSQL.
(++.) :: SqlString s => SqlExpr (Value s) -> SqlExpr (Value s) -> SqlExpr (Value s)
(++.)   = unsafeSqlBinOp    " || "

-- | Cast a string type into 'Text'.  This function
-- is very useful if you want to use @newtype@s, or if you want
-- to apply functions such as 'like' to strings of different
-- types.
--
-- /Safety:/ This is a slightly unsafe function, especially if
-- you have defined your own instances of 'SqlString'.  Also,
-- since 'Maybe' is an instance of 'SqlString', it's possible
-- to turn a nullable value into a non-nullable one.  Avoid
-- using this function if possible.
castString :: (SqlString s, SqlString r) => SqlExpr (Value s) -> SqlExpr (Value r)
castString = veryUnsafeCoerceSqlExprValue

-- | Execute a subquery @SELECT@ in an SqlExpression.  Returns a
-- list of values.
subList_select :: PersistField a => SqlQuery (SqlExpr (Value a)) -> SqlExpr (ValueList a)
subList_select query = ERaw noMeta $ \_ info -> first parens $ toRawSql SELECT info query


-- | Lift a list of constant value from Haskell-land to the query.
valList :: PersistField typ => [typ] -> SqlExpr (ValueList typ)
valList []   = ERaw noMeta $ \_ _ -> ("()", [])
valList vals = ERaw noMeta $ \p -> const (parensM p (uncommas ("?" <$ vals)), map toPersistValue vals )

-- | Same as 'just' but for 'ValueList'.  Most of the time you
-- won't need it, though, because you can use 'just' from
-- inside 'subList_select' or 'Just' from inside 'valList'.
--
-- @since 2.2.12
justList :: SqlExpr (ValueList typ) -> SqlExpr (ValueList (Maybe typ))
justList (ERaw m f) = ERaw m f

-- | @IN@ operator. For example if you want to select all @Person@s by a list
-- of IDs:
--
-- @
-- SELECT *
-- FROM Person
-- WHERE Person.id IN (?)
-- @
--
-- In @esqueleto@, we may write the same query above as:
--
-- @
-- select $
-- 'from' $ \\person -> do
-- 'where_' $ person '^.' PersonId `'in_`` 'valList' personIds
-- return person
-- @
--
-- Where @personIds@ is of type @[Key Person]@.
in_ :: PersistField typ => SqlExpr (Value typ) -> SqlExpr (ValueList typ) -> SqlExpr (Value Bool)
(ERaw _ v) `in_` (ERaw _ list) =
    ERaw noMeta $ \_ info ->
        let (b1, vals1) = v Parens info
            (b2, vals2) = list Parens info
        in
        if b2 == "()" then
            ("FALSE", [])
        else
            (b1 <> " IN " <> b2, vals1 <> vals2)

-- | @NOT IN@ operator.
notIn :: PersistField typ => SqlExpr (Value typ) -> SqlExpr (ValueList typ) -> SqlExpr (Value Bool)
(ERaw _ v) `notIn` (ERaw _ list) =
    ERaw noMeta $ \_ info ->
        let (b1, vals1) = v Parens info
            (b2, vals2) = list Parens info
        in
        if b2 == "()" then
            ("TRUE", [])
        else
            (b1 <> " NOT IN " <> b2, vals1 <> vals2)

-- | @EXISTS@ operator.  For example:
--
-- @
-- select $
-- 'from' $ \\person -> do
-- 'where_' $ 'exists' $
--          'from' $ \\post -> do
--          'where_' (post '^.' BlogPostAuthorId '==.' person '^.' PersonId)
-- return person
-- @
exists :: SqlQuery () -> SqlExpr (Value Bool)
exists q = ERaw noMeta $ \p info ->
    let ERaw _ f = existsHelper q
        (b, vals) = f Never info
    in ( parensM p $ "EXISTS " <> b, vals)

-- | @NOT EXISTS@ operator.
notExists :: SqlQuery () -> SqlExpr (Value Bool)
notExists q = ERaw noMeta $ \p info ->
    let ERaw _ f = existsHelper q
        (b, vals) = f Never info
    in ( parensM p $ "NOT EXISTS " <> b, vals)

-- | @SET@ clause used on @UPDATE@s.  Note that while it's not
-- a type error to use this function on a @SELECT@, it will
-- most certainly result in a runtime error.
set :: PersistEntity val => SqlExpr (Entity val) -> [SqlExpr (Entity val) -> SqlExpr Update] -> SqlQuery ()
set ent upds = Q $ W.tell mempty { sdSetClause = map apply upds }
  where
    apply f = SetClause (f ent)

(=.)  :: (PersistEntity val, PersistField typ) => EntityField val typ -> SqlExpr (Value typ) -> (SqlExpr (Entity val) -> SqlExpr Update )
field  =. expr = setAux field (const expr)

(+=.) :: (PersistEntity val, PersistField a) => EntityField val a -> SqlExpr (Value a) -> (SqlExpr (Entity val) -> SqlExpr Update)
field +=. expr = setAux field (\ent -> ent ^. field +. expr)

(-=.) :: (PersistEntity val, PersistField a) => EntityField val a -> SqlExpr (Value a) -> (SqlExpr (Entity val) -> SqlExpr Update)
field -=. expr = setAux field (\ent -> ent ^. field -. expr)

(*=.) :: (PersistEntity val, PersistField a) => EntityField val a -> SqlExpr (Value a) -> (SqlExpr (Entity val) -> SqlExpr Update)
field *=. expr = setAux field (\ent -> ent ^. field *. expr)

(/=.) :: (PersistEntity val, PersistField a) => EntityField val a -> SqlExpr (Value a) -> (SqlExpr (Entity val) -> SqlExpr Update)
field /=. expr = setAux field (\ent -> ent ^. field /. expr)

-- | Apply a 'PersistField' constructor to @SqlExpr Value@ arguments.
(<#) :: (a -> b) -> SqlExpr (Value a) -> SqlExpr (Insertion b)
(<#) _ (ERaw _ f)        = ERaw noMeta f

-- | Apply extra @SqlExpr Value@ arguments to a 'PersistField' constructor
(<&>) :: SqlExpr (Insertion (a -> b)) -> SqlExpr (Value a) -> SqlExpr (Insertion b)
(ERaw _ f) <&> (ERaw _ g) =
    ERaw noMeta $ \_ info ->
        let (fb, fv) = f Never info
            (gb, gv) = g Never info
        in (fb <> ", " <> gb, fv ++ gv)

-- | @CASE@ statement.  For example:
--
-- @
-- select $
-- return $
-- 'case_'
--    [ 'when_'
--        ('exists' $
--        'from' $ \\p -> do
--        'where_' (p '^.' PersonName '==.' 'val' \"Mike\"))
--      'then_'
--        ('sub_select' $
--        'from' $ \\v -> do
--        let sub =
--                'from' $ \\c -> do
--                'where_' (c '^.' PersonName '==.' 'val' \"Mike\")
--                return (c '^.' PersonFavNum)
--        'where_' (v '^.' PersonFavNum >. 'sub_select' sub)
--        return $ 'count' (v '^.' PersonName) +. 'val' (1 :: Int)) ]
--    ('else_' $ 'val' (-1))
-- @
--
-- This query is a bit complicated, but basically it checks if a person
-- named @\"Mike\"@ exists, and if that person does, run the subquery to find
-- out how many people have a ranking (by Fav Num) higher than @\"Mike\"@.
--
-- __NOTE:__ There are a few things to be aware about this statement.
--
--    * This only implements the full CASE statement, it does not
--      implement the \"simple\" CASE statement.
--
--
--    * At least one 'when_' and 'then_' is mandatory otherwise it will
--      emit an error.
--
--
--    * The 'else_' is also mandatory, unlike the SQL statement in which
--      if the @ELSE@ is omitted it will return a @NULL@. You can
--      reproduce this via 'nothing'.
--
-- @since 2.1.2
case_ :: PersistField a => [(SqlExpr (Value Bool), SqlExpr (Value a))] -> SqlExpr (Value a) -> SqlExpr (Value a)
case_ = unsafeSqlCase

-- | Convert an entity's key into another entity's.
--
-- This function is to be used when you change an entity's @Id@ to be
-- that of another entity. For example:
--
-- @
-- Bar
--   barNum Int
-- Foo
--   bar BarId
--   fooNum Int
--   Primary bar
-- @
--
-- In this example, Bar is said to be the BaseEnt(ity), and Foo the child.
-- To model this in Esqueleto, declare:
--
-- @
-- instance ToBaseId Foo where
--   type BaseEnt Foo = Bar
--   toBaseIdWitness barId = FooKey barId
-- @
--
-- Now you're able to write queries such as:
--
-- @
-- 'select' $
-- 'from' $ \(bar `'InnerJoin`` foo) -> do
-- 'on' ('toBaseId' (foo '^.' FooId) '==.' bar '^.' BarId)
-- return (bar, foo)
-- @
--
-- Note: this function may be unsafe to use in conditions not like the
-- one of the example above.
--
-- @since 2.4.3
toBaseId :: ToBaseId ent => SqlExpr (Value (Key ent)) -> SqlExpr (Value (Key (BaseEnt ent)))
toBaseId = veryUnsafeCoerceSqlExprValue

{-# DEPRECATED random_ "Since 2.6.0: `random_` is not uniform across all databases! Please use a specific one such as 'Database.Esqueleto.PostgreSQL.random_', 'Database.Esqueleto.MySQL.random_', or 'Database.Esqueleto.SQLite.random_'" #-}

{-# DEPRECATED rand "Since 2.6.0: `rand` ordering function is not uniform across all databases! To avoid accidental partiality it will be removed in the next major version." #-}

-- Fixity declarations
infixl 9 ^.
infixl 7 *., /.
infixl 6 +., -.
infixr 5 ++.
infix  4 ==., >=., >., <=., <., !=.
infixr 3 &&., =., +=., -=., *=., /=.
infixr 2 ||., `like`, `ilike`
infixl 2 `InnerJoin`, `CrossJoin`, `LeftOuterJoin`, `RightOuterJoin`, `FullOuterJoin`

-- | Syntax sugar for 'case_'.
--
-- @since 2.1.2
when_ :: expr (Value Bool) -> () -> expr a -> (expr (Value Bool), expr a)
when_ cond _ expr = (cond, expr)

-- | Syntax sugar for 'case_'.
--
-- @since 2.1.2
then_ :: ()
then_ = ()

-- | Syntax sugar for 'case_'.
--
-- @since 2.1.2
else_ :: expr a -> expr a
else_ = id

-- | A single value (as opposed to a whole entity).  You may use
-- @('^.')@ or @('?.')@ to get a 'Value' from an 'Entity'.
newtype Value a = Value { unValue :: a } deriving (Eq, Ord, Show, Typeable)

-- | @since 1.4.4
instance Functor Value where
    fmap f (Value a) = Value (f a)

instance Applicative Value where
  (<*>) (Value f) (Value a) = Value (f a)
  pure = Value

instance Monad Value where
  (>>=) x f = valueJoin $ fmap f x
    where valueJoin (Value v) = v

-- | A list of single values.  There's a limited set of functions
-- able to work with this data type (such as 'subList_select',
-- 'valList', 'in_' and 'exists').
newtype ValueList a = ValueList a deriving (Eq, Ord, Show, Typeable)

-- | A wrapper type for for any @expr (Value a)@ for all a.
data SomeValue where
    SomeValue :: SqlExpr (Value a) -> SomeValue

-- | A class of things that can be converted into a list of SomeValue. It has
-- instances for tuples and is the reason why 'groupBy' can take tuples, like
-- @'groupBy' (foo '^.' FooId, foo '^.' FooName, foo '^.' FooType)@.
class ToSomeValues a where
    toSomeValues :: a -> [SomeValue]

instance
    ( ToSomeValues a
    , ToSomeValues b
    )
  =>
    ToSomeValues (a, b)
  where
    toSomeValues (a,b) = toSomeValues a ++ toSomeValues b

instance
    ( ToSomeValues a
    , ToSomeValues b
    , ToSomeValues c
    )
  =>
    ToSomeValues (a, b, c)
  where
    toSomeValues (a,b,c) = toSomeValues a ++ toSomeValues b ++ toSomeValues c

instance
    ( ToSomeValues a
    , ToSomeValues b
    , ToSomeValues c
    , ToSomeValues d
    )
  =>
    ToSomeValues (a, b, c, d)
  where
    toSomeValues (a,b,c,d) =
        toSomeValues a ++ toSomeValues b ++ toSomeValues c ++ toSomeValues d

instance
    ( ToSomeValues a
    , ToSomeValues b
    , ToSomeValues c
    , ToSomeValues d
    , ToSomeValues e
    )
  =>
    ToSomeValues (a, b, c, d, e)
  where
    toSomeValues (a,b,c,d,e) = concat
        [ toSomeValues a, toSomeValues b, toSomeValues c , toSomeValues d
        , toSomeValues e
        ]

instance
    ( ToSomeValues a
    , ToSomeValues b
    , ToSomeValues c
    , ToSomeValues d
    , ToSomeValues e
    , ToSomeValues f
    )
  =>
    ToSomeValues (a, b, c, d, e, f)
  where
    toSomeValues (a,b,c,d,e,f) = concat
        [ toSomeValues a, toSomeValues b, toSomeValues c, toSomeValues d
        , toSomeValues e , toSomeValues f
        ]

instance
    ( ToSomeValues a
    , ToSomeValues b
    , ToSomeValues c
    , ToSomeValues d
    , ToSomeValues e
    , ToSomeValues f
    , ToSomeValues g
    )
  =>
    ToSomeValues (a, b, c, d, e, f, g)
  where
    toSomeValues (a,b,c,d,e,f,g) = concat
        [ toSomeValues a,  toSomeValues b, toSomeValues c,  toSomeValues d
        , toSomeValues e,  toSomeValues f, toSomeValues g
        ]

instance
    ( ToSomeValues a
    , ToSomeValues b
    , ToSomeValues c
    , ToSomeValues d
    , ToSomeValues e
    , ToSomeValues f
    , ToSomeValues g
    , ToSomeValues h
    )
  =>
    ToSomeValues (a, b, c, d, e, f, g, h)
  where
    toSomeValues (a,b,c,d,e,f,g,h) = concat
        [ toSomeValues a, toSomeValues b, toSomeValues c, toSomeValues d
        , toSomeValues e, toSomeValues f, toSomeValues g, toSomeValues h
        ]

type family KnowResult a where
    KnowResult (i -> o) = KnowResult o
    KnowResult a = a

-- | A class for constructors or function which result type is known.
--
-- @since 3.1.3
class FinalResult a where
  finalR :: a -> KnowResult a

instance FinalResult (Unique val) where
  finalR = id

instance (FinalResult b) => FinalResult (a -> b) where
  finalR f = finalR (f undefined)

-- | Convert a constructor for a 'Unique' key on a record to the 'UniqueDef'
-- that defines it. You can supply just the constructor itself, or a value of
-- the type - the library is capable of figuring it out from there.
--
-- @since 3.1.3
toUniqueDef
    :: forall a val.
    ( KnowResult a ~ Unique val
    , PersistEntity val
    , FinalResult a
    )
    => a
    -> UniqueDef
toUniqueDef uniqueConstructor = uniqueDef
  where
    proxy :: Proxy val
    proxy = Proxy
    unique :: Unique val
    unique = finalR uniqueConstructor
    -- there must be a better way to get the constrain name from a unique, make this not a list search
    filterF = (==) (persistUniqueToFieldNames unique) . uniqueFields
    uniqueDef = head . filter filterF . getEntityUniques . entityDef $ proxy

-- | Render updates to be use in a SET clause for a given sql backend.
--
-- @since 3.1.3
renderUpdates
    :: (BackendCompatible SqlBackend backend)
    => backend
    -> [SqlExpr (Entity val) -> SqlExpr Update]
    -> (TLB.Builder, [PersistValue])
renderUpdates conn = uncommas' . concatMap renderUpdate
  where
    mk :: SqlExpr Update -> [(TLB.Builder, [PersistValue])]
    mk (ERaw _ f)             = [f Never info]

    renderUpdate :: (SqlExpr (Entity val) -> SqlExpr Update) -> [(TLB.Builder, [PersistValue])]
    renderUpdate f = mk (f undefined) -- second parameter of f is always unused
    info = (projectBackend conn, initialIdentState)

-- | Data type that represents an @INNER JOIN@ (see 'LeftOuterJoin' for an example).
data InnerJoin a b = a `InnerJoin` b

-- | Data type that represents a @CROSS JOIN@ (see 'LeftOuterJoin' for an example).
data CrossJoin a b = a `CrossJoin` b

-- | Data type that represents a @LEFT OUTER JOIN@. For example,
--
-- @
-- select $
-- 'from' $ \\(person `'LeftOuterJoin`` pet) ->
--   ...
-- @
--
-- is translated into
--
-- @
-- SELECT ...
-- FROM Person LEFT OUTER JOIN Pet
-- ...
-- @
--
-- See also: 'from'.
data LeftOuterJoin a b = a `LeftOuterJoin` b

-- | Data type that represents a @RIGHT OUTER JOIN@ (see 'LeftOuterJoin' for an example).
data RightOuterJoin a b = a `RightOuterJoin` b

-- | Data type that represents a @FULL OUTER JOIN@ (see 'LeftOuterJoin' for an example).
data FullOuterJoin a b = a `FullOuterJoin` b


-- | (Internal) A kind of @JOIN@.
data JoinKind
    = InnerJoinKind      -- ^ @INNER JOIN@
    | CrossJoinKind      -- ^ @CROSS JOIN@
    | LeftOuterJoinKind  -- ^ @LEFT OUTER JOIN@
    | RightOuterJoinKind -- ^ @RIGHT OUTER JOIN@
    | FullOuterJoinKind  -- ^ @FULL OUTER JOIN@
    deriving (Eq, Show)


-- | (Internal) Functions that operate on types (that should be)
-- of kind 'JoinKind'.
class IsJoinKind join where
    -- | (Internal) @smartJoin a b@ is a @JOIN@ of the correct kind.
    smartJoin :: a -> b -> join a b
    -- | (Internal) Reify a @JoinKind@ from a @JOIN@.  This
    -- function is non-strict.
    reifyJoinKind :: join a b -> JoinKind

instance IsJoinKind InnerJoin where
    smartJoin a b = a `InnerJoin` b
    reifyJoinKind _ = InnerJoinKind
instance IsJoinKind CrossJoin where
    smartJoin a b = a `CrossJoin` b
    reifyJoinKind _ = CrossJoinKind
instance IsJoinKind LeftOuterJoin where
    smartJoin a b = a `LeftOuterJoin` b
    reifyJoinKind _ = LeftOuterJoinKind
instance IsJoinKind RightOuterJoin where
    smartJoin a b = a `RightOuterJoin` b
    reifyJoinKind _ = RightOuterJoinKind
instance IsJoinKind FullOuterJoin where
    smartJoin a b = a `FullOuterJoin` b
    reifyJoinKind _ = FullOuterJoinKind


-- | Exception thrown whenever 'on' is used to create an @ON@
-- clause but no matching @JOIN@ is found.
data OnClauseWithoutMatchingJoinException =
    OnClauseWithoutMatchingJoinException String
    deriving (Eq, Ord, Show, Typeable)

instance Exception OnClauseWithoutMatchingJoinException

-- | Phantom type used by 'orderBy', 'asc' and 'desc'.
data OrderBy

-- | Phantom type used by 'distinctOn' and 'don'.
data DistinctOn

-- | Phantom type for a @SET@ operation on an entity of the given
-- type (see 'set' and '(=.)').
data Update

-- | Phantom type used by 'insertSelect'.
data Insertion a

-- | A left-precedence pair. Pronounced \"and\". Used to represent expressions
-- that have been joined together.
--
-- The precedence behavior can be demonstrated by:
--
-- @
-- a :& b :& c == ((a :& b) :& c)
-- @
--
-- See the examples at the beginning of this module to see how this
-- operator is used in 'JOIN' operations.
data (:&) a b = a :& b
    deriving (Eq, Show)
infixl 2 :&

-- | Different kinds of locking clauses supported by 'locking'.
--
-- Note that each RDBMS has different locking support.  The
-- constructors of this datatype specify only the /syntax/ of the
-- locking mechanism, not its /semantics/.  For example, even
-- though both MySQL and PostgreSQL support 'ForUpdate', there
-- are no guarantees that they will behave the same.
--
-- @since 2.2.7
data LockingKind
    = ForUpdate
      -- ^ @FOR UPDATE@ syntax.  Supported by MySQL, Oracle and
      -- PostgreSQL.
      --
      -- @since 2.2.7
    | ForUpdateSkipLocked
      -- ^ @FOR UPDATE SKIP LOCKED@ syntax.  Supported by MySQL, Oracle and
      -- PostgreSQL.
      --
      -- @since 2.2.7
    | ForShare
      -- ^ @FOR SHARE@ syntax.  Supported by PostgreSQL.
      --
      -- @since 2.2.7
    | LockInShareMode
      -- ^ @LOCK IN SHARE MODE@ syntax.  Supported by MySQL.
      --
      -- @since 2.2.7

-- | Postgres specific locking, used only internally
--
-- @since 3.5.9.0
data PostgresLockingKind =
    PostgresLockingKind
        {
          postgresRowLevelLockStrength :: PostgresRowLevelLockStrength
        , postgresLockingOfClause :: Maybe LockingOfClause
        , postgresOnLockedBehavior :: OnLockedBehavior
        }

-- Arranged in order of lock strength
data PostgresRowLevelLockStrength =
    PostgresForUpdate
    | PostgresForShare
  deriving (Ord, Eq)

data LockingOfClause where
    LockingOfClause :: LockableEntity a => a -> LockingOfClause

data OnLockedBehavior =
    NoWait
    -- ^ @NOWAIT@ syntax locking behaviour.
    --  query excutes immediately failing on locked rows
    --
    -- @since 3.5.9.0
      | SkipLocked
    -- ^ @SKIP LOCKED@ syntax locking behaviour.
    --  query skips locked rows
    --
    -- @since 3.5.9.0
      | Wait
    -- ^ default locking behaviour.
    --  query will wait on locked rows
    --
    -- @since 3.5.9.0
      deriving (Ord, Eq, Show)


-- | Lockable entity
--
-- Example use:
--
-- @
-- select $ do
--     (p :& bp) <- from $
--         table @Person
--         `innerJoin` table @BlogPost
--             `on` do
--                 \(p :& bp) -> p ^. PersonId ==. b ^. BlogPostAuthorId
--     forUpdateOf (p :& b) skipLocked
--     return p
-- @
class LockableEntity a where
    flattenLockableEntity :: a -> NonEmpty LockableSqlExpr

makeLockableEntity :: LockableEntity a => IdentInfo -> a -> (TLB.Builder, [PersistValue])
makeLockableEntity info lockableEntity =
    uncommas' $ Set.toList . Set.fromList $ (\(LockableSqlExpr (ERaw _ f)) -> f Never info) <$> NEL.toList (flattenLockableEntity lockableEntity)

instance PersistEntity val => LockableEntity (SqlExpr (Entity val)) where
    flattenLockableEntity e = LockableSqlExpr e :| []

instance (LockableEntity a, LockableEntity b) => LockableEntity (a :& b) where
    flattenLockableEntity (a :& b) = flattenLockableEntity a <> flattenLockableEntity b

data LockableSqlExpr where
    LockableSqlExpr :: PersistEntity val => (SqlExpr (Entity val)) -> LockableSqlExpr

-- | Phantom class of data types that are treated as strings by the
-- RDBMS.  It has no methods because it's only used to avoid type
-- errors such as trying to concatenate integers.
--
-- If you have a custom data type or @newtype@, feel free to make
-- it an instance of this class.
--
-- @since 2.4.0
class PersistField a => SqlString a where

-- | @since 2.3.0
instance (a ~ Char) => SqlString [a] where

-- | @since 2.3.0
instance SqlString T.Text where

-- | @since 2.3.0
instance SqlString TL.Text where

-- | @since 2.3.0
instance SqlString B.ByteString where

-- | @since 2.3.0
instance SqlString Html where

-- | @since 2.4.0
instance SqlString a => SqlString (Maybe a) where

-- | Class that enables one to use 'toBaseId' to convert an entity's
-- key on a query into another (cf. 'toBaseId').
class ToBaseId ent where
    -- | e.g. @type BaseEnt MyBase = MyChild@
    type BaseEnt ent :: Type
    -- | Convert from the key of the BaseEnt(ity) to the key of the child entity.
    -- This function is not actually called, but that it typechecks proves this operation is safe.
    toBaseIdWitness :: Key (BaseEnt ent) -> Key ent


-- | @FROM@ clause: bring entities into scope.
--
-- Note that this function will be replaced by the one in
-- "Database.Esqueleto.Experimental" in version 4.0.0.0 of the library. The
-- @Experimental@ module has a dramatically improved means for introducing
-- tables and entities that provides more power and less potential for runtime
-- errors.
--
-- This function internally uses two type classes in order to
-- provide some flexibility of how you may call it.  Internally
-- we refer to these type classes as the two different magics.
--
-- The innermost magic allows you to use @from@ with the
-- following types:
--
--  * @expr (Entity val)@, which brings a single entity into
--  scope.
--
--  * @expr (Maybe (Entity val))@, which brings a single entity
--  that may be @NULL@ into scope.  Used for @OUTER JOIN@s.
--
--  * A @JOIN@ of any other two types allowed by the innermost
--  magic, where a @JOIN@ may be an 'InnerJoin', a 'CrossJoin', a
--  'LeftOuterJoin', a 'RightOuterJoin', or a 'FullOuterJoin'.
--  The @JOINs@ have left fixity.
--
-- The outermost magic allows you to use @from@ on any tuples of
-- types supported by innermost magic (and also tuples of tuples,
-- and so on), up to 8-tuples.
--
-- Note that using @from@ for the same entity twice does work and
-- corresponds to a self-join.  You don't even need to use two
-- different calls to @from@, you may use a @JOIN@ or a tuple.
--
-- The following are valid examples of uses of @from@ (the types
-- of the arguments of the lambda are inside square brackets):
--
-- @
-- 'from' $ \\person -> ...
-- 'from' $ \\(person, blogPost) -> ...
-- 'from' $ \\(p `'LeftOuterJoin`` mb) -> ...
-- 'from' $ \\(p1 `'InnerJoin`` f `'InnerJoin`` p2) -> ...
-- 'from' $ \\((p1 `'InnerJoin`` f) `'InnerJoin`` p2) -> ...
-- @
--
-- The types of the arguments to the lambdas above are,
-- respectively:
--
-- @
-- person
--   :: ( Esqueleto query expr backend
--      , PersistEntity Person
--      , PersistEntityBackend Person ~ backend
--      ) => expr (Entity Person)
-- (person, blogPost)
--   :: (...) => (expr (Entity Person), expr (Entity BlogPost))
-- (p `'LeftOuterJoin`` mb)
--   :: (...) => InnerJoin (expr (Entity Person)) (expr (Maybe (Entity BlogPost)))
-- (p1 `'InnerJoin`` f `'InnerJoin`` p2)
--   :: (...) => InnerJoin
--                 (InnerJoin (expr (Entity Person))
--                            (expr (Entity Follow)))
--                 (expr (Entity Person))
-- (p1 `'InnerJoin`` (f `'InnerJoin`` p2)) ::
--   :: (...) => InnerJoin
--                 (expr (Entity Person))
--                 (InnerJoin (expr (Entity Follow))
--                            (expr (Entity Person)))
-- @
--
-- Note that some backends may not support all kinds of @JOIN@s.
from :: From a => (a -> SqlQuery b) -> SqlQuery b
from = (from_ >>=)


-- | (Internal) Class that implements the tuple 'from' magic (see
-- 'fromStart').
class From a where
    from_ :: SqlQuery a

instance
    ( FromPreprocess (SqlExpr (Entity val))
    )
  =>
    From (SqlExpr (Entity val))
  where
    from_ = fromPreprocess >>= fromFinish

instance
    ( FromPreprocess (SqlExpr (Maybe (Entity val)))
    )
  =>
    From (SqlExpr (Maybe (Entity val)))
  where
    from_ = fromPreprocess >>= fromFinish

instance
    ( FromPreprocess (InnerJoin a b)
    )
  =>
    From (InnerJoin a b)
  where
    from_ = fromPreprocess >>= fromFinish

instance
    ( FromPreprocess (CrossJoin a b)
    )
  =>
    From (CrossJoin a b)
  where
    from_ = fromPreprocess >>= fromFinish

instance (FromPreprocess (LeftOuterJoin a b)) => From (LeftOuterJoin a b) where
    from_ = fromPreprocess >>= fromFinish

instance (FromPreprocess (RightOuterJoin a b)) => From (RightOuterJoin a b) where
    from_ = fromPreprocess >>= fromFinish

instance (FromPreprocess (FullOuterJoin a b)) => From (FullOuterJoin a b) where
    from_ = fromPreprocess >>= fromFinish

instance (From a, From b) => From (a, b) where
    from_ = (,) <$> from_ <*> from_

instance (From a, From b, From c) => From (a, b, c) where
    from_ = (,,) <$> from_ <*> from_ <*> from_

instance (From a, From b, From c, From d) => From (a, b, c, d) where
    from_ = (,,,) <$> from_ <*> from_ <*> from_ <*> from_

instance (From a, From b, From c, From d, From e) => From (a, b, c, d, e) where
    from_ = (,,,,) <$> from_ <*> from_ <*> from_ <*> from_ <*> from_

instance
    (From a, From b, From c, From d, From e, From f)
  =>
    From (a, b, c, d, e, f)
  where
    from_ = (,,,,,) <$> from_ <*> from_ <*> from_ <*> from_ <*> from_ <*> from_

instance
    (From a, From b, From c, From d, From e, From f, From g)
  =>
    From (a, b, c, d, e, f, g)
  where
    from_ =
        (,,,,,,) <$> from_ <*> from_ <*> from_ <*> from_ <*> from_ <*> from_ <*> from_

instance
    (From a, From b, From c, From d, From e, From f, From g, From h)
  =>
    From (a, b, c, d, e, f, g, h)
  where
    from_ =
        (,,,,,,,) <$> from_ <*> from_ <*> from_ <*> from_ <*> from_ <*> from_ <*> from_ <*> from_



-- | (Internal) Class that implements the @JOIN@ 'from' magic
-- (see 'fromStart').
class FromPreprocess a where
    fromPreprocess :: SqlQuery (PreprocessedFrom a)

instance
    (PersistEntity val, BackendCompatible SqlBackend (PersistEntityBackend val))
  =>
    FromPreprocess (SqlExpr (Entity val))
  where
    fromPreprocess = fromStart

instance
    (PersistEntity val, BackendCompatible SqlBackend (PersistEntityBackend val))
  =>
    FromPreprocess (SqlExpr (Maybe (Entity val)))
  where
    fromPreprocess = fromStartMaybe

instance
    (FromPreprocess a, FromPreprocess b, IsJoinKind join)
  =>
    FromPreprocess (join a b)
  where
    fromPreprocess = do
        a <- fromPreprocess
        b <- fromPreprocess
        fromJoin a b

-- | Exception data type for @esqueleto@ internal errors
data EsqueletoError
    = CompositeKeyErr CompositeKeyError
    | AliasedValueErr UnexpectedValueError
    | UnexpectedCaseErr UnexpectedCaseError
    | SqlBinOpCompositeErr SqlBinOpCompositeError
    deriving (Show)

instance Exception EsqueletoError

data UnexpectedValueError
    = NotError
    | ToInsertionError
    | CombineInsertionError
    | FoldHelpError
    | SqlCaseError
    | SqlCastAsError
    | SqlFunctionError
    | MakeOnClauseError
    | MakeExcError
    | MakeSetError
    | MakeWhereError
    | MakeHavingError
    | FilterWhereAggError
    | FilterWhereClauseError
    deriving (Show)

type CompositeKeyError = UnexpectedValueError

data UnexpectedCaseError
    = EmptySqlExprValueList
    | MakeFromError
    | UnsupportedSqlInsertIntoType
    | InsertionFinalError
    | NewIdentForError
    | UnsafeSqlCaseError
    | OperationNotSupported
    | NotImplemented
    deriving (Show)

data SqlBinOpCompositeError
    = MismatchingLengthsError
    | NullPlaceholdersError
    | DeconstructionError
    deriving (Show)

-- | SQL backend for @esqueleto@ using 'SqlPersistT'.
newtype SqlQuery a = Q { unQ :: W.WriterT SideData (S.State IdentState) a }
    deriving newtype (Functor, Applicative, Monad)

-- | Constraint synonym for @persistent@ entities whose backend
-- is 'SqlBackend'.
type SqlEntity ent = (PersistEntity ent, PersistEntityBackend ent ~ SqlBackend)


----------------------------------------------------------------------


-- | Side data written by 'SqlQuery'.
data SideData = SideData
    { sdDistinctClause :: !DistinctClause
    , sdFromClause     :: ![FromClause]
    , sdSetClause      :: ![SetClause]
    , sdWhereClause    :: !WhereClause
    , sdGroupByClause  :: !GroupByClause
    , sdHavingClause   :: !HavingClause
    , sdOrderByClause  :: ![OrderByClause]
    , sdLimitClause    :: !LimitClause
    , sdLockingClause  :: !LockingClause
    , sdCteClause      :: ![CommonTableExpressionClause]
    }

instance Semigroup SideData where
    SideData d f s w g h o l k c <> SideData d' f' s' w' g' h' o' l' k' c' =
        SideData (d <> d') (f <> f') (s <> s') (w <> w') (g <> g') (h <> h') (o <> o') (l <> l') (k <> k') (c <> c')

instance Monoid SideData where
    mempty = SideData mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty
    mappend = (<>)

-- | The @DISTINCT@ "clause".
data DistinctClause
    = DistinctAll
    -- ^ The default, everything.
    | DistinctStandard
    -- ^ Only @DISTINCT@, SQL standard.
    | DistinctOn [SqlExpr DistinctOn]
    -- ^ @DISTINCT ON@, PostgreSQL extension.

instance Semigroup DistinctClause where
    DistinctOn a     <> DistinctOn b = DistinctOn (a <> b)
    DistinctOn a     <> _            = DistinctOn a
    DistinctStandard <> _            = DistinctStandard
    DistinctAll      <> b            = b

instance Monoid DistinctClause where
    mempty = DistinctAll
    mappend = (<>)

-- | A part of a @FROM@ clause.
data FromClause
    = FromStart Ident EntityDef
    | FromJoin FromClause JoinKind FromClause (Maybe (SqlExpr (Value Bool)))
    | OnClause (SqlExpr (Value Bool))
    | FromRaw (NeedParens -> IdentInfo -> (TLB.Builder, [PersistValue]))

data CommonTableExpressionKind
    = RecursiveCommonTableExpression
    | NormalCommonTableExpression
    deriving Eq

data CommonTableExpressionClause =
    CommonTableExpressionClause CommonTableExpressionKind Ident (IdentInfo -> (TLB.Builder, [PersistValue]))

data SubQueryType
    = NormalSubQuery
    | LateralSubQuery
    deriving Show

collectIdents :: FromClause -> Set Ident
collectIdents fc = case fc of
    FromStart i _ -> Set.singleton i
    FromJoin lhs _ rhs _ -> collectIdents lhs <> collectIdents rhs
    OnClause _ -> mempty
    FromRaw _ -> mempty

instance Show FromClause where
    show fc = case fc of
        FromStart i _ ->
            "(FromStart " <> show i <> ")"
        FromJoin lhs jk rhs mexpr ->
            mconcat
            [ "(FromJoin "
            , show lhs
            , " "
            , show jk
            , " "
            , case mexpr of
                Nothing -> "(no on clause)"
                Just expr -> "(" <> render' expr <> ")"
            , " "
            , show rhs
            , ")"
            ]
        OnClause expr ->
            "(OnClause " <> render' expr <> ")"
        FromRaw _ ->
            "(FromRaw _)"

      where
        -- We just want to use this to render expressions for a `Show` instance
        -- so we leave most of the fields undefined. But we explicitly
        -- initialize them to `undefined` so that GHC doesn't complain.
        dummy = mkSqlBackend MkSqlBackendArgs
            { connEscapeRawName = id
            , connPrepare = undefined
            , connInsertSql = undefined
            , connStmtMap = undefined
            , connClose = undefined
            , connMigrateSql = undefined
            , connBegin = undefined
            , connCommit = undefined
            , connRollback = undefined
            , connEscapeFieldName  = undefined
            , connEscapeTableName = undefined
            , connNoLimit = undefined
            , connRDBMS = undefined
            , connLimitOffset = undefined
            , connLogFunc = undefined
            }
        render' = T.unpack . renderExpr dummy

-- | A part of a @SET@ clause.
newtype SetClause = SetClause (SqlExpr Update)

-- | Collect 'OnClause's on 'FromJoin's.  Returns the first
-- unmatched 'OnClause's data on error.  Returns a list without
-- 'OnClauses' on success.
collectOnClauses
    :: SqlBackend
    -> [FromClause]
    -> Either (SqlExpr (Value Bool)) [FromClause]
collectOnClauses sqlBackend = go Set.empty []
  where
    go is []  (f@(FromStart i _) : fs) =
        fmap (f:) (go (Set.insert i is) [] fs) -- fast path
    go idents acc (OnClause expr : fs) = do
        (idents', a) <- findMatching idents acc expr
        go idents' a fs
    go idents acc (f:fs) =
        go idents (f:acc) fs
    go _ acc [] =
        return $ reverse acc

    findMatching
        :: Set Ident
        -> [FromClause]
        -> SqlExpr (Value Bool)
        -> Either (SqlExpr (Value Bool)) (Set Ident, [FromClause])
    findMatching idents fromClauses expr =
        case fromClauses of
            f : acc ->
              let idents' =
                      idents
                      <> Set.fromList
                          (Maybe.catMaybes [findLeftmostIdent f, findRightmostIdent f])
              in
                  case tryMatch idents' expr f of
                      Just (idents'', f') ->
                          return (idents'', f' : acc)
                      Nothing ->
                          fmap (f:) <$> findMatching idents' acc expr
            [] ->
                Left expr

    findRightmostIdent (FromStart i _) = Just i
    findRightmostIdent (FromJoin _ _ r _) = findRightmostIdent r
    findRightmostIdent (OnClause {}) = Nothing
    findRightmostIdent (FromRaw _) = Nothing

    findLeftmostIdent (FromStart i _) = Just i
    findLeftmostIdent (FromJoin l _ _ _) = findLeftmostIdent l
    findLeftmostIdent (OnClause {}) = Nothing
    findLeftmostIdent (FromRaw _) = Nothing

    tryMatch
        :: Set Ident
        -> SqlExpr (Value Bool)
        -> FromClause
        -> Maybe (Set Ident, FromClause)
    tryMatch idents expr fromClause =
      case fromClause of
        FromJoin l k r onClause ->
          matchTable <|> matchR <|> matchC <|> matchL <|> matchPartial -- right to left
            where
              matchR = fmap (\r' -> FromJoin l k r' onClause)
                <$> tryMatch idents expr r
              matchL = fmap (\l' -> FromJoin l' k r onClause)
                <$> tryMatch idents expr l

              matchPartial = do
                --Debug.traceM $ "matchPartial"
                --Debug.traceM $ "matchPartial: identsInOnClause: " <> show identsInOnClause
                i1 <- findLeftmostIdent l
                i2 <- findLeftmostIdent r
                let leftIdents = collectIdents l
                -- Debug.traceM $ "matchPartial: i1: " <> show i1
                -- Debug.traceM $ "matchPartial: i2: " <> show i2
                -- Debug.traceM $ "matchPartial: idents: " <> show idents
                guard $
                  Set.isSubsetOf
                    identsInOnClause
                    (Set.fromList [i1, i2] <> leftIdents)
                guard $ k /= CrossJoinKind
                guard $ Maybe.isNothing onClause
                pure (idents, FromJoin l k r (Just expr))

              matchC =
                case onClause of
                  Nothing
                    | "?" `T.isInfixOf` renderedExpr ->
                        return (idents, FromJoin l k r (Just expr))
                    | Set.null identsInOnClause ->
                        return (idents, FromJoin l k r (Just expr))
                    | otherwise ->
                        Nothing
                  Just _ ->
                    Nothing
              matchTable = do
                i1 <- findLeftmostIdent r
                i2 <- findRightmostIdent l
                guard $ Set.fromList [i1, i2] `Set.isSubsetOf` identsInOnClause
                guard $ k /= CrossJoinKind
                guard $ Maybe.isNothing onClause
                pure (Set.fromList [i1, i2] <> idents, FromJoin l k r (Just expr))

        _ ->
          Nothing
      where
        identsInOnClause =
          onExprToTableIdentifiers

        renderedExpr =
          renderExpr sqlBackend expr

        onExprToTableIdentifiers =
          Set.map (I . tableAccessTable)
          . either error id
          . parseOnExpr sqlBackend
          $ renderedExpr

-- | A complete @WHERE@ clause.
data WhereClause = Where (SqlExpr (Value Bool))
                 | NoWhere

instance Semigroup WhereClause where
  NoWhere  <> w        = w
  w        <> NoWhere  = w
  Where e1 <> Where e2 = Where (e1 &&. e2)

instance Monoid WhereClause where
  mempty = NoWhere
  mappend = (<>)

-- | A @GROUP BY@ clause.
newtype GroupByClause = GroupBy [SomeValue]

instance Semigroup GroupByClause where
  GroupBy fs <> GroupBy fs' = GroupBy (fs <> fs')

instance Monoid GroupByClause where
  mempty = GroupBy []
  mappend = (<>)

-- | A @HAVING@ cause.
type HavingClause = WhereClause

-- | A @ORDER BY@ clause.
type OrderByClause = SqlExpr OrderBy

-- | A @LIMIT@ clause.
data LimitClause = Limit (Maybe Int64) (Maybe Int64)
  deriving Eq

instance Semigroup LimitClause where
  Limit l1 o1 <> Limit l2 o2 =
    Limit (l2 `mplus` l1) (o2 `mplus` o1)
    -- More than one 'limit' or 'offset' is issued, we want to
    -- keep the latest one.  That's why we use mplus with
    -- "reversed" arguments.

instance Monoid LimitClause where
  mempty = Limit mzero mzero

-- | A locking clause.
data LockingClause =
    LegacyLockingClause LockingKind
    -- ^ Locking clause not specific to any database implementation
    | PostgresLockingClauses [PostgresLockingKind]
    -- ^ Locking clause specific to postgres
    | NoLockingClause

instance Semigroup LockingClause where
  -- Postgres allows us to have multiple locking clauses
    (<>) (PostgresLockingClauses pleft) (PostgresLockingClauses pright) = PostgresLockingClauses (pleft <> pright)
    (<>) mleft NoLockingClause = mleft
    (<>) _ mright = mright
--
instance Monoid LockingClause where
    mempty = NoLockingClause
    mappend = (<>)

----------------------------------------------------------------------

-- | Identifier used for table names.
newtype Ident = I T.Text
  deriving (Eq, Ord, Show)

-- | List of identifiers already in use and supply of temporary
-- identifiers.
newtype IdentState = IdentState { inUse :: HS.HashSet T.Text }

initialIdentState :: IdentState
initialIdentState = IdentState mempty

-- | Create a fresh 'Ident'.  If possible, use the given
-- 'DBName'.
newIdentFor :: DBName -> SqlQuery Ident
newIdentFor (DBName original) = Q $ lift $ findFree Nothing
  where
    findFree msuffix = do
      let
        withSuffix =
          maybe id (\suffix -> (<> T.pack (show suffix))) msuffix original
      isInUse <- S.gets (HS.member withSuffix . inUse)
      if isInUse
        then findFree (succ <$> (msuffix <|> Just (1 :: Int)))
        else do
          S.modify (\s -> s { inUse = HS.insert withSuffix (inUse s) })
          pure (I withSuffix)

-- | Information needed to escape and use identifiers.
type IdentInfo = (SqlBackend, IdentState)

-- | Use an identifier.
useIdent :: IdentInfo -> Ident -> TLB.Builder
useIdent info (I ident) = fromDBName info $ DBName ident

data SqlExprMeta = SqlExprMeta
    { -- A composite key.
      --
      -- Persistent uses the same 'PersistList' constructor for both
      -- fields which are (homogeneous) lists of values and the
      -- (probably heterogeneous) values of a composite primary key.
      --
      -- We need to treat composite keys as fields.  For example, we
      -- have to support using ==., otherwise you wouldn't be able to
      -- join.  OTOH, lists of values should be treated exactly the
      -- same as any other scalar value.
      --
      -- In particular, this is valid for persistent via rawSql for
      -- an F field that is a list:
      --
      --   A.F in ?    -- [PersistList [foo, bar]]
      --
      -- However, this is not for a composite key entity:
      --
      --   A.ID = ?    -- [PersistList [foo, bar]]
      --
      -- The ID field doesn't exist on the DB for a composite key
      -- table, it exists only on the Haskell side.  Those variations
      -- also don't work:
      --
      --   (A.KeyA, A.KeyB) = ?    -- [PersistList [foo, bar]]
      --   [A.KeyA, A.KeyB] = ?    -- [PersistList [foo, bar]]
      --
      -- We have to generate:
      --
      --   A.KeyA = ? AND A.KeyB = ?      -- [foo, bar]
      --
      -- Note that the PersistList had to be deconstructed into its
      -- components.
      --
      -- In order to disambiguate behaviors, this constructor is used
      -- /only/ to represent a composite field access.  It does not
      -- represent a 'PersistList', not even if the 'PersistList' is
      -- used in the context of a composite key.  That's because it's
      -- impossible, e.g., for 'val' to disambiguate between these
      -- uses.
      sqlExprMetaCompositeFields :: Maybe (IdentInfo -> [TLB.Builder])
    , sqlExprMetaAlias :: Maybe Ident -- Alias ident if this is an aliased value/entity
    , sqlExprMetaIsReference :: Bool -- Is this SqlExpr a reference to the selected value/entity (supports subqueries)
    }

-- | Empty 'SqlExprMeta' if you are constructing an 'ERaw' probably use this
-- for your meta
noMeta :: SqlExprMeta
noMeta = SqlExprMeta
    { sqlExprMetaCompositeFields = Nothing
    , sqlExprMetaAlias = Nothing
    , sqlExprMetaIsReference = False
    }

-- | Does this meta contain values for composite fields.
-- This field is field out for composite key values
hasCompositeKeyMeta :: SqlExprMeta -> Bool
hasCompositeKeyMeta = Maybe.isJust . sqlExprMetaCompositeFields

entityAsValue
    :: SqlExpr (Entity val)
    -> SqlExpr (Value (Entity val))
entityAsValue = coerce

entityAsValueMaybe
    :: SqlExpr (Maybe (Entity val))
    -> SqlExpr (Value (Maybe (Entity val)))
entityAsValueMaybe = coerce

-- | An expression on the SQL backend.
--
-- Raw expression: Contains a 'SqlExprMeta' and a function for
-- building the expr. It recieves a parameter telling it whether
-- it is in a parenthesized context, and takes information about the SQL
-- connection (mainly for escaping names) and returns both an
-- string ('TLB.Builder') and a list of values to be
-- interpolated by the SQL backend.
data SqlExpr a = ERaw SqlExprMeta (NeedParens -> IdentInfo -> (TLB.Builder, [PersistValue]))

-- | Folks often want the ability to promote a Haskell function into the
-- 'SqlExpr' expression language - and naturally reach for 'fmap'.
-- Unfortunately, this is impossible. We cannot send *functions* to the
-- database, which is what we would need to do in order for this to make sense.
-- Let's consider the type of 'fmap' for 'SqlExpr':
--
-- @
-- fmap :: (a -> b) -> 'SqlExpr' a -> 'SqlExpr' b
-- @
--
-- This type signature is making a pretty strong claim: "Give me a Haskell
-- function from @a -> b@. I will then transform a SQL expression representing
-- a Haskell value of type @a@ and turn it into a SQL expression representing
-- a Haskell value of type @b@."
--
-- Let's suppose we *could* do this - @fmap (+1)@ would have to somehow inspect
-- the function expression means "add one", and then translate that to the
-- appropriate SQL.
--
-- This is why @esqueleto@ defines a bunch of operators: @x '+.' ('val' 1)@ can
-- be used instead of @'fmap' (+1) x@.
--
-- If you do have a SQL function, then you can provide a safe type and introduce
-- it with 'unsafeSqlFunction' or 'unsafeSqlBinOp'.
--
-- @since 3.5.8.2
instance TypeError SqlExprFunctorMessage => Functor SqlExpr where
    fmap = error "impossible"

-- | The type error message given when you try to do 'fmap' on a 'SqlExpr'. This
-- is intended to guide folks towards the docs, which should guide them towards
-- alternative implementations.
--
-- @since 3.5.8.2
type SqlExprFunctorMessage =
    'Text "You're trying to treat `SqlExpr` like a `Functor`, but it cannot be one."
    ':$$: 'Text "We would need to send arbitrary functions to the database for interpretation to support that instance."
    ':$$: 'Text "See the docs for the fake instance of `Functor SqlExpr` for more information."
    ':$$: 'Text "Consider using a SQL function with `unsafeSqlFunction` and a good type signature."

-- |  This instance allows you to use @record.field@ notation with GHC 9.2's
-- @OverloadedRecordDot@ extension.
--
-- Example:
--
-- @
-- -- persistent model:
-- BlogPost
--     authorId     PersonId
--     title        Text
--
-- -- query:
-- 'select' $ do
--     bp <- 'from' $ 'table' \@BlogPost
--     pure $ bp.title
-- @
--
-- This is exactly equivalent to the following:
--
-- @
-- blogPost :: SqlExpr (Entity BlogPost)
--
-- blogPost ^. BlogPostTitle
-- blogPost ^. #title
-- blogPost.title
-- @
-- There's another instance defined on @'SqlExpr' ('Entity' ('Maybe' rec))@,
-- which allows you to project from a @LEFT JOIN@ed entity.
--
-- @since 3.5.4.0
instance
    (PersistEntity rec, PersistField typ, SymbolToField sym rec typ)
  =>
    HasField sym (SqlExpr (Entity rec)) (SqlExpr (Value typ))
  where
    getField expr = expr ^. symbolToField @sym

-- | This instance allows you to use @record.field@ notation with GC 9.2's
-- @OverloadedRecordDot@ extension.
--
-- Example:
--
-- @
-- -- persistent model:
-- Person
--     name         Text
--
-- BlogPost
--     title        Text
--     authorId     PersonId
--
-- -- query:
--
-- 'select' $ do
--     (p :& bp) <- 'from' $
--         'table' @Person
--         `leftJoin` table @BlogPost
--         `on` do
--             \\(p :& bp) ->
--                 just p.id ==. bp.authorId
--     pure (p.name, bp.title)
-- @
--
-- The following forms are all equivalent:
--
-- @
-- blogPost :: SqlExpr (Maybe (Entity BlogPost))
--
-- blogPost ?. BlogPostTitle
-- blogPost ?. #title
-- blogPost.title
-- @
--
-- @since 3.5.4.0
instance
    (PersistEntity rec, PersistField typ, SymbolToField sym rec typ)
  =>
    HasField sym (SqlExpr (Maybe (Entity rec))) (SqlExpr (Value (Maybe typ)))
  where
    getField expr = expr ?. symbolToField @sym

-- | Data type to support from hack
data PreprocessedFrom a = PreprocessedFrom a FromClause

-- | Phantom type used to mark a @INSERT INTO@ query.
data InsertFinal

data NeedParens = Parens | Never
    deriving Eq

parensM :: NeedParens -> TLB.Builder -> TLB.Builder
parensM Never  = id
parensM Parens = parens

data OrderByType = ASC | DESC

instance ToSomeValues (SqlExpr (Value a)) where
  toSomeValues a = [SomeValue a]

fieldName
    :: (PersistEntity val, PersistField typ)
    => IdentInfo -> EntityField val typ -> TLB.Builder
fieldName info = fromDBName info . coerce . fieldDB . persistFieldDef

-- FIXME: Composite/non-id pKS not supported on set
setAux
    :: (PersistEntity val, PersistField typ)
    => EntityField val typ
    -> (SqlExpr (Entity val) -> SqlExpr (Value typ))
    -> (SqlExpr (Entity val) -> SqlExpr Update)
setAux field value = \ent -> ERaw noMeta $ \_ info ->
    let ERaw _ valueF = value ent
        (valueToSet, valueVals) = valueF Parens info
    in (fieldName info field <> " = " <> valueToSet, valueVals)

sub :: PersistField a => Mode -> SqlQuery (SqlExpr (Value a)) -> SqlExpr (Value a)
sub mode query = ERaw noMeta $ \_ info -> first parens $ toRawSql mode info query

fromDBName :: IdentInfo -> DBName -> TLB.Builder
fromDBName (conn, _) = TLB.fromText . flip getEscapedRawName conn . unDBName

existsHelper :: SqlQuery () -> SqlExpr (Value Bool)
existsHelper = sub SELECT . (>> return true)
  where
    true  :: SqlExpr (Value Bool)
    true = val True

-- | (Internal) Create a case statement.
--
-- Since: 2.1.1
unsafeSqlCase :: PersistField a => [(SqlExpr (Value Bool), SqlExpr (Value a))] -> SqlExpr (Value a) -> SqlExpr (Value a)
unsafeSqlCase when v = ERaw noMeta buildCase
  where
    buildCase :: NeedParens -> IdentInfo -> (TLB.Builder, [PersistValue])
    buildCase _ info =
        let (elseText, elseVals) = valueToSql v Parens info
            (whenText, whenVals) = mapWhen when Parens info
        in ( "CASE" <> whenText <> " ELSE " <> elseText <> " END", whenVals <> elseVals)

    mapWhen :: [(SqlExpr (Value Bool), SqlExpr (Value a))] -> NeedParens -> IdentInfo -> (TLB.Builder, [PersistValue])
    mapWhen []    _ _    = throw (UnexpectedCaseErr UnsafeSqlCaseError)
    mapWhen when' p info = foldl (foldHelp p info) (mempty, mempty) when'


    foldHelp :: NeedParens -> IdentInfo -> (TLB.Builder, [PersistValue]) -> (SqlExpr (Value Bool), SqlExpr (Value a)) -> (TLB.Builder, [PersistValue])
    foldHelp p info (b0, vals0) (v1, v2) =
        let (b1, vals1) = valueToSql v1 p info
            (b2, vals2) = valueToSql v2 p info
        in ( b0 <> " WHEN " <> b1 <> " THEN " <> b2, vals0 <> vals1 <> vals2 )

    valueToSql :: SqlExpr (Value a) -> NeedParens -> IdentInfo -> (TLB.Builder, [PersistValue])
    valueToSql (ERaw _ f) p = f p

-- | (Internal) Create a custom binary operator.  You /should/
-- /not/ use this function directly since its type is very
-- general, you should always use it with an explicit type
-- signature.  For example:
--
-- @
-- (==.) :: SqlExpr (Value a) -> SqlExpr (Value a) -> SqlExpr (Value Bool)
-- (==.) = unsafeSqlBinOp " = "
-- @
--
-- In the example above, we constraint the arguments to be of the
-- same type and constraint the result to be a boolean value.
unsafeSqlBinOp :: TLB.Builder -> SqlExpr (Value a) -> SqlExpr (Value b) -> SqlExpr (Value c)
unsafeSqlBinOp op (ERaw m1 f1) (ERaw m2 f2)
  | not (hasCompositeKeyMeta m1 || hasCompositeKeyMeta m2) = ERaw noMeta f
  where
    f p info =
        let (b1, vals1) = f1 Parens info
            (b2, vals2) = f2 Parens info
        in
            ( parensM p (b1 <> op <> b2)
            , vals1 <> vals2
            )
unsafeSqlBinOp op a b = unsafeSqlBinOp op (construct a) (construct b)
  where
    construct :: SqlExpr (Value a) -> SqlExpr (Value a)
    construct (ERaw m f) =
        case sqlExprMetaCompositeFields m of
            Just fields ->
                ERaw noMeta $ \_ info -> (parens $ uncommas $ fields info, mempty)
            Nothing ->
                ERaw noMeta $ \p info ->
                    let (b1, vals) = f (if p == Never then Parens else Never) info
                        build ("?", [PersistList vals']) =
                            (uncommas $ replicate (length vals') "?", vals')
                        build expr = expr
                     in
                        first (parensM p) $ build (b1, vals)
{-# INLINE unsafeSqlBinOp #-}

-- | Similar to 'unsafeSqlBinOp', but may also be applied to
-- composite keys.  Uses the operator given as the second
-- argument whenever applied to composite keys.
--
-- Usage example:
--
-- @
-- (==.) :: SqlExpr (Value a) -> SqlExpr (Value a) -> SqlExpr (Value Bool)
-- (==.) = unsafeSqlBinOpComposite " = " " AND "
-- @
--
-- Persistent has a hack for implementing composite keys (see
-- 'ECompositeKey' doc for more details), so we're forced to use
-- a hack here as well.  We deconstruct 'ERaw' values based on
-- two rules:
--
--   - If it is a single placeholder, then it's assumed to be
--   coming from a 'PersistList' and thus its components are
--   separated so that they may be applied to a composite key.
--
--   - If it is not a single placeholder, then it's assumed to be
--   a foreign (composite or not) key, so we enforce that it has
--   no placeholders and split it on the commas.
unsafeSqlBinOpComposite :: TLB.Builder -> TLB.Builder -> SqlExpr (Value a) -> SqlExpr (Value b) -> SqlExpr (Value c)
unsafeSqlBinOpComposite op sep a b
    | isCompositeKey a || isCompositeKey b = ERaw noMeta $ const $ compose (listify a) (listify b)
    | otherwise = unsafeSqlBinOp op a b
  where
    isCompositeKey :: SqlExpr (Value x) -> Bool
    isCompositeKey (ERaw m _) = hasCompositeKeyMeta m

    listify :: SqlExpr (Value x) -> IdentInfo -> ([TLB.Builder], [PersistValue])
    listify (ERaw m f)
        | Just k <- sqlExprMetaCompositeFields m = flip (,) [] . k
        | otherwise = deconstruct . f Parens

    deconstruct :: (TLB.Builder, [PersistValue]) -> ([TLB.Builder], [PersistValue])
    deconstruct ("?", [PersistList vals]) = (replicate (length vals) "?", vals)
    deconstruct (b', []) = (TLB.fromLazyText <$> TL.splitOn "," (TLB.toLazyText b'), [])
    deconstruct _ = throw (SqlBinOpCompositeErr DeconstructionError)

    compose f1 f2 info
        | not (null v1 || null v2) = throw (SqlBinOpCompositeErr NullPlaceholdersError)
        | length b1 /= length b2   = throw (SqlBinOpCompositeErr MismatchingLengthsError)
        | otherwise                = (bc, vc)
      where
        (b1, v1) = f1 info
        (b2, v2) = f2 info
        bc = intersperseB sep [x <> op <> y | (x, y) <- zip b1 b2]
        vc = v1 <> v2


-- | (Internal) A raw SQL value.  The same warning from
-- 'unsafeSqlBinOp' applies to this function as well.
unsafeSqlValue :: TLB.Builder -> SqlExpr (Value a)
unsafeSqlValue v = ERaw noMeta $ \_ _ -> (v, mempty)
{-# INLINE unsafeSqlValue #-}

unsafeSqlEntity :: PersistEntity ent => Ident -> SqlExpr (Entity ent)
unsafeSqlEntity ident = ERaw noMeta $ \_ info ->
    (useIdent info ident, [])

valueToFunctionArg :: IdentInfo -> SqlExpr (Value a) -> (TLB.Builder, [PersistValue])
valueToFunctionArg info (ERaw _ f) = f Never info

-- | (Internal) A raw SQL function.  Once again, the same warning
-- from 'unsafeSqlBinOp' applies to this function as well.
unsafeSqlFunction
    :: UnsafeSqlFunctionArgument a
    => TLB.Builder -> a -> SqlExpr (Value b)
unsafeSqlFunction name arg =
    ERaw noMeta $ \_ info ->
        let (argsTLB, argsVals) =
              uncommas' $ map (valueToFunctionArg info) $ toArgList arg
        in
            (name <> parens argsTLB, argsVals)

-- | (Internal) An unsafe SQL function to extract a subfield from a compound
-- field, e.g. datetime. See 'unsafeSqlBinOp' for warnings.
--
-- Since: 1.3.6.
unsafeSqlExtractSubField
    :: UnsafeSqlFunctionArgument a
    => TLB.Builder -> a -> SqlExpr (Value b)
unsafeSqlExtractSubField subField arg =
    ERaw noMeta $ \_ info ->
        let (argsTLB, argsVals) =
                uncommas' $ map (valueToFunctionArg info) $ toArgList arg
        in
            ("EXTRACT" <> parens (subField <> " FROM " <> argsTLB), argsVals)

-- | (Internal) A raw SQL function. Preserves parentheses around arguments.
-- See 'unsafeSqlBinOp' for warnings.
unsafeSqlFunctionParens
    :: UnsafeSqlFunctionArgument a
    => TLB.Builder -> a -> SqlExpr (Value b)
unsafeSqlFunctionParens name arg =
    ERaw noMeta $ \_ info ->
        let valueToFunctionArgParens (ERaw _ f) = f Never info
            (argsTLB, argsVals) =
                uncommas' $ map valueToFunctionArgParens $ toArgList arg
        in (name <> parens argsTLB, argsVals)

-- | (Internal) An explicit SQL type cast using CAST(value as type).
-- See 'unsafeSqlBinOp' for warnings.
unsafeSqlCastAs :: T.Text -> SqlExpr (Value a) -> SqlExpr (Value b)
unsafeSqlCastAs t (ERaw _ f) = ERaw noMeta $ \_ -> ((first (\value -> "CAST" <> parens (value <> " AS " <> TLB.fromText t))) . f Never)

-- | (Internal) This class allows 'unsafeSqlFunction' to work with different
-- numbers of arguments; specifically it allows providing arguments to a sql
-- function via an n-tuple of @SqlExpr (Value _)@ values, which are not all
-- necessarily required to be the same type. There are instances for up to
-- 10-tuples, but for sql functions which take more than 10 arguments, you can
-- also nest tuples, as e.g. @toArgList ((a,b),(c,d))@ is the same as
-- @toArgList (a,b,c,d)@.
class UnsafeSqlFunctionArgument a where
    toArgList :: a -> [SqlExpr (Value ())]

-- | Useful for 0-argument functions, like @now@ in Postgresql.
--
-- @since 3.2.1
instance UnsafeSqlFunctionArgument () where
    toArgList _ = []

instance (a ~ Value b) => UnsafeSqlFunctionArgument (SqlExpr a) where
    toArgList = (:[]) . veryUnsafeCoerceSqlExprValue

instance UnsafeSqlFunctionArgument a => UnsafeSqlFunctionArgument [a] where
  toArgList = concatMap toArgList

instance
    (UnsafeSqlFunctionArgument a , UnsafeSqlFunctionArgument b)
  =>
    UnsafeSqlFunctionArgument (a, b)
  where
    toArgList (a, b) = toArgList a ++ toArgList b

instance
    ( UnsafeSqlFunctionArgument a
    , UnsafeSqlFunctionArgument b
    , UnsafeSqlFunctionArgument c
    )
  =>
    UnsafeSqlFunctionArgument (a, b, c)
  where
    toArgList = toArgList . from3

instance
    ( UnsafeSqlFunctionArgument a
    , UnsafeSqlFunctionArgument b
    , UnsafeSqlFunctionArgument c
    , UnsafeSqlFunctionArgument d
    )
  =>
    UnsafeSqlFunctionArgument (a, b, c, d)
  where
    toArgList = toArgList . from4

-- | @since 3.2.3
instance
    ( UnsafeSqlFunctionArgument a
    , UnsafeSqlFunctionArgument b
    , UnsafeSqlFunctionArgument c
    , UnsafeSqlFunctionArgument d
    , UnsafeSqlFunctionArgument e
    )
  =>
    UnsafeSqlFunctionArgument (a, b, c, d, e)
  where
    toArgList = toArgList . from5

-- | @since 3.2.3
instance
    ( UnsafeSqlFunctionArgument a
    , UnsafeSqlFunctionArgument b
    , UnsafeSqlFunctionArgument c
    , UnsafeSqlFunctionArgument d
    , UnsafeSqlFunctionArgument e
    , UnsafeSqlFunctionArgument f
    )
  =>
    UnsafeSqlFunctionArgument (a, b, c, d, e, f)
  where
    toArgList = toArgList . from6

-- | @since 3.2.3
instance ( UnsafeSqlFunctionArgument a
         , UnsafeSqlFunctionArgument b
         , UnsafeSqlFunctionArgument c
         , UnsafeSqlFunctionArgument d
         , UnsafeSqlFunctionArgument e
         , UnsafeSqlFunctionArgument f
         , UnsafeSqlFunctionArgument g
         ) => UnsafeSqlFunctionArgument (a, b, c, d, e, f, g) where
  toArgList = toArgList . from7
-- | @since 3.2.3
instance ( UnsafeSqlFunctionArgument a
         , UnsafeSqlFunctionArgument b
         , UnsafeSqlFunctionArgument c
         , UnsafeSqlFunctionArgument d
         , UnsafeSqlFunctionArgument e
         , UnsafeSqlFunctionArgument f
         , UnsafeSqlFunctionArgument g
         , UnsafeSqlFunctionArgument h
         ) => UnsafeSqlFunctionArgument (a, b, c, d, e, f, g, h) where
  toArgList = toArgList . from8
-- | @since 3.2.3
instance ( UnsafeSqlFunctionArgument a
         , UnsafeSqlFunctionArgument b
         , UnsafeSqlFunctionArgument c
         , UnsafeSqlFunctionArgument d
         , UnsafeSqlFunctionArgument e
         , UnsafeSqlFunctionArgument f
         , UnsafeSqlFunctionArgument g
         , UnsafeSqlFunctionArgument h
         , UnsafeSqlFunctionArgument i
         ) => UnsafeSqlFunctionArgument (a, b, c, d, e, f, g, h, i) where
  toArgList = toArgList . from9
-- | @since 3.2.3
instance ( UnsafeSqlFunctionArgument a
         , UnsafeSqlFunctionArgument b
         , UnsafeSqlFunctionArgument c
         , UnsafeSqlFunctionArgument d
         , UnsafeSqlFunctionArgument e
         , UnsafeSqlFunctionArgument f
         , UnsafeSqlFunctionArgument g
         , UnsafeSqlFunctionArgument h
         , UnsafeSqlFunctionArgument i
         , UnsafeSqlFunctionArgument j
         ) => UnsafeSqlFunctionArgument (a, b, c, d, e, f, g, h, i, j) where
  toArgList = toArgList . from10


-- | (Internal) Coerce a value's type from 'SqlExpr (Value a)' to
-- 'SqlExpr (Value b)'.  You should /not/ use this function
-- unless you know what you're doing!
veryUnsafeCoerceSqlExprValue :: SqlExpr (Value a) -> SqlExpr (Value b)
veryUnsafeCoerceSqlExprValue = coerce


-- | (Internal) Coerce a value's type from 'SqlExpr (ValueList
-- a)' to 'SqlExpr (Value a)'.  Does not work with empty lists.
veryUnsafeCoerceSqlExprValueList :: SqlExpr (ValueList a) -> SqlExpr (Value a)
veryUnsafeCoerceSqlExprValueList = coerce


----------------------------------------------------------------------

-- | (Internal) Execute an @esqueleto@ @SELECT@ 'SqlQuery' inside
-- @persistent@'s 'SqlPersistT' monad.
rawSelectSource
    ::
    ( SqlSelect a r
    , MonadIO m1
    , MonadIO m2
    , SqlBackendCanRead backend
    )
    => Mode
    -> SqlQuery a
    -> R.ReaderT backend m1 (Acquire (C.ConduitT () r m2 ()))
rawSelectSource mode query = do
    conn <- projectBackend <$> R.ask
    let _ = conn :: SqlBackend
    res <- R.withReaderT (const conn) (run conn)
    return $ (C..| massage) `fmap` res
  where
    run conn =
        uncurry rawQueryRes $
        first builderToText $
        toRawSql mode (conn, initialIdentState) query

    massage = do
        mrow <- C.await
        case sqlSelectProcessRow <$> mrow of
            Just (Right r)  -> C.yield r >> massage
            Just (Left err) -> liftIO $ throwIO $ PersistMarshalError err
            Nothing         -> return ()

-- | Execute an @esqueleto@ @SELECT@ query inside @persistent@'s
-- 'SqlPersistT' monad and return a 'C.Source' of rows.
selectSource
    ::
    ( SqlSelect a r
    , BackendCompatible SqlBackend backend
    , IsPersistBackend backend
    , PersistQueryRead backend
    , PersistStoreRead backend, PersistUniqueRead backend
    , MonadResource m
    )
    => SqlQuery a
    -> C.ConduitT () r (R.ReaderT backend m) ()
selectSource query = do
    res <- lift $ rawSelectSource SELECT query
    (key, src) <- lift $ allocateAcquire res
    src
    lift $ release key

-- | Execute an @esqueleto@ @SELECT@ query inside @persistent@'s
-- 'SqlPersistT' monad and return a list of rows.
--
-- We've seen that 'from' has some magic about which kinds of
-- things you may bring into scope.  This 'select' function also
-- has some magic for which kinds of things you may bring back to
-- Haskell-land by using @SqlQuery@'s @return@:
--
--  * You may return a @SqlExpr ('Entity' v)@ for an entity @v@
--  (i.e., like the @*@ in SQL), which is then returned to
--  Haskell-land as just @Entity v@.
--
--  * You may return a @SqlExpr (Maybe (Entity v))@ for an entity
--  @v@ that may be @NULL@, which is then returned to
--  Haskell-land as @Maybe (Entity v)@.  Used for @OUTER JOIN@s.
--
--  * You may return a @SqlExpr ('Value' t)@ for a value @t@
--  (i.e., a single column), where @t@ is any instance of
--  'PersistField', which is then returned to Haskell-land as
--  @Value t@.  You may use @Value@ to return projections of an
--  @Entity@ (see @('^.')@ and @('?.')@) or to return any other
--  value calculated on the query (e.g., 'countRows' or
--  'subSelect').
--
-- The @SqlSelect a r@ class has functional dependencies that
-- allow type information to flow both from @a@ to @r@ and
-- vice-versa.  This means that you'll almost never have to give
-- any type signatures for @esqueleto@ queries.  For example, the
-- query @'select' $ from $ \\p -> return p@ alone is ambiguous, but
-- in the context of
--
-- @
-- do ps <- 'select' $
--          'from' $ \\p ->
--          return p
--    liftIO $ mapM_ (putStrLn . personName . entityVal) ps
-- @
--
-- we are able to infer from that single @personName . entityVal@
-- function composition that the @p@ inside the query is of type
-- @SqlExpr (Entity Person)@.
select
    ::
    ( SqlSelect a r
    , MonadIO m
    , SqlBackendCanRead backend
    )
    => SqlQuery a
    -> R.ReaderT backend m [r]
select query = do
    res <- rawSelectSource SELECT query
    conn <- R.ask
    liftIO $ with res $ flip R.runReaderT conn . runSource

-- | Execute an @esqueleto@ @SELECT@ query inside @persistent@'s
-- 'SqlPersistT' monad and return the first entry wrapped in a @Maybe@.
-- @since 3.5.1.0
--
-- === __Example usage__
--
-- @
-- firstPerson :: MonadIO m => SqlPersistT m (Maybe (Entity Person))
-- firstPerson =
--  'selectOne' $ do
--      person <- 'from' $ 'table' @Person
--      return person
-- @
--
-- The above query is equivalent to a 'select' combined with 'limit' but you
-- would still have to transform the results from a list:
--
-- @
-- firstPerson :: MonadIO m => SqlPersistT m [Entity Person]
-- firstPerson =
--  'select' $ do
--      person <- 'from' $ 'table' @Person
--      'limit' 1
--      return person
-- @

selectOne :: (SqlSelect a r, MonadIO m, SqlBackendCanRead backend) => SqlQuery a -> R.ReaderT backend m (Maybe r)
selectOne query = fmap Maybe.listToMaybe $ select $ limit 1 >> query

-- | (Internal) Run a 'C.Source' of rows.
runSource
    :: Monad m
    => C.ConduitT () r (R.ReaderT backend m) ()
    -> R.ReaderT backend m [r]
runSource src = C.runConduit $ src C..| CL.consume

-- | (Internal) Execute an @esqueleto@ statement inside
-- @persistent@'s 'SqlPersistT' monad.
rawEsqueleto
    :: (MonadIO m, SqlSelect a r, BackendCompatible SqlBackend backend)
    => Mode
    -> SqlQuery a
    -> R.ReaderT backend m Int64
rawEsqueleto mode query = do
    conn <- R.ask
    uncurry rawExecuteCount $
        first builderToText $
        toRawSql mode (conn, initialIdentState) query

-- | Execute an @esqueleto@ @DELETE@ query inside @persistent@'s
-- 'SqlPersistT' monad.  Note that currently there are no type
-- checks for statements that should not appear on a @DELETE@
-- query.
--
-- Example of usage:
--
-- @
-- 'delete' $
-- 'from' $ \\appointment ->
-- 'where_' (appointment '^.' AppointmentDate '<.' 'val' now)
-- @
--
-- Unlike 'select', there is a useful way of using 'delete' that
-- will lead to type ambiguities.  If you want to delete all rows
-- (i.e., no 'where_' clause), you'll have to use a type signature:
--
-- @
-- 'delete' $
-- 'from' $ \\(appointment :: 'SqlExpr' ('Entity' Appointment)) ->
-- return ()
-- @
--
-- ==== "Database.Esqueleto.Experimental":
--
-- @
--  delete $ do
--    userFeature <- from $ table @UserFeature
--    where_ ((userFeature ^. UserFeatureFeature) `notIn` valList allKnownFeatureFlags)
-- @
--
delete
    :: (MonadIO m, SqlBackendCanWrite backend)
    => SqlQuery ()
    -> R.ReaderT backend m ()
delete a = void $ deleteCount a

-- | Same as 'delete', but returns the number of rows affected.
deleteCount
    :: (MonadIO m, SqlBackendCanWrite backend)
    => SqlQuery ()
    -> R.ReaderT backend m Int64
deleteCount a = rawEsqueleto DELETE a

-- | Execute an @esqueleto@ @UPDATE@ query inside @persistent@'s
-- 'SqlPersistT' monad.  Note that currently there are no type
-- checks for statements that should not appear on a @UPDATE@
-- query.
--
-- Example of usage:
--
-- @
-- 'update' $ \\p -> do
-- 'set' p [ PersonAge '=.' 'just' ('val' thisYear) -. p '^.' PersonBorn ]
-- 'where_' $ isNothing (p '^.' PersonAge)
-- @
update
    ::
    ( MonadIO m, PersistEntity val
    , BackendCompatible SqlBackend (PersistEntityBackend val)
    , SqlBackendCanWrite backend
    )
    => (SqlExpr (Entity val) -> SqlQuery ())
    -> R.ReaderT backend m ()
update a = void $ updateCount a

-- | Same as 'update', but returns the number of rows affected.
updateCount
    ::
    ( MonadIO m, PersistEntity val
    , BackendCompatible SqlBackend (PersistEntityBackend val)
    , SqlBackendCanWrite backend
    )
    => (SqlExpr (Entity val) -> SqlQuery ())
    -> R.ReaderT backend m Int64
updateCount a = rawEsqueleto UPDATE $ from a

builderToText :: TLB.Builder -> T.Text
builderToText = TL.toStrict . TLB.toLazyTextWith defaultChunkSize
  where
    defaultChunkSize :: Int
    defaultChunkSize = 1024 - 32

-- | (Internal) Pretty prints a 'SqlQuery' into a SQL query.
--
-- Note: if you're curious about the SQL query being generated by
-- @esqueleto@, instead of manually using this function (which is
-- possible but tedious), see the 'renderQueryToText' function (along with
-- 'renderQuerySelect', 'renderQueryUpdate', etc).
toRawSql
  :: (SqlSelect a r, BackendCompatible SqlBackend backend)
  => Mode -> (backend, IdentState) -> SqlQuery a -> (TLB.Builder, [PersistValue])
toRawSql mode (conn, firstIdentState) query =
    let ((ret, sd), finalIdentState) =
            flip S.runState firstIdentState $
            W.runWriterT $
            unQ query
        deleteRepeatedNewlines txt =
            let
                (preNewlines, rest) = TL.break (== '\n') txt
                (_, rest') = TL.break (/= '\n') rest
             in
                if TL.null rest'
                    then preNewlines <> "\n"
                    else preNewlines <> "\n" <> deleteRepeatedNewlines rest'

        SideData distinctClause
                 fromClauses
                 setClauses
                 whereClauses
                 groupByClause
                 havingClause
                 orderByClauses
                 limitClause
                 lockingClause
                 cteClause = sd
        -- Pass the finalIdentState (containing all identifiers
        -- that were used) to the subsequent calls.  This ensures
        -- that no name clashes will occur on subqueries that may
        -- appear on the expressions below.
        info = (projectBackend conn, finalIdentState)
    in (\(x, t) -> (TLB.fromLazyText $ deleteRepeatedNewlines $ TL.strip $ TLB.toLazyText x, t)) $ mconcat $ intersperse ("\n", [])
        [ makeCte        info cteClause
        , makeInsertInto info mode ret
        , makeSelect     info mode distinctClause ret
        , makeFrom       info mode fromClauses
        , makeSet        info setClauses
        , makeWhere      info whereClauses
        , makeGroupBy    info groupByClause
        , makeHaving     info havingClause
        , makeOrderBy    info orderByClauses
        , makeLimit      info limitClause
        , makeLocking    info lockingClause
        ]


-- | Renders a 'SqlQuery' into a 'Text' value along with the list of
-- 'PersistValue's that would be supplied to the database for @?@ placeholders.
--
-- You must ensure that the 'Mode' you pass to this function corresponds with
-- the actual 'SqlQuery'. If you pass a query that uses incompatible features
-- (like an @INSERT@ statement with a @SELECT@ mode) then you'll get a weird
-- result.
--
-- @since 3.1.1
renderQueryToText
    :: (SqlSelect a r, BackendCompatible SqlBackend backend, Monad m)
    => Mode
    -- ^ Whether to render as an 'SELECT', 'DELETE', etc.
    -> SqlQuery a
    -- ^ The SQL query you want to render.
    -> R.ReaderT backend m (T.Text, [PersistValue])
renderQueryToText mode query = do
    backend <- R.ask
    let (builder, pvals) = toRawSql mode (backend, initialIdentState) query
    pure (builderToText builder, pvals)

-- | Renders a 'SqlQuery' into a 'Text' value along with the list of
-- 'PersistValue's that would be supplied to the database for @?@ placeholders.
--
-- @since 3.1.1
renderQuerySelect
    :: (SqlSelect a r, BackendCompatible SqlBackend backend, Monad m)
    => SqlQuery a
    -- ^ The SQL query you want to render.
    -> R.ReaderT backend m (T.Text, [PersistValue])
renderQuerySelect = renderQueryToText SELECT

-- | Renders a 'SqlQuery' into a 'Text' value along with the list of
-- 'PersistValue's that would be supplied to the database for @?@ placeholders.
--
-- @since 3.1.1
renderQueryDelete
    :: (SqlSelect a r, BackendCompatible SqlBackend backend, Monad m)
    => SqlQuery a
    -- ^ The SQL query you want to render.
    -> R.ReaderT backend m (T.Text, [PersistValue])
renderQueryDelete = renderQueryToText DELETE

-- | Renders a 'SqlQuery' into a 'Text' value along with the list of
-- 'PersistValue's that would be supplied to the database for @?@ placeholders.
--
-- @since 3.1.1
renderQueryUpdate
    :: (SqlSelect a r, BackendCompatible SqlBackend backend, Monad m)
    => SqlQuery a
    -- ^ The SQL query you want to render.
    -> R.ReaderT backend m (T.Text, [PersistValue])
renderQueryUpdate = renderQueryToText UPDATE

-- | Renders a 'SqlQuery' into a 'Text' value along with the list of
-- 'PersistValue's that would be supplied to the database for @?@ placeholders.
--
-- @since 3.1.1
renderQueryInsertInto
    :: (SqlSelect a r, BackendCompatible SqlBackend backend, Monad m)
    => SqlQuery a
    -- ^ The SQL query you want to render.
    -> R.ReaderT backend m (T.Text, [PersistValue])
renderQueryInsertInto = renderQueryToText INSERT_INTO

-- | (Internal) Mode of query being converted by 'toRawSql'.
data Mode
    = SELECT
    | DELETE
    | UPDATE
    | INSERT_INTO

uncommas :: [TLB.Builder] -> TLB.Builder
uncommas = intersperseB ", "

intersperseB :: TLB.Builder -> [TLB.Builder] -> TLB.Builder
intersperseB a = mconcat . intersperse a . filter (/= mempty)

uncommas' :: Monoid a => [(TLB.Builder, a)] -> (TLB.Builder, a)
uncommas' = (uncommas *** mconcat) . unzip

makeCte :: IdentInfo -> [CommonTableExpressionClause] -> (TLB.Builder, [PersistValue])
makeCte info cteClauses =
  let
    withCteText
        | hasRecursive = "WITH RECURSIVE "
        | otherwise = "WITH "
      where
        hasRecursive =
            elem RecursiveCommonTableExpression
            $ fmap (\(CommonTableExpressionClause cteKind _ _) -> cteKind)
            $ cteClauses

    cteClauseToText (CommonTableExpressionClause _ cteIdent cteFn) =
        first
            (\tlb -> useIdent info cteIdent <> " AS " <> parens tlb)
            (cteFn info)

    cteBody =
        mconcat
        $ intersperse (",\n", mempty)
        $ fmap cteClauseToText cteClauses
  in
    case cteClauses of
        [] ->
            mempty
        _ ->
          first (\tlb -> withCteText <> tlb <> "\n") cteBody

makeInsertInto :: SqlSelect a r => IdentInfo -> Mode -> a -> (TLB.Builder, [PersistValue])
makeInsertInto info INSERT_INTO ret = sqlInsertInto info ret
makeInsertInto _    _           _   = mempty

makeSelect :: SqlSelect a r => IdentInfo -> Mode -> DistinctClause -> a -> (TLB.Builder, [PersistValue])
makeSelect info mode_ distinctClause ret = process mode_
  where
    process mode =
        case mode of
            SELECT      -> withCols selectKind
            DELETE      -> plain "DELETE "
            UPDATE      -> plain "UPDATE "
            INSERT_INTO -> process SELECT
    selectKind =
        case distinctClause of
            DistinctAll      -> ("SELECT ", [])
            DistinctStandard -> ("SELECT DISTINCT ", [])
            DistinctOn exprs ->
                first (("SELECT DISTINCT ON (" <>) . (<> ") "))
                $ uncommas' (processExpr <$> exprs)
      where
        processExpr e = materializeExpr info (coerce e :: SqlExpr (Value a))
    withCols v = v <> sqlSelectCols info ret
    plain    v = (v, [])

makeFrom
    :: IdentInfo
    -> Mode
    -> [FromClause]
    -> (TLB.Builder, [PersistValue])
makeFrom _    _    [] = mempty
makeFrom info mode fs = ret
  where
    ret =
        case collectOnClauses (fst info) fs of
            Left expr -> throw $ mkExc expr
            Right fs' -> keyword $ uncommas' (map (mk Never) fs')
    keyword =
        case mode of
            UPDATE -> id
            _      -> first ("\nFROM " <>)

    mk _     (FromStart i def) = base i def
    mk paren (FromJoin lhs kind rhs monClause) =
        first (parensM paren) $
        mconcat [ mk Never lhs
                , (fromKind kind, mempty)
                , mk Parens rhs
                , maybe mempty makeOnClause monClause
                ]
    mk _ (OnClause _) = throw (UnexpectedCaseErr MakeFromError)
    mk paren (FromRaw f) = f paren info

    base ident@(I identText) def =
        let db@(DBName dbText) = coerce $ getEntityDBName def
        in ( fromDBName info db <>
                 if dbText == identText
                 then mempty
                 else " AS " <> useIdent info ident
           , mempty
           )

    fromKind InnerJoinKind      = " INNER JOIN "
    fromKind CrossJoinKind      = " CROSS JOIN "
    fromKind LeftOuterJoinKind  = " LEFT OUTER JOIN "
    fromKind RightOuterJoinKind = " RIGHT OUTER JOIN "
    fromKind FullOuterJoinKind  = " FULL OUTER JOIN "

    makeOnClause (ERaw _ f)        = first (" ON " <>) (f Never info)

    mkExc :: SqlExpr (Value Bool) -> OnClauseWithoutMatchingJoinException
    mkExc (ERaw _ f) =
        OnClauseWithoutMatchingJoinException $
            TL.unpack $ TLB.toLazyText $ fst (f Never info)

makeSet :: IdentInfo -> [SetClause] -> (TLB.Builder, [PersistValue])
makeSet _    [] = mempty
makeSet info os = first ("\nSET " <>) . uncommas' $ concatMap mk os
  where
    mk (SetClause (ERaw _ f))             = [f Never info]

makeWhere :: IdentInfo -> WhereClause -> (TLB.Builder, [PersistValue])
makeWhere _    NoWhere            = mempty
makeWhere info (Where (ERaw _ f)) = first ("\nWHERE " <>) $ f Never info

makeGroupBy :: IdentInfo -> GroupByClause -> (TLB.Builder, [PersistValue])
makeGroupBy _ (GroupBy []) = (mempty, [])
makeGroupBy info (GroupBy fields) = first ("\nGROUP BY " <>) build
  where
    build :: (TLB.Builder, [PersistValue])
    build = uncommas' $ map match fields

    match :: SomeValue -> (TLB.Builder, [PersistValue])
    match (SomeValue (ERaw _ f)) = f Never info

makeHaving :: IdentInfo -> WhereClause -> (TLB.Builder, [PersistValue])
makeHaving _    NoWhere   = mempty
makeHaving info (Where (ERaw _ f)) = first ("\nHAVING " <>) $ f Never info

-- makeHaving, makeWhere and makeOrderBy
makeOrderByNoNewline
    :: IdentInfo -> [OrderByClause] -> (TLB.Builder, [PersistValue])
makeOrderByNoNewline _    [] = mempty
makeOrderByNoNewline info os = first ("ORDER BY " <>) . uncommas' $ concatMap mk os
  where
    mk :: OrderByClause -> [(TLB.Builder, [PersistValue])]
    mk (ERaw _ f) = [f Never info]

makeOrderBy :: IdentInfo -> [OrderByClause] -> (TLB.Builder, [PersistValue])
makeOrderBy _ [] = mempty
makeOrderBy info is =
    let (tlb, vals) = makeOrderByNoNewline info is
    in (tlb, vals)

makeLimit :: IdentInfo -> LimitClause -> (TLB.Builder, [PersistValue])
makeLimit (conn, _) (Limit ml mo) =
    let limitRaw = getConnLimitOffset (v ml, v mo) "" conn
        v :: Maybe Int64 -> Int
        v = maybe 0 fromIntegral
    in (TLB.fromText limitRaw, mempty)

makeLocking :: IdentInfo -> LockingClause -> (TLB.Builder, [PersistValue])
makeLocking _ (LegacyLockingClause lockingClause) =
    case lockingClause of
        ForUpdate           -> ("\nFOR UPDATE", [])
        ForUpdateSkipLocked -> ("\nFOR UPDATE SKIP LOCKED", [])
        ForShare            -> ("\nFOR SHARE", [])
        LockInShareMode     -> ("\nLOCK IN SHARE MODE", [])
makeLocking info (PostgresLockingClauses clauses) =
    List.foldl' combineBuilderValPairs ("",[]) (makePostgresLockingClauses <$> clauses)
        where
            combineBuilderValPairs (builder1, persistvals1) (builder2,persistvals2) =
                (builder1 <> builder2 <> "\n", persistvals1 <> persistvals2)

            makePostgresLockingClauses :: PostgresLockingKind -> (TLB.Builder , [PersistValue])
            makePostgresLockingClauses l =
                makeLockingStrength (postgresRowLevelLockStrength l)
                    <> plain " "
                    <> makeOfClause (postgresLockingOfClause l)
                    <> plain " "
                    <> makeLockingBehavior (postgresOnLockedBehavior l)
            makeLockingStrength :: PostgresRowLevelLockStrength -> (TLB.Builder, [PersistValue])
            makeLockingStrength PostgresForUpdate = plain "FOR UPDATE"
            makeLockingStrength PostgresForShare = plain "FOR SHARE"

            makeLockingBehavior :: OnLockedBehavior -> (TLB.Builder, [PersistValue])
            makeLockingBehavior NoWait = plain "NOWAIT"
            makeLockingBehavior SkipLocked = plain "SKIP LOCKED"
            makeLockingBehavior Wait = plain ""

            makeOfClause :: Maybe LockingOfClause -> (TLB.Builder, [PersistValue])
            makeOfClause (Just (LockingOfClause lockableEnts)) = plain "OF " <> makeLockableEntity info lockableEnts
            makeOfClause Nothing = plain ""

            plain v = (v,[])
makeLocking _ NoLockingClause = mempty

parens :: TLB.Builder -> TLB.Builder
parens b = "(" <> (b <> ")")

aliasedEntityColumnIdent :: Ident -> FieldDef -> Ident
aliasedEntityColumnIdent (I baseIdent) field =
    I (baseIdent <> "_" <> (unDBName $ coerce $ fieldDB field))

aliasedColumnName :: Ident -> IdentInfo -> T.Text -> TLB.Builder
aliasedColumnName (I baseIdent) info columnName =
    useIdent info (I (baseIdent <> "_" <> columnName))

-- | (Internal) Class for mapping results coming from 'SqlQuery'
-- into actual results.
--
-- This looks very similar to @RawSql@, and it is!  However,
-- there are some crucial differences and ultimately they're
-- different classes.
class SqlSelect a r | a -> r, r -> a where
    -- | Creates the variable part of the @SELECT@ query and
    -- returns the list of 'PersistValue's that will be given to
    -- 'rawQuery'.
    sqlSelectCols :: IdentInfo -> a -> (TLB.Builder, [PersistValue])

    -- | Number of columns that will be consumed.
    sqlSelectColCount :: Proxy a -> Int

    -- | Transform a row of the result into the data type.
    sqlSelectProcessRow :: [PersistValue] -> Either T.Text r

    -- | Create @INSERT INTO@ clause instead.
    sqlInsertInto :: IdentInfo -> a -> (TLB.Builder, [PersistValue])
    sqlInsertInto = throw (UnexpectedCaseErr UnsupportedSqlInsertIntoType)


-- | @INSERT INTO@ hack.
instance PersistEntity e => SqlSelect (SqlExpr (Insertion e)) (Insertion e) where
    sqlInsertInto info e =
        let fields =
                uncommas $
                map (fromDBName info . coerce . fieldDB) $
                getEntityFields $
                entityDef (proxy e)

            proxy :: SqlExpr (Insertion a) -> Proxy a
            proxy = const Proxy

            table  =
                fromDBName info . DBName . coerce . getEntityDBName . entityDef . proxy
        in
            ("INSERT INTO " <> table e <> parens fields <> "\n", [])
    sqlSelectCols info (ERaw _ f) = f Never info
    sqlSelectColCount   = const 0
    sqlSelectProcessRow =
      const (Right (throw (UnexpectedCaseErr InsertionFinalError)))

-- | Not useful for 'select', but used for 'update' and 'delete'.
instance SqlSelect () () where
    sqlSelectCols _ _ = ("1", [])
    sqlSelectColCount _ = 1
    sqlSelectProcessRow _ = Right ()

unescapedColumnNames :: EntityDef -> [DBName]
unescapedColumnNames ent =
    addIdColumn rest
  where
    rest =
        map (coerce . fieldDB) (getEntityFields ent)
    addIdColumn =
        case getEntityId ent of
            EntityIdField fd ->
                (:) (coerce (fieldDB fd))
            EntityIdNaturalKey _ ->
                id

-- | You may return an 'Entity' from a 'select' query.
instance PersistEntity a => SqlSelect (SqlExpr (Entity a)) (Entity a) where
    sqlSelectCols info expr@(ERaw m f)
      | Just baseIdent <- sqlExprMetaAlias m, False <- sqlExprMetaIsReference m =
          let process = uncommas $
                           map ((name <>) . aliasName) $
                           unescapedColumnNames ed
              aliasName columnName = (fromDBName info columnName) <> " AS " <> aliasedColumnName baseIdent info (unDBName columnName)
              name = fst (f Never info) <> "."
              ed = entityDef $ getEntityVal $ return expr
          in (process, mempty)
      | Just baseIdent <- sqlExprMetaAlias m, True <- sqlExprMetaIsReference m =
          let process = uncommas $
                           map ((name <>) . aliasedColumnName baseIdent info . unDBName) $
                           unescapedColumnNames ed
              name = fst (f Never info) <> "."
              ed = entityDef $ getEntityVal $ return expr
          in (process, mempty)
      | otherwise =
          let process =
                  uncommas
                  $ map ((name <>) . TLB.fromText)
                  $ NEL.toList
                  $ keyAndEntityColumnNames ed (fst info)
              -- 'name' is the biggest difference between 'RawSql' and
              -- 'SqlSelect'.  We automatically create names for tables
              -- (since it's not the user who's writing the FROM
              -- clause), while 'rawSql' assumes that it's just the
              -- name of the table (which doesn't allow self-joins, for
              -- example).
              name = fst (f Never info) <> "."
              ed = entityDef $ getEntityVal $ return expr
          in (process, mempty)

    sqlSelectColCount = entityColumnCount . entityDef . getEntityVal
    sqlSelectProcessRow = parseEntityValues ed
      where
        ed = entityDef $ getEntityVal (Proxy :: Proxy (SqlExpr (Entity a)))

getEntityVal :: Proxy (SqlExpr (Entity a)) -> Proxy a
getEntityVal = const Proxy

-- | You may return a possibly-@NULL@ 'Entity' from a 'select' query.
instance PersistEntity a => SqlSelect (SqlExpr (Maybe (Entity a))) (Maybe (Entity a)) where
    sqlSelectCols info e = sqlSelectCols info (coerce e :: SqlExpr (Entity a))
    sqlSelectColCount = sqlSelectColCount . fromEMaybe
      where
        fromEMaybe :: Proxy (SqlExpr (Maybe e)) -> Proxy (SqlExpr e)
        fromEMaybe = const Proxy
    sqlSelectProcessRow cols
        | all (== PersistNull) cols = return Nothing
        | otherwise                 = Just <$> sqlSelectProcessRow cols


-- | You may return any single value (i.e. a single column) from
-- a 'select' query.
instance PersistField a => SqlSelect (SqlExpr (Value a)) (Value a) where
    sqlSelectCols = materializeExpr
    sqlSelectColCount = const 1
    sqlSelectProcessRow [pv] = Value <$> fromPersistValue pv
    sqlSelectProcessRow pvs  = Value <$> fromPersistValue (PersistList pvs)

-- | Materialize a @SqlExpr (Value a)@.
materializeExpr :: IdentInfo -> SqlExpr (Value a) -> (TLB.Builder, [PersistValue])
materializeExpr info (ERaw m f)
    | Just fields <- sqlExprMetaCompositeFields m = (uncommas $ fmap parens $ fields info, [])
    | Just alias <- sqlExprMetaAlias m
    , not (sqlExprMetaIsReference m) = first (<> " AS " <> useIdent info alias) (f Parens info)
    | otherwise = f Parens info


-- | You may return tuples (up to 16-tuples) and tuples of tuples
-- from a 'select' query.
instance (SqlSelect a ra, SqlSelect b rb) => SqlSelect (a, b) (ra, rb) where
    sqlSelectCols esc (a, b) =
        uncommas'
            [ sqlSelectCols esc a
            , sqlSelectCols esc b
            ]
    sqlSelectColCount = uncurry (+) . (sqlSelectColCount *** sqlSelectColCount) . fromTuple
      where
        fromTuple :: Proxy (a,b) -> (Proxy a, Proxy b)
        fromTuple = const (Proxy, Proxy)
    sqlSelectProcessRow =
      let x = getType processRow
          getType :: SqlSelect a r => (z -> Either y (r,x)) -> Proxy a
          getType = const Proxy

          colCountFst = sqlSelectColCount x

          processRow row =
              let (rowFst, rowSnd) = splitAt colCountFst row
              in (,) <$> sqlSelectProcessRow rowFst
                     <*> sqlSelectProcessRow rowSnd

      in colCountFst `seq` processRow
         -- Avoids recalculating 'colCountFst'.

instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         ) => SqlSelect (a, b, c) (ra, rb, rc) where
  sqlSelectCols esc (a, b, c) =
    uncommas'
      [ sqlSelectCols esc a
      , sqlSelectCols esc b
      , sqlSelectCols esc c
      ]
  sqlSelectColCount   = sqlSelectColCount . from3P
  sqlSelectProcessRow = fmap to3 . sqlSelectProcessRow

from3P :: Proxy (a,b,c) -> Proxy ((a,b),c)
from3P = const Proxy

from3 :: (a,b,c) -> ((a,b),c)
from3 (a,b,c) = ((a,b),c)

to3 :: ((a,b),c) -> (a,b,c)
to3 ((a,b),c) = (a,b,c)

instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         , SqlSelect d rd
         ) => SqlSelect (a, b, c, d) (ra, rb, rc, rd) where
  sqlSelectCols esc (a, b, c, d) =
    uncommas'
      [ sqlSelectCols esc a
      , sqlSelectCols esc b
      , sqlSelectCols esc c
      , sqlSelectCols esc d
      ]
  sqlSelectColCount   = sqlSelectColCount . from4P
  sqlSelectProcessRow = fmap to4 . sqlSelectProcessRow

from4P :: Proxy (a,b,c,d) -> Proxy ((a,b),(c,d))
from4P = const Proxy

from4 :: (a,b,c,d) -> ((a,b),(c,d))
from4 (a,b,c,d) = ((a,b),(c,d))

to4 :: ((a,b),(c,d)) -> (a,b,c,d)
to4 ((a,b),(c,d)) = (a,b,c,d)

instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         , SqlSelect d rd
         , SqlSelect e re
         ) => SqlSelect (a, b, c, d, e) (ra, rb, rc, rd, re) where
  sqlSelectCols esc (a, b, c, d, e) =
    uncommas'
      [ sqlSelectCols esc a
      , sqlSelectCols esc b
      , sqlSelectCols esc c
      , sqlSelectCols esc d
      , sqlSelectCols esc e
      ]
  sqlSelectColCount   = sqlSelectColCount . from5P
  sqlSelectProcessRow = fmap to5 . sqlSelectProcessRow

from5P :: Proxy (a,b,c,d,e) -> Proxy ((a,b),(c,d),e)
from5P = const Proxy

from5 :: (a,b,c,d,e) -> ((a,b),(c,d),e)
from5 (a,b,c,d,e) = ((a,b),(c,d),e)

to5 :: ((a,b),(c,d),e) -> (a,b,c,d,e)
to5 ((a,b),(c,d),e) = (a,b,c,d,e)

instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         , SqlSelect d rd
         , SqlSelect e re
         , SqlSelect f rf
         ) => SqlSelect (a, b, c, d, e, f) (ra, rb, rc, rd, re, rf) where
  sqlSelectCols esc (a, b, c, d, e, f) =
    uncommas'
      [ sqlSelectCols esc a
      , sqlSelectCols esc b
      , sqlSelectCols esc c
      , sqlSelectCols esc d
      , sqlSelectCols esc e
      , sqlSelectCols esc f
      ]
  sqlSelectColCount   = sqlSelectColCount . from6P
  sqlSelectProcessRow = fmap to6 . sqlSelectProcessRow

from6P :: Proxy (a,b,c,d,e,f) -> Proxy ((a,b),(c,d),(e,f))
from6P = const Proxy

from6 :: (a,b,c,d,e,f) -> ((a,b),(c,d),(e,f))
from6 (a,b,c,d,e,f) = ((a,b),(c,d),(e,f))

to6 :: ((a,b),(c,d),(e,f)) -> (a,b,c,d,e,f)
to6 ((a,b),(c,d),(e,f)) = (a,b,c,d,e,f)

instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         , SqlSelect d rd
         , SqlSelect e re
         , SqlSelect f rf
         , SqlSelect g rg
         ) => SqlSelect (a, b, c, d, e, f, g) (ra, rb, rc, rd, re, rf, rg) where
  sqlSelectCols esc (a, b, c, d, e, f, g) =
    uncommas'
      [ sqlSelectCols esc a
      , sqlSelectCols esc b
      , sqlSelectCols esc c
      , sqlSelectCols esc d
      , sqlSelectCols esc e
      , sqlSelectCols esc f
      , sqlSelectCols esc g
      ]
  sqlSelectColCount   = sqlSelectColCount . from7P
  sqlSelectProcessRow = fmap to7 . sqlSelectProcessRow

from7P :: Proxy (a,b,c,d,e,f,g) -> Proxy ((a,b),(c,d),(e,f),g)
from7P = const Proxy

from7 :: (a,b,c,d,e,f,g) -> ((a,b),(c,d),(e,f),g)
from7 (a,b,c,d,e,f,g) = ((a,b),(c,d),(e,f),g)

to7 :: ((a,b),(c,d),(e,f),g) -> (a,b,c,d,e,f,g)
to7 ((a,b),(c,d),(e,f),g) = (a,b,c,d,e,f,g)

instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         , SqlSelect d rd
         , SqlSelect e re
         , SqlSelect f rf
         , SqlSelect g rg
         , SqlSelect h rh
         ) => SqlSelect (a, b, c, d, e, f, g, h) (ra, rb, rc, rd, re, rf, rg, rh) where
  sqlSelectCols esc (a, b, c, d, e, f, g, h) =
    uncommas'
      [ sqlSelectCols esc a
      , sqlSelectCols esc b
      , sqlSelectCols esc c
      , sqlSelectCols esc d
      , sqlSelectCols esc e
      , sqlSelectCols esc f
      , sqlSelectCols esc g
      , sqlSelectCols esc h
      ]
  sqlSelectColCount   = sqlSelectColCount . from8P
  sqlSelectProcessRow = fmap to8 . sqlSelectProcessRow

from8P :: Proxy (a,b,c,d,e,f,g,h) -> Proxy ((a,b),(c,d),(e,f),(g,h))
from8P = const Proxy

from8 :: (a,b,c,d,e,f,g,h) -> ((a,b),(c,d),(e,f),(g,h))
from8 (a,b,c,d,e,f,g,h) = ((a,b),(c,d),(e,f),(g,h))

to8 :: ((a,b),(c,d),(e,f),(g,h)) -> (a,b,c,d,e,f,g,h)
to8 ((a,b),(c,d),(e,f),(g,h)) = (a,b,c,d,e,f,g,h)

instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         , SqlSelect d rd
         , SqlSelect e re
         , SqlSelect f rf
         , SqlSelect g rg
         , SqlSelect h rh
         , SqlSelect i ri
         ) => SqlSelect (a, b, c, d, e, f, g, h, i) (ra, rb, rc, rd, re, rf, rg, rh, ri) where
  sqlSelectCols esc (a, b, c, d, e, f, g, h, i) =
    uncommas'
      [ sqlSelectCols esc a
      , sqlSelectCols esc b
      , sqlSelectCols esc c
      , sqlSelectCols esc d
      , sqlSelectCols esc e
      , sqlSelectCols esc f
      , sqlSelectCols esc g
      , sqlSelectCols esc h
      , sqlSelectCols esc i
      ]
  sqlSelectColCount   = sqlSelectColCount . from9P
  sqlSelectProcessRow = fmap to9 . sqlSelectProcessRow

from9P :: Proxy (a,b,c,d,e,f,g,h,i) -> Proxy ((a,b),(c,d),(e,f),(g,h),i)
from9P = const Proxy

from9 :: (a,b,c,d,e,f,g,h,i) -> ((a,b),(c,d),(e,f),(g,h),i)
from9 (a,b,c,d,e,f,g,h,i) = ((a,b),(c,d),(e,f),(g,h),i)

to9 :: ((a,b),(c,d),(e,f),(g,h),i) -> (a,b,c,d,e,f,g,h,i)
to9 ((a,b),(c,d),(e,f),(g,h),i) = (a,b,c,d,e,f,g,h,i)

instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         , SqlSelect d rd
         , SqlSelect e re
         , SqlSelect f rf
         , SqlSelect g rg
         , SqlSelect h rh
         , SqlSelect i ri
         , SqlSelect j rj
         ) => SqlSelect (a, b, c, d, e, f, g, h, i, j) (ra, rb, rc, rd, re, rf, rg, rh, ri, rj) where
  sqlSelectCols esc (a, b, c, d, e, f, g, h, i, j) =
    uncommas'
      [ sqlSelectCols esc a
      , sqlSelectCols esc b
      , sqlSelectCols esc c
      , sqlSelectCols esc d
      , sqlSelectCols esc e
      , sqlSelectCols esc f
      , sqlSelectCols esc g
      , sqlSelectCols esc h
      , sqlSelectCols esc i
      , sqlSelectCols esc j
      ]
  sqlSelectColCount   = sqlSelectColCount . from10P
  sqlSelectProcessRow = fmap to10 . sqlSelectProcessRow

from10P :: Proxy (a,b,c,d,e,f,g,h,i,j) -> Proxy ((a,b),(c,d),(e,f),(g,h),(i,j))
from10P = const Proxy

from10 :: (a,b,c,d,e,f,g,h,i,j) -> ((a,b),(c,d),(e,f),(g,h),(i,j))
from10 (a,b,c,d,e,f,g,h,i,j) = ((a,b),(c,d),(e,f),(g,h),(i,j))

to10 :: ((a,b),(c,d),(e,f),(g,h),(i,j)) -> (a,b,c,d,e,f,g,h,i,j)
to10 ((a,b),(c,d),(e,f),(g,h),(i,j)) = (a,b,c,d,e,f,g,h,i,j)

instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         , SqlSelect d rd
         , SqlSelect e re
         , SqlSelect f rf
         , SqlSelect g rg
         , SqlSelect h rh
         , SqlSelect i ri
         , SqlSelect j rj
         , SqlSelect k rk
         ) => SqlSelect (a, b, c, d, e, f, g, h, i, j, k) (ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk) where
  sqlSelectCols esc (a, b, c, d, e, f, g, h, i, j, k) =
    uncommas'
      [ sqlSelectCols esc a
      , sqlSelectCols esc b
      , sqlSelectCols esc c
      , sqlSelectCols esc d
      , sqlSelectCols esc e
      , sqlSelectCols esc f
      , sqlSelectCols esc g
      , sqlSelectCols esc h
      , sqlSelectCols esc i
      , sqlSelectCols esc j
      , sqlSelectCols esc k
      ]
  sqlSelectColCount   = sqlSelectColCount . from11P
  sqlSelectProcessRow = fmap to11 . sqlSelectProcessRow

from11P :: Proxy (a,b,c,d,e,f,g,h,i,j,k) -> Proxy ((a,b),(c,d),(e,f),(g,h),(i,j),k)
from11P = const Proxy

from11 :: (a,b,c,d,e,f,g,h,i,j,k) -> ((a,b),(c,d),(e,f),(g,h),(i,j),k)
from11 (a,b,c,d,e,f,g,h,i,j,k) = ((a,b),(c,d),(e,f),(g,h),(i,j),k)

to11 :: ((a,b),(c,d),(e,f),(g,h),(i,j),k) -> (a,b,c,d,e,f,g,h,i,j,k)
to11 ((a,b),(c,d),(e,f),(g,h),(i,j),k) = (a,b,c,d,e,f,g,h,i,j,k)

instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         , SqlSelect d rd
         , SqlSelect e re
         , SqlSelect f rf
         , SqlSelect g rg
         , SqlSelect h rh
         , SqlSelect i ri
         , SqlSelect j rj
         , SqlSelect k rk
         , SqlSelect l rl
         ) => SqlSelect (a, b, c, d, e, f, g, h, i, j, k, l) (ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk, rl) where
  sqlSelectCols esc (a, b, c, d, e, f, g, h, i, j, k, l) =
    uncommas'
      [ sqlSelectCols esc a
      , sqlSelectCols esc b
      , sqlSelectCols esc c
      , sqlSelectCols esc d
      , sqlSelectCols esc e
      , sqlSelectCols esc f
      , sqlSelectCols esc g
      , sqlSelectCols esc h
      , sqlSelectCols esc i
      , sqlSelectCols esc j
      , sqlSelectCols esc k
      , sqlSelectCols esc l
      ]
  sqlSelectColCount   = sqlSelectColCount . from12P
  sqlSelectProcessRow = fmap to12 . sqlSelectProcessRow

from12P :: Proxy (a,b,c,d,e,f,g,h,i,j,k,l) -> Proxy ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l))
from12P = const Proxy

from12 :: (a,b,c,d,e,f,g,h,i,j,k,l) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l))
from12 (a,b,c,d,e,f,g,h,i,j,k,l) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l))

to12 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l)) -> (a,b,c,d,e,f,g,h,i,j,k,l)
to12 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l)) = (a,b,c,d,e,f,g,h,i,j,k,l)

instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         , SqlSelect d rd
         , SqlSelect e re
         , SqlSelect f rf
         , SqlSelect g rg
         , SqlSelect h rh
         , SqlSelect i ri
         , SqlSelect j rj
         , SqlSelect k rk
         , SqlSelect l rl
         , SqlSelect m rm
         ) => SqlSelect (a, b, c, d, e, f, g, h, i, j, k, l, m) (ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk, rl, rm) where
  sqlSelectCols esc (a, b, c, d, e, f, g, h, i, j, k, l, m) =
    uncommas'
      [ sqlSelectCols esc a
      , sqlSelectCols esc b
      , sqlSelectCols esc c
      , sqlSelectCols esc d
      , sqlSelectCols esc e
      , sqlSelectCols esc f
      , sqlSelectCols esc g
      , sqlSelectCols esc h
      , sqlSelectCols esc i
      , sqlSelectCols esc j
      , sqlSelectCols esc k
      , sqlSelectCols esc l
      , sqlSelectCols esc m
      ]
  sqlSelectColCount   = sqlSelectColCount . from13P
  sqlSelectProcessRow = fmap to13 . sqlSelectProcessRow

from13P :: Proxy (a,b,c,d,e,f,g,h,i,j,k,l,m) -> Proxy ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),m)
from13P = const Proxy

from13 :: (a,b,c,d,e,f,g,h,i,j,k,l,m) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),m)
from13 (a,b,c,d,e,f,g,h,i,j,k,l,m) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),m)

to13 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),m) -> (a,b,c,d,e,f,g,h,i,j,k,l,m)
to13 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),m) = (a,b,c,d,e,f,g,h,i,j,k,l,m)

instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         , SqlSelect d rd
         , SqlSelect e re
         , SqlSelect f rf
         , SqlSelect g rg
         , SqlSelect h rh
         , SqlSelect i ri
         , SqlSelect j rj
         , SqlSelect k rk
         , SqlSelect l rl
         , SqlSelect m rm
         , SqlSelect n rn
         ) => SqlSelect (a, b, c, d, e, f, g, h, i, j, k, l, m, n) (ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk, rl, rm, rn) where
  sqlSelectCols esc (a, b, c, d, e, f, g, h, i, j, k, l, m, n) =
    uncommas'
      [ sqlSelectCols esc a
      , sqlSelectCols esc b
      , sqlSelectCols esc c
      , sqlSelectCols esc d
      , sqlSelectCols esc e
      , sqlSelectCols esc f
      , sqlSelectCols esc g
      , sqlSelectCols esc h
      , sqlSelectCols esc i
      , sqlSelectCols esc j
      , sqlSelectCols esc k
      , sqlSelectCols esc l
      , sqlSelectCols esc m
      , sqlSelectCols esc n
      ]
  sqlSelectColCount   = sqlSelectColCount . from14P
  sqlSelectProcessRow = fmap to14 . sqlSelectProcessRow

from14P :: Proxy (a,b,c,d,e,f,g,h,i,j,k,l,m,n) -> Proxy ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n))
from14P = const Proxy

from14 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n))
from14 (a,b,c,d,e,f,g,h,i,j,k,l,m,n) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n))

to14 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n)) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n)
to14 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n)) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n)

instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         , SqlSelect d rd
         , SqlSelect e re
         , SqlSelect f rf
         , SqlSelect g rg
         , SqlSelect h rh
         , SqlSelect i ri
         , SqlSelect j rj
         , SqlSelect k rk
         , SqlSelect l rl
         , SqlSelect m rm
         , SqlSelect n rn
         , SqlSelect o ro
         ) => SqlSelect (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) (ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk, rl, rm, rn, ro) where
  sqlSelectCols esc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) =
    uncommas'
      [ sqlSelectCols esc a
      , sqlSelectCols esc b
      , sqlSelectCols esc c
      , sqlSelectCols esc d
      , sqlSelectCols esc e
      , sqlSelectCols esc f
      , sqlSelectCols esc g
      , sqlSelectCols esc h
      , sqlSelectCols esc i
      , sqlSelectCols esc j
      , sqlSelectCols esc k
      , sqlSelectCols esc l
      , sqlSelectCols esc m
      , sqlSelectCols esc n
      , sqlSelectCols esc o
      ]
  sqlSelectColCount   = sqlSelectColCount . from15P
  sqlSelectProcessRow = fmap to15 . sqlSelectProcessRow

from15P :: Proxy (a,b,c,d,e,f,g,h,i,j,k,l,m,n, o) -> Proxy ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),o)
from15P = const Proxy

from15 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),o)
from15 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),o)

to15 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),o) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
to15 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),o) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)

instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         , SqlSelect d rd
         , SqlSelect e re
         , SqlSelect f rf
         , SqlSelect g rg
         , SqlSelect h rh
         , SqlSelect i ri
         , SqlSelect j rj
         , SqlSelect k rk
         , SqlSelect l rl
         , SqlSelect m rm
         , SqlSelect n rn
         , SqlSelect o ro
         , SqlSelect p rp
         ) => SqlSelect (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) (ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk, rl, rm, rn, ro, rp) where
  sqlSelectCols esc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) =
    uncommas'
      [ sqlSelectCols esc a
      , sqlSelectCols esc b
      , sqlSelectCols esc c
      , sqlSelectCols esc d
      , sqlSelectCols esc e
      , sqlSelectCols esc f
      , sqlSelectCols esc g
      , sqlSelectCols esc h
      , sqlSelectCols esc i
      , sqlSelectCols esc j
      , sqlSelectCols esc k
      , sqlSelectCols esc l
      , sqlSelectCols esc m
      , sqlSelectCols esc n
      , sqlSelectCols esc o
      , sqlSelectCols esc p
      ]
  sqlSelectColCount   = sqlSelectColCount . from16P
  sqlSelectProcessRow = fmap to16 . sqlSelectProcessRow

from16P :: Proxy (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) -> Proxy ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p))
from16P = const Proxy

from16 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p))
from16 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p))

to16 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p)) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
to16 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p)) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)

-- | Insert a 'PersistField' for every selected value.
--
-- @since 2.4.2
insertSelect
    :: (MonadIO m, PersistEntity a, SqlBackendCanWrite backend)
    => SqlQuery (SqlExpr (Insertion a))
    -> R.ReaderT backend m ()
insertSelect a = void $ insertSelectCount a

-- | Insert a 'PersistField' for every selected value, return the count afterward
insertSelectCount
    :: (MonadIO m, PersistEntity a, SqlBackendCanWrite backend)
    => SqlQuery (SqlExpr (Insertion a))
    -> R.ReaderT backend m Int64
insertSelectCount a = rawEsqueleto INSERT_INTO a

-- | Renders an expression into 'Text'. Only useful for creating a textual
-- representation of the clauses passed to an "On" clause.
--
-- @since 3.2.0
renderExpr :: SqlBackend -> SqlExpr (Value Bool) -> T.Text
renderExpr sqlBackend e = case e of
    ERaw _ mkBuilderValues ->
         let (builder, _) = mkBuilderValues Never (sqlBackend, initialIdentState)
         in (builderToText builder)

-- | An exception thrown by 'RenderExpr' - it's not designed to handle composite
-- keys, and will blow up if you give it one.
--
-- @since 3.2.0
data RenderExprException = RenderExprUnexpectedECompositeKey T.Text
    deriving Show

-- |
--
-- @since 3.2.0
instance Exception RenderExprException

-- | @valkey i = 'val' . 'toSqlKey'@
-- (<https://github.com/prowdsponsor/esqueleto/issues/9>).
valkey
    :: (ToBackendKey SqlBackend entity, PersistField (Key entity))
    => Int64 -> SqlExpr (Value (Key entity))
valkey = val . toSqlKey

-- | @valJ@ is like @val@ but for something that is already a @Value@. The use
-- case it was written for was, given a @Value@ lift the @Key@ for that @Value@
-- into the query expression in a type safe way. However, the implementation is
-- more generic than that so we call it @valJ@.
--
-- Its important to note that the input entity and the output entity are
-- constrained to be the same by the type signature on the function
-- (<https://github.com/prowdsponsor/esqueleto/pull/69>).
--
-- @since 1.4.2
valJ
    :: (PersistField (Key entity))
    => Value (Key entity)
    -> SqlExpr (Value (Key entity))
valJ = val . unValue


-- | Synonym for 'Database.Persist.Store.delete' that does not
-- clash with @esqueleto@'s 'delete'.
deleteKey
    ::
    ( PersistStore backend
    , BaseBackend backend ~ PersistEntityBackend val
    , MonadIO m
    , PersistEntity val
    )
    => Key val
    -> R.ReaderT backend m ()
deleteKey = Database.Persist.delete

-- | Avoid N+1 queries and join entities into a map structure.
--
-- This function is useful to call on the result of a single @JOIN@. For
-- example, suppose you have this query:
--
-- @
-- getFoosAndNestedBarsFromParent
--     :: ParentId
--     -> SqlPersistT IO [(Entity Foo, Maybe (Entity Bar))]
-- getFoosAndNestedBarsFromParent parentId =
--     'select' $ do
--         (foo :& bar) <- from $
--             table @Foo
--             ``LeftOuterJoin``
--             table @Bar
--                 ``on`` do
--                     \\(foo :& bar) ->
--                         foo ^. FooId ==. bar ?. BarFooId
--         where_ $
--             foo ^. FooParentId ==. val parentId
--         pure (foo, bar)
-- @
--
-- This is a natural result type for SQL - a list of tuples. However, it's not
-- what we usually want in Haskell - each @Foo@ in the list will be represented
-- multiple times, once for each @Bar@.
--
-- We can write @'fmap' 'associateJoin'@ and it will translate it into a 'Map'
-- that is keyed on the 'Key' of the left 'Entity', and the value is a tuple of
-- the entity's value as well as the list of each coresponding entity.
--
-- @
-- getFoosAndNestedBarsFromParentHaskellese
--     :: ParentId
--     -> SqlPersistT (Map (Key Foo) (Foo, [Maybe (Entity Bar)]))
-- getFoosAndNestedBarsFromParentHaskellese parentId =
--     'fmap' 'associateJoin' $ getFoosdAndNestedBarsFromParent parentId
-- @
--
-- What if you have multiple joins?
--
-- Let's use 'associateJoin' with a *two* join query.
--
-- @
-- userPostComments
--     :: SqlQuery (SqlExpr (Entity User, Entity Post, Entity Comment))
-- userPostsComment = do
--     (u :& p :& c) <- from $
--         table @User
--         ``InnerJoin``
--         table @Post
--             `on` do
--                 \\(u :& p) ->
--                     u ^. UserId ==. p ^. PostUserId
--         ``InnerJoin``
--         table @Comment
--             ``on`` do
--                 \\(_ :& p :& c) ->
--                     p ^. PostId ==. c ^. CommentPostId
--     pure (u, p, c)
-- @
--
-- This query returns a User, with all of the users Posts, and then all of the
-- Comments on that post.
--
-- First, we *nest* the tuple.
--
-- @
-- nest :: (a, b, c) -> (a, (b, c))
-- nest (a, b, c) = (a, (b, c))
-- @
--
-- This makes the return of the query conform to the input expected from
-- 'associateJoin'.
--
-- @
-- nestedUserPostComments
--     :: SqlPersistT IO [(Entity User, (Entity Post, Entity Comment))]
-- nestedUserPostComments =
--     fmap nest $ select userPostsComments
-- @
--
-- Now, we can call 'associateJoin' on it.
--
-- @
-- associateUsers
--     :: [(Entity User, (Entity Post, Entity Comment))]
--     -> Map UserId (User, [(Entity Post, Entity Comment)])
-- associateUsers =
--     associateJoin
-- @
--
-- Next, we'll use the 'Functor' instances for 'Map' and tuple to call
-- 'associateJoin' on the @[(Entity Post, Entity Comment)]@.
--
-- @
-- associatePostsAndComments
--     :: Map UserId (User, [(Entity Post, Entity Comment)])
--     -> Map UserId (User, Map PostId (Post, [Entity Comment]))
-- associatePostsAndComments =
--     fmap (fmap associateJoin)
-- @
--
-- For more reading on this topic, see
-- <https://www.foxhound.systems/blog/grouping-query-results-haskell/ this Foxhound Systems blog post>.
--
-- @since 3.1.2
associateJoin
    :: forall e1 e0.  Ord (Key e0)
    => [(Entity e0, e1)]
    -> Map.Map (Key e0) (e0, [e1])
associateJoin = foldr f start
  where
    start = Map.empty
    f (one, many) =
        Map.insertWith
            (\(oneOld, manyOld) (_, manyNew) -> (oneOld, manyNew ++ manyOld ))
            (entityKey one)
            (entityVal one, [many])
