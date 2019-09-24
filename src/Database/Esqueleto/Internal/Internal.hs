{-# LANGUAGE DeriveDataTypeable
           , EmptyDataDecls
           , FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , MultiParamTypeClasses
           , TypeFamilies
           , UndecidableInstances
           , GADTs
 #-}
{-# LANGUAGE ConstraintKinds
           , EmptyDataDecls
           , FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , GADTs
           , MultiParamTypeClasses
           , OverloadedStrings
           , UndecidableInstances
           , ScopedTypeVariables
           , InstanceSigs
           , Rank2Types
           , CPP
 #-}
-- | This is an internal module, anything exported by this module
-- may change without a major version bump.  Please use only
-- "Database.Esqueleto" if possible.
module Database.Esqueleto.Internal.Internal where

import Control.Arrow ((***), first)
import Control.Exception (Exception, throw, throwIO)
import Control.Monad (ap, MonadPlus(..), void)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (MonadResource, release)
import Data.Acquire (with, allocateAcquire, Acquire)
import Data.Int (Int64)
import Data.List (intersperse)
#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup
#endif
import qualified Data.Monoid as Monoid
import Data.Proxy (Proxy(..))
import Database.Esqueleto.Internal.PersistentImport
import Database.Persist.Sql.Util (entityColumnNames, entityColumnCount, parseEntityValues, isIdField, hasCompositeKey)
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State as S
import qualified Control.Monad.Trans.Writer as W
import qualified Data.ByteString as B
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB

import Data.Typeable (Typeable)
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
  :: ( PersistEntity a
     , BackendCompatible SqlBackend (PersistEntityBackend a) )
  => SqlQuery (SqlExpr (PreprocessedFrom (SqlExpr (Entity a))))
fromStart = x
  where
    x = do
      let ed = entityDef (getVal x)
      ident <- newIdentFor (entityDB ed)
      let ret   = EEntity ident
          from_ = FromStart ident ed
      return (EPreprocessedFrom ret from_)
    getVal :: SqlQuery (SqlExpr (PreprocessedFrom (SqlExpr (Entity a)))) -> Proxy a
    getVal = const Proxy

-- | (Internal) Same as 'fromStart', but entity may be missing.
fromStartMaybe
  :: ( PersistEntity a
     , BackendCompatible SqlBackend (PersistEntityBackend a) )
  => SqlQuery (SqlExpr (PreprocessedFrom (SqlExpr (Maybe (Entity a)))))
fromStartMaybe = maybelize <$> fromStart
  where
    maybelize :: SqlExpr (PreprocessedFrom (SqlExpr (Entity a)))
              -> SqlExpr (PreprocessedFrom (SqlExpr (Maybe (Entity a))))
    maybelize (EPreprocessedFrom ret from_) = EPreprocessedFrom (EMaybe ret) from_

-- | (Internal) Do a @JOIN@.
fromJoin
  :: IsJoinKind join
  => SqlExpr (PreprocessedFrom a)
  -> SqlExpr (PreprocessedFrom b)
  -> SqlQuery (SqlExpr (PreprocessedFrom (join a b)))
fromJoin (EPreprocessedFrom lhsRet lhsFrom)
         (EPreprocessedFrom rhsRet rhsFrom) = Q $ do
  let ret   = smartJoin lhsRet rhsRet
      from_ = FromJoin lhsFrom             -- LHS
                       (reifyJoinKind ret) -- JOIN
                       rhsFrom             -- RHS
                       Nothing             -- ON
  return (EPreprocessedFrom ret from_)

-- | (Internal) Finish a @JOIN@.
fromFinish
  :: SqlExpr (PreprocessedFrom a)
  -> SqlQuery a
fromFinish (EPreprocessedFrom ret from_) = Q $ do
  W.tell mempty { sdFromClause = [from_] }
  return ret

-- | @WHERE@ clause: restrict the query's result.
where_ :: SqlExpr (Value Bool) -> SqlQuery ()
where_ expr = Q $ W.tell mempty { sdWhereClause = Where expr }

-- | @ON@ clause: restrict the a @JOIN@'s result.  The @ON@
-- clause will be applied to the /last/ @JOIN@ that does not
-- have an @ON@ clause yet.  If there are no @JOIN@s without
-- @ON@ clauses (either because you didn't do any @JOIN@, or
-- because all @JOIN@s already have their own @ON@ clauses), a
-- runtime exception 'OnClauseWithoutMatchingJoinException' is
-- thrown.  @ON@ clauses are optional when doing @JOIN@s.
--
-- On the simple case of doing just one @JOIN@, for example
--
-- @
-- select $
-- 'from' $ \\(foo `'InnerJoin`` bar) -> do
--   'on' (foo '^.' FooId '==.' bar '^.' BarFooId)
--   ...
-- @
--
-- there's no ambiguity and the rules above just mean that
-- you're allowed to call 'on' only once (as in SQL).  If you
-- have many joins, then the 'on's are applied on the /reverse/
-- order that the @JOIN@s appear.  For example:
--
-- @
-- select $
-- 'from' $ \\(foo `'InnerJoin`` bar `'InnerJoin`` baz) -> do
--   'on' (baz '^.' BazId '==.' bar '^.' BarBazId)
--   'on' (foo '^.' FooId '==.' bar '^.' BarFooId)
--   ...
-- @
--
-- The order is /reversed/ in order to improve composability.
-- For example, consider @query1@ and @query2@ below:
--
-- @
-- let query1 =
--       'from' $ \\(foo `'InnerJoin`` bar) -> do
--         'on' (foo '^.' FooId '==.' bar '^.' BarFooId)
--     query2 =
--       'from' $ \\(mbaz `'LeftOuterJoin`` quux) -> do
--         return (mbaz '?.' BazName, quux)
--     test1 =      (,) \<$\> query1 \<*\> query2
--     test2 = flip (,) \<$\> query2 \<*\> query1
-- @
--
-- If the order was /not/ reversed, then @test2@ would be
-- broken: @query1@'s 'on' would refer to @query2@'s
-- 'LeftOuterJoin'.
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
groupBy :: (ToSomeValues a) => a -> SqlQuery ()
groupBy expr = Q $ W.tell mempty { sdGroupByClause = GroupBy $ toSomeValues expr }

-- | @ORDER BY@ clause. See also 'asc' and 'desc'.
--
-- Multiple calls to 'orderBy' get concatenated on the final
-- query, including 'distinctOnOrderBy'.
orderBy :: [SqlExpr OrderBy] -> SqlQuery ()
orderBy exprs = Q $ W.tell mempty { sdOrderByClause = exprs }

-- | Ascending order of this field or SqlExpression.
asc :: PersistField a => SqlExpr (Value a) -> SqlExpr OrderBy
asc  = EOrderBy ASC

-- | Descending order of this field or SqlExpression.
desc :: PersistField a => SqlExpr (Value a) -> SqlExpr OrderBy
desc = EOrderBy DESC

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
-- /Since: 2.2.4/
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
-- /Since: 2.2.4/
distinctOn :: [SqlExpr DistinctOn] -> SqlQuery a -> SqlQuery a
distinctOn exprs act = Q (W.tell mempty { sdDistinctClause = DistinctOn exprs }) >> act

-- | Erase an SqlExpression's type so that it's suitable to
-- be used by 'distinctOn'.
--
-- /Since: 2.2.4/
don :: SqlExpr (Value a) -> SqlExpr DistinctOn
don = EDistinctOn

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
-- /Since: 2.2.4/
distinctOnOrderBy :: [SqlExpr OrderBy] -> SqlQuery a -> SqlQuery a
distinctOnOrderBy exprs act =
  distinctOn (toDistinctOn <$> exprs) $ do
    orderBy exprs
    act
  where
    toDistinctOn :: SqlExpr OrderBy -> SqlExpr DistinctOn
    toDistinctOn (EOrderBy _ f) = EDistinctOn f
    toDistinctOn EOrderRandom =
      error "We can't select distinct by a random order!"

-- | @ORDER BY random()@ clause.
--
-- /Since: 1.3.10/
rand :: SqlExpr OrderBy
rand = EOrderRandom

-- | @HAVING@.
--
-- /Since: 1.2.2/
having :: SqlExpr (Value Bool) -> SqlQuery ()
having expr = Q $ W.tell mempty { sdHavingClause = Where expr }

-- | Add a locking clause to the query.  Please read
-- 'LockingKind' documentation and your RDBMS manual.
--
-- If multiple calls to 'locking' are made on the same query,
-- the last one is used.
--
-- /Since: 2.2.7/
locking :: LockingKind -> SqlQuery ()
locking kind = Q $ W.tell mempty { sdLockingClause = Monoid.Last (Just kind) }

-- | Execute a subquery @SELECT@ in an SqlExpression.  Returns a
-- simple value so should be used only when the @SELECT@ query
-- is guaranteed to return just one row.
sub_select :: PersistField a => SqlQuery (SqlExpr (Value a)) -> SqlExpr (Value a)
sub_select         = sub SELECT

-- | Project a field of an entity.
(^.)
  :: forall typ val. (PersistEntity val, PersistField typ)
  => SqlExpr (Entity val)
  -> EntityField val typ
  -> SqlExpr (Value typ)
EEntity ident ^. field
  | isComposite = ECompositeKey $ \info ->  dot info <$> compositeFields pdef
  | otherwise   = ERaw Never    $ \info -> (dot info  $  persistFieldDef field, [])
  where
    isComposite = isIdField field && hasCompositeKey ed
    dot info x  = useIdent info ident <> "." <> fromDBName info (fieldDB x)
    ed          = entityDef $ getEntityVal (Proxy :: Proxy (SqlExpr (Entity val)))
    Just pdef   = entityPrimary ed

-- | Project an SqlExpression that may be null, guarding against null cases.
withNonNull :: PersistField typ
            => SqlExpr (Value (Maybe typ))
            -> (SqlExpr (Value typ) -> SqlQuery a)
            -> SqlQuery a
withNonNull field f = do
  where_ $ not_ $ isNothing field
  f $ veryUnsafeCoerceSqlExprValue field

-- | Project a field of an entity that may be null.
(?.) :: (PersistEntity val, PersistField typ) =>
        SqlExpr (Maybe (Entity val)) -> EntityField val typ -> SqlExpr (Value (Maybe typ))
EMaybe r ?. field = just (r ^. field)

-- | Lift a constant value from Haskell-land to the query.
val  :: PersistField typ => typ -> SqlExpr (Value typ)
val v = ERaw Never $ const ("?", [toPersistValue v])

-- | @IS NULL@ comparison.
isNothing :: PersistField typ => SqlExpr (Value (Maybe typ)) -> SqlExpr (Value Bool)
isNothing (ERaw p f)        = ERaw Parens $ first ((<> " IS NULL") . parensM p) . f
isNothing (ECompositeKey f) = ERaw Parens $ flip (,) [] . (intersperseB " AND " . map (<> " IS NULL")) . f

-- | Analogous to 'Just', promotes a value of type @typ@ into
-- one of type @Maybe typ@.  It should hold that @'val' . Just
-- === just . 'val'@.
just :: SqlExpr (Value typ) -> SqlExpr (Value (Maybe typ))
just (ERaw p f)        = ERaw p f
just (ECompositeKey f) = ECompositeKey f

-- | @NULL@ value.
nothing :: SqlExpr (Value (Maybe typ))
nothing = unsafeSqlValue "NULL"

-- | Join nested 'Maybe's in a 'Value' into one. This is useful when
-- calling aggregate functions on nullable fields.
joinV :: SqlExpr (Value (Maybe (Maybe typ))) -> SqlExpr (Value (Maybe typ))
joinV (ERaw p f)        = ERaw p f
joinV (ECompositeKey f) = ECompositeKey f

-- | @COUNT(*)@ value.
countRows :: Num a => SqlExpr (Value a)
countRows     = unsafeSqlValue "COUNT(*)"

-- | @COUNT@.
count :: Num a => SqlExpr (Value typ) -> SqlExpr (Value a)
count         = countHelper ""           ""

-- | @COUNT(DISTINCT x)@.
--
-- /Since: 2.4.1/
countDistinct :: Num a => SqlExpr (Value typ) -> SqlExpr (Value a)
countDistinct = countHelper "(DISTINCT " ")"

not_ :: SqlExpr (Value Bool) -> SqlExpr (Value Bool)
not_ (ERaw p f) = ERaw Never $ \info -> let (b, vals) = f info
                                        in ("NOT " <> parensM p b, vals)
not_ (ECompositeKey _) = throw (CompositeKeyErr NotError)


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
-- /Since: 2.2.9/
castNum :: (Num a, Num b) => SqlExpr (Value a) -> SqlExpr (Value b)
castNum  = veryUnsafeCoerceSqlExprValue

-- | Same as 'castNum', but for nullable values.
--
-- /Since: 2.2.9/
castNumM :: (Num a, Num b) => SqlExpr (Value (Maybe a)) -> SqlExpr (Value (Maybe b))
castNumM = veryUnsafeCoerceSqlExprValue

-- | @COALESCE@ function. Evaluates the arguments in order and
-- returns the value of the first non-NULL SqlExpression, or NULL
-- (Nothing) otherwise. Some RDBMSs (such as SQLite) require
-- at least two arguments; please refer to the appropriate
-- documentation.
--
-- /Since: 1.4.3/
coalesce :: PersistField a => [SqlExpr (Value (Maybe a))] -> SqlExpr (Value (Maybe a))
coalesce              = unsafeSqlFunctionParens "COALESCE"

-- | Like @coalesce@, but takes a non-nullable SqlExpression
-- placed at the end of the SqlExpression list, which guarantees
-- a non-NULL result.
--
-- /Since: 1.4.3/
coalesceDefault :: PersistField a => [SqlExpr (Value (Maybe a))] -> SqlExpr (Value a) -> SqlExpr (Value a)
coalesceDefault exprs = unsafeSqlFunctionParens "COALESCE" . (exprs ++) . return . just

-- | @LOWER@ function.
lower_ :: SqlString s => SqlExpr (Value s) -> SqlExpr (Value s)
lower_  = unsafeSqlFunction "LOWER"

-- | @LIKE@ operator.
like :: SqlString s => SqlExpr (Value s) -> SqlExpr (Value s) -> SqlExpr (Value Bool)
like    = unsafeSqlBinOp    " LIKE "

-- | @ILIKE@ operator (case-insensitive @LIKE@).
--
-- Supported by PostgreSQL only.
--
-- /Since: 2.2.3/
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
subList_select         = EList . sub_select

-- | Lift a list of constant value from Haskell-land to the query.
valList :: PersistField typ => [typ] -> SqlExpr (ValueList typ)
valList []   = EEmptyList
valList vals = EList $ ERaw Parens $ const ( uncommas ("?" <$ vals)
                                           , map toPersistValue vals )

-- | Same as 'just' but for 'ValueList'.  Most of the time you
-- won't need it, though, because you can use 'just' from
-- inside 'subList_select' or 'Just' from inside 'valList'.
--
-- /Since: 2.2.12/
justList :: SqlExpr (ValueList typ) -> SqlExpr (ValueList (Maybe typ))
justList EEmptyList = EEmptyList
justList (EList v)  = EList (just v)

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
v `in_`   e = ifNotEmptyList e False $ unsafeSqlBinOp     " IN " v (veryUnsafeCoerceSqlExprValueList e)

-- | @NOT IN@ operator.
notIn :: PersistField typ => SqlExpr (Value typ) -> SqlExpr (ValueList typ) -> SqlExpr (Value Bool)
v `notIn` e = ifNotEmptyList e True  $ unsafeSqlBinOp " NOT IN " v (veryUnsafeCoerceSqlExprValueList e)

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
exists    = unsafeSqlFunction     "EXISTS " . existsHelper

-- | @NOT EXISTS@ operator.
notExists :: SqlQuery () -> SqlExpr (Value Bool)
notExists = unsafeSqlFunction "NOT EXISTS " . existsHelper

-- | @SET@ clause used on @UPDATE@s.  Note that while it's not
-- a type error to use this function on a @SELECT@, it will
-- most certainly result in a runtime error.
set :: PersistEntity val => SqlExpr (Entity val) -> [SqlExpr (Update val)] -> SqlQuery ()
set ent upds = Q $ W.tell mempty { sdSetClause = map apply upds }
  where
    apply (ESet f) = SetClause (f ent)

(=.)  :: (PersistEntity val, PersistField typ) => EntityField val typ -> SqlExpr (Value typ) -> SqlExpr (Update val)
field  =. expr = setAux field (const expr)

(+=.) :: (PersistEntity val, PersistField a) => EntityField val a -> SqlExpr (Value a) -> SqlExpr (Update val)
field +=. expr = setAux field (\ent -> ent ^. field +. expr)

(-=.) :: (PersistEntity val, PersistField a) => EntityField val a -> SqlExpr (Value a) -> SqlExpr (Update val)
field -=. expr = setAux field (\ent -> ent ^. field -. expr)

(*=.) :: (PersistEntity val, PersistField a) => EntityField val a -> SqlExpr (Value a) -> SqlExpr (Update val)
field *=. expr = setAux field (\ent -> ent ^. field *. expr)

(/=.) :: (PersistEntity val, PersistField a) => EntityField val a -> SqlExpr (Value a) -> SqlExpr (Update val)
field /=. expr = setAux field (\ent -> ent ^. field /. expr)

-- | Apply a 'PersistField' constructor to @SqlExpr Value@ arguments.
(<#) :: (a -> b) -> SqlExpr (Value a) -> SqlExpr (Insertion b)
(<#) _ (ERaw _ f)        = EInsert Proxy f
(<#) _ (ECompositeKey _) = throw (CompositeKeyErr ToInsertionError)


-- | Apply extra @SqlExpr Value@ arguments to a 'PersistField' constructor
(<&>) :: SqlExpr (Insertion (a -> b)) -> SqlExpr (Value a) -> SqlExpr (Insertion b)
(EInsert _ f) <&> (ERaw _ g) = EInsert Proxy $ \x ->
  let (fb, fv) = f x
      (gb, gv) = g x
  in (fb <> ", " <> gb, fv ++ gv)
(EInsert _ _) <&> (ECompositeKey _) = throw (CompositeKeyErr CombineInsertionError)

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
-- /Since: 2.1.2/
case_ :: PersistField a => [(SqlExpr (Value Bool), SqlExpr (Value a))] -> SqlExpr (Value a) -> SqlExpr (Value a)
case_ = unsafeSqlCase

-- | Convert an entity's key into another entity's.
--
-- This function is to be used when you change an entity's @Id@ to be
-- that of another entity.  For example:
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
-- For this example, declare:
--
-- @
-- instance ToBaseId Foo where
--   type BaseEnt Foo = Bar
--   toBaseIdWitness = FooKey
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
-- /Since: 2.4.3/
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
-- /Since: 2.1.2/
when_ :: expr (Value Bool) -> () -> expr a -> (expr (Value Bool), expr a)
when_ cond _ expr = (cond, expr)

-- | Syntax sugar for 'case_'.
--
-- /Since: 2.1.2/
then_ :: ()
then_ = ()

-- | Syntax sugar for 'case_'.
--
-- /Since: 2.1.2/
else_ :: expr a -> expr a
else_ = id

-- | A single value (as opposed to a whole entity).  You may use
-- @('^.')@ or @('?.')@ to get a 'Value' from an 'Entity'.
newtype Value a = Value { unValue :: a } deriving (Eq, Ord, Show, Typeable)


-- | /Since: 1.4.4/
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

instance ( ToSomeValues a
         , ToSomeValues b
         ) => ToSomeValues (a, b) where
  toSomeValues (a,b) = toSomeValues a ++ toSomeValues b

instance ( ToSomeValues a
         , ToSomeValues b
         , ToSomeValues c
         ) => ToSomeValues (a, b, c) where
  toSomeValues (a,b,c) = toSomeValues a ++ toSomeValues b ++ toSomeValues c

instance ( ToSomeValues a
         , ToSomeValues b
         , ToSomeValues c
         , ToSomeValues d
         ) => ToSomeValues (a, b, c, d) where
  toSomeValues (a,b,c,d) = toSomeValues a ++ toSomeValues b ++ toSomeValues c ++
    toSomeValues d

instance ( ToSomeValues a
         , ToSomeValues b
         , ToSomeValues c
         , ToSomeValues d
         , ToSomeValues e
         ) => ToSomeValues (a, b, c, d, e) where
  toSomeValues (a,b,c,d,e) = toSomeValues a ++ toSomeValues b ++
    toSomeValues c ++ toSomeValues d ++ toSomeValues e

instance ( ToSomeValues a
         , ToSomeValues b
         , ToSomeValues c
         , ToSomeValues d
         , ToSomeValues e
         , ToSomeValues f
         ) => ToSomeValues (a, b, c, d, e, f) where
  toSomeValues (a,b,c,d,e,f) = toSomeValues a ++ toSomeValues b ++
    toSomeValues c ++ toSomeValues d ++ toSomeValues e ++ toSomeValues f

instance ( ToSomeValues a
         , ToSomeValues b
         , ToSomeValues c
         , ToSomeValues d
         , ToSomeValues e
         , ToSomeValues f
         , ToSomeValues g
         ) => ToSomeValues (a, b, c, d, e, f, g) where
  toSomeValues (a,b,c,d,e,f,g) = toSomeValues a ++ toSomeValues b ++
    toSomeValues c ++ toSomeValues d ++ toSomeValues e ++ toSomeValues f ++
    toSomeValues g

instance ( ToSomeValues a
         , ToSomeValues b
         , ToSomeValues c
         , ToSomeValues d
         , ToSomeValues e
         , ToSomeValues f
         , ToSomeValues g
         , ToSomeValues h
         ) => ToSomeValues (a, b, c, d, e, f, g, h) where
  toSomeValues (a,b,c,d,e,f,g,h) = toSomeValues a ++ toSomeValues b ++
    toSomeValues c ++ toSomeValues d ++ toSomeValues e ++ toSomeValues f ++
    toSomeValues g ++ toSomeValues h


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
data JoinKind =
    InnerJoinKind      -- ^ @INNER JOIN@
  | CrossJoinKind      -- ^ @CROSS JOIN@
  | LeftOuterJoinKind  -- ^ @LEFT OUTER JOIN@
  | RightOuterJoinKind -- ^ @RIGHT OUTER JOIN@
  | FullOuterJoinKind  -- ^ @FULL OUTER JOIN@
    deriving Eq


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
instance Exception OnClauseWithoutMatchingJoinException where


-- | (Internal) Phantom type used to process 'from' (see 'fromStart').
data PreprocessedFrom a


-- | Phantom type used by 'orderBy', 'asc' and 'desc'.
data OrderBy


-- | Phantom type used by 'distinctOn' and 'don'.
data DistinctOn


-- | Phantom type for a @SET@ operation on an entity of the given
-- type (see 'set' and '(=.)').
data Update typ


-- | Phantom type used by 'insertSelect'.
data Insertion a


-- | Different kinds of locking clauses supported by 'locking'.
--
-- Note that each RDBMS has different locking support.  The
-- constructors of this datatype specify only the /syntax/ of the
-- locking mechanism, not its /semantics/.  For example, even
-- though both MySQL and PostgreSQL support 'ForUpdate', there
-- are no guarantees that they will behave the same.
--
-- /Since: 2.2.7/
data LockingKind =
    ForUpdate
    -- ^ @FOR UPDATE@ syntax.  Supported by MySQL, Oracle and
    -- PostgreSQL.
    --
    -- /Since: 2.2.7/
  | ForUpdateSkipLocked
    -- ^ @FOR UPDATE SKIP LOCKED@ syntax.  Supported by MySQL, Oracle and
    -- PostgreSQL.
    --
    -- /Since: 2.2.7/
  | ForShare
    -- ^ @FOR SHARE@ syntax.  Supported by PostgreSQL.
    --
    -- /Since: 2.2.7/
  | LockInShareMode
    -- ^ @LOCK IN SHARE MODE@ syntax.  Supported by MySQL.
    --
    -- /Since: 2.2.7/


-- | Phantom class of data types that are treated as strings by the
-- RDBMS.  It has no methods because it's only used to avoid type
-- errors such as trying to concatenate integers.
--
-- If you have a custom data type or @newtype@, feel free to make
-- it an instance of this class.
--
-- /Since: 2.4.0/
class PersistField a => SqlString a where

-- | /Since: 2.3.0/
instance (a ~ Char) => SqlString [a] where

-- | /Since: 2.3.0/
instance SqlString T.Text where

-- | /Since: 2.3.0/
instance SqlString TL.Text where

-- | /Since: 2.3.0/
instance SqlString B.ByteString where

-- | /Since: 2.3.0/
instance SqlString Html where

-- | /Since: 2.4.0/
instance SqlString a => SqlString (Maybe a) where

-- | Class that enables one to use 'toBaseId' to convert an entity's
-- key on a query into another (cf. 'toBaseId').
class ToBaseId ent where
  type BaseEnt ent :: *
  toBaseIdWitness :: Key (BaseEnt ent) -> Key ent


-- | @FROM@ clause: bring entities into scope.
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

instance ( FromPreprocess (SqlExpr (Entity val))
         ) => From (SqlExpr (Entity val)) where
  from_ = fromPreprocess >>= fromFinish

instance ( FromPreprocess (SqlExpr (Maybe (Entity val)))
         ) => From (SqlExpr (Maybe (Entity val))) where
  from_ = fromPreprocess >>= fromFinish

instance ( FromPreprocess (InnerJoin a b)
         ) => From (InnerJoin a b) where
  from_ = fromPreprocess >>= fromFinish

instance ( FromPreprocess (CrossJoin a b)
         ) => From (CrossJoin a b) where
  from_ = fromPreprocess >>= fromFinish

instance ( FromPreprocess (LeftOuterJoin a b)
         ) => From (LeftOuterJoin a b) where
  from_ = fromPreprocess >>= fromFinish

instance ( FromPreprocess (RightOuterJoin a b)
         ) => From (RightOuterJoin a b) where
  from_ = fromPreprocess >>= fromFinish

instance ( FromPreprocess (FullOuterJoin a b)
         ) => From (FullOuterJoin a b) where
  from_ = fromPreprocess >>= fromFinish

instance ( From a
         , From b
         ) => From (a, b) where
  from_ = (,) <$> from_ <*> from_

instance ( From a
         , From b
         , From c
         ) => From (a, b, c) where
  from_ = (,,) <$> from_ <*> from_ <*> from_

instance ( From a
         , From b
         , From c
         , From d
         ) => From (a, b, c, d) where
  from_ = (,,,) <$> from_ <*> from_ <*> from_ <*> from_

instance ( From a
         , From b
         , From c
         , From d
         , From e
         ) => From (a, b, c, d, e) where
  from_ = (,,,,) <$> from_ <*> from_ <*> from_ <*> from_ <*> from_

instance ( From a
         , From b
         , From c
         , From d
         , From e
         , From f
         ) => From (a, b, c, d, e, f) where
  from_ = (,,,,,) <$> from_ <*> from_ <*> from_ <*> from_ <*> from_ <*> from_

instance ( From a
         , From b
         , From c
         , From d
         , From e
         , From f
         , From g
         ) => From (a, b, c, d, e, f, g) where
  from_ = (,,,,,,) <$> from_ <*> from_ <*> from_ <*> from_ <*> from_ <*> from_ <*> from_

instance ( From a
         , From b
         , From c
         , From d
         , From e
         , From f
         , From g
         , From h
         ) => From (a, b, c, d, e, f, g, h) where
  from_ = (,,,,,,,) <$> from_ <*> from_ <*> from_ <*> from_ <*> from_ <*> from_ <*> from_ <*> from_



-- | (Internal) Class that implements the @JOIN@ 'from' magic
-- (see 'fromStart').
class FromPreprocess a where
  fromPreprocess :: SqlQuery (SqlExpr (PreprocessedFrom a))

instance ( PersistEntity val
         , BackendCompatible SqlBackend (PersistEntityBackend val)
         ) => FromPreprocess (SqlExpr (Entity val)) where
  fromPreprocess = fromStart

instance ( PersistEntity val
         , BackendCompatible SqlBackend (PersistEntityBackend val)
         ) => FromPreprocess (SqlExpr (Maybe (Entity val))) where
  fromPreprocess = fromStartMaybe

instance ( FromPreprocess a
         , FromPreprocess b
         , IsJoinKind join
         ) => FromPreprocess (join a b) where
  fromPreprocess = do
    a <- fromPreprocess
    b <- fromPreprocess
    fromJoin a b

-- | Exception data type for @esqueleto@ internal errors
data EsqueletoError =
    CompositeKeyErr CompositeKeyError
  | UnexpectedCaseErr UnexpectedCaseError
  | SqlBinOpCompositeErr SqlBinOpCompositeError
  deriving (Show)

instance Exception EsqueletoError

data CompositeKeyError =
    NotError
  | ToInsertionError
  | CombineInsertionError
  | FoldHelpError
  | SqlCaseError
  | SqlCastAsError
  | MakeOnClauseError
  | MakeExcError
  | MakeSetError
  | MakeWhereError
  | MakeHavingError
  deriving (Show)

data UnexpectedCaseError =
    EmptySqlExprValueList
  | MakeFromError
  | UnsupportedSqlInsertIntoType
  | InsertionFinalError
  | NewIdentForError
  | UnsafeSqlCaseError
  deriving (Show)

data SqlBinOpCompositeError =
    MismatchingLengthsError
  | NullPlaceholdersError
  | DeconstructionError
  deriving (Show)



-- | SQL backend for @esqueleto@ using 'SqlPersistT'.
newtype SqlQuery a =
  Q { unQ :: W.WriterT SideData (S.State IdentState) a }

instance Functor SqlQuery where
  fmap f = Q . fmap f . unQ

instance Monad SqlQuery where
  return  = Q . return
  m >>= f = Q (unQ m >>= unQ . f)

instance Applicative SqlQuery where
  pure  = return
  (<*>) = ap


-- | Constraint synonym for @persistent@ entities whose backend
-- is 'SqlPersistT'.
type SqlEntity ent = (PersistEntity ent, PersistEntityBackend ent ~ SqlBackend)


----------------------------------------------------------------------


-- | Side data written by 'SqlQuery'.
data SideData = SideData { sdDistinctClause :: !DistinctClause
                         , sdFromClause     :: ![FromClause]
                         , sdSetClause      :: ![SetClause]
                         , sdWhereClause    :: !WhereClause
                         , sdGroupByClause  :: !GroupByClause
                         , sdHavingClause   :: !HavingClause
                         , sdOrderByClause  :: ![OrderByClause]
                         , sdLimitClause    :: !LimitClause
                         , sdLockingClause  :: !LockingClause
                         }

instance Semigroup SideData where
  SideData d f s w g h o l k <> SideData d' f' s' w' g' h' o' l' k' =
    SideData (d <> d') (f <> f') (s <> s') (w <> w') (g <> g') (h <> h') (o <> o') (l <> l') (k <> k')

instance Monoid SideData where
  mempty = SideData mempty mempty mempty mempty mempty mempty mempty mempty mempty
  mappend = (<>)

-- | The @DISTINCT@ "clause".
data DistinctClause =
    DistinctAll                     -- ^ The default, everything.
  | DistinctStandard                -- ^ Only @DISTINCT@, SQL standard.
  | DistinctOn [SqlExpr DistinctOn] -- ^ @DISTINCT ON@, PostgreSQL extension.

instance Semigroup DistinctClause where
  DistinctOn a     <> DistinctOn b = DistinctOn (a <> b)
  DistinctOn a     <> _            = DistinctOn a
  DistinctStandard <> _            = DistinctStandard
  DistinctAll      <> b            = b

instance Monoid DistinctClause where
  mempty = DistinctAll
  mappend = (<>)

-- | A part of a @FROM@ clause.
data FromClause =
    FromStart Ident EntityDef
  | FromJoin FromClause JoinKind FromClause (Maybe (SqlExpr (Value Bool)))
  | OnClause (SqlExpr (Value Bool))


-- | A part of a @SET@ clause.
newtype SetClause = SetClause (SqlExpr (Value ()))


-- | Collect 'OnClause's on 'FromJoin's.  Returns the first
-- unmatched 'OnClause's data on error.  Returns a list without
-- 'OnClauses' on success.
collectOnClauses :: [FromClause] -> Either (SqlExpr (Value Bool)) [FromClause]
collectOnClauses = go []
  where
    go []  (f@(FromStart _ _):fs) = fmap (f:) (go [] fs) -- fast path
    go acc (OnClause expr    :fs) = findMatching acc expr >>= flip go fs
    go acc (f:fs)                 = go (f:acc) fs
    go acc []                     = return $ reverse acc

    findMatching (f : acc) expr =
      case tryMatch expr f of
        Just f' -> return (f' : acc)
        Nothing -> (f:) <$> findMatching acc expr
    findMatching [] expr = Left expr

    tryMatch expr (FromJoin l k r onClause) =
      matchR `mplus` matchC `mplus` matchL -- right to left
        where
          matchR = (\r' -> FromJoin l k r' onClause) <$> tryMatch expr r
          matchL = (\l' -> FromJoin l' k r onClause) <$> tryMatch expr l
          matchC = case onClause of
                     Nothing | k /= CrossJoinKind
                               -> return (FromJoin l k r (Just expr))
                             | otherwise -> mzero
                     Just _  -> mzero
    tryMatch _ _ = mzero


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

instance Semigroup LimitClause where
  Limit l1 o1 <> Limit l2 o2 =
    Limit (l2 `mplus` l1) (o2 `mplus` o1)
    -- More than one 'limit' or 'offset' is issued, we want to
    -- keep the latest one.  That's why we use mplus with
    -- "reversed" arguments.

instance Monoid LimitClause where
  mempty = Limit mzero mzero
  mappend = (<>)

-- | A locking clause.
type LockingClause = Monoid.Last LockingKind


----------------------------------------------------------------------


-- | Identifier used for table names.
newtype Ident = I T.Text


-- | List of identifiers already in use and supply of temporary
-- identifiers.
newtype IdentState = IdentState { inUse :: HS.HashSet T.Text }

initialIdentState :: IdentState
initialIdentState = IdentState mempty


-- | Create a fresh 'Ident'.  If possible, use the given
-- 'DBName'.
newIdentFor :: DBName -> SqlQuery Ident
newIdentFor = Q . lift . try . unDBName
  where
    try orig = do
      s <- S.get
      let go (t:ts) | t `HS.member` inUse s = go ts
                    | otherwise             = use t
          go [] = throw (UnexpectedCaseErr NewIdentForError)
      go (possibilities orig)

    possibilities t = t : map addNum [2..]
      where
        addNum :: Int -> T.Text
        addNum = T.append t . T.pack . show

    use t = do
      S.modify (\s -> s { inUse = HS.insert t (inUse s) })
      return (I t)


-- | Information needed to escape and use identifiers.
type IdentInfo = (SqlBackend, IdentState)


-- | Use an identifier.
useIdent :: IdentInfo -> Ident -> TLB.Builder
useIdent info (I ident) = fromDBName info $ DBName ident




-- | An expression on the SQL backend.
--
-- There are many comments describing the constructors of this
-- data type.  However, Haddock doesn't like GADTs, so you'll have to read them by hitting \"Source\".
data SqlExpr a where
  -- An entity, created by 'from' (cf. 'fromStart').
  EEntity  :: Ident -> SqlExpr (Entity val)

  -- Just a tag stating that something is nullable.
  EMaybe   :: SqlExpr a -> SqlExpr (Maybe a)

  -- Raw expression: states whether parenthesis are needed
  -- around this expression, and takes information about the SQL
  -- connection (mainly for escaping names) and returns both an
  -- string ('TLB.Builder') and a list of values to be
  -- interpolated by the SQL backend.
  ERaw     :: NeedParens -> (IdentInfo -> (TLB.Builder, [PersistValue])) -> SqlExpr (Value a)

  -- A composite key.
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
  ECompositeKey :: (IdentInfo -> [TLB.Builder]) -> SqlExpr (Value a)

  -- 'EList' and 'EEmptyList' are used by list operators.
  EList      :: SqlExpr (Value a) -> SqlExpr (ValueList a)
  EEmptyList :: SqlExpr (ValueList a)

  -- A 'SqlExpr' accepted only by 'orderBy'.
  EOrderBy :: OrderByType -> SqlExpr (Value a) -> SqlExpr OrderBy
  EOrderRandom :: SqlExpr OrderBy

  -- A 'SqlExpr' accepted only by 'distinctOn'.
  EDistinctOn :: SqlExpr (Value a) -> SqlExpr DistinctOn

  -- A 'SqlExpr' accepted only by 'set'.
  ESet :: (SqlExpr (Entity val) -> SqlExpr (Value ())) -> SqlExpr (Update val)

  -- An internal 'SqlExpr' used by the 'from' hack.
  EPreprocessedFrom :: a -> FromClause -> SqlExpr (PreprocessedFrom a)

  -- Used by 'insertSelect'.
  EInsert  :: Proxy a -> (IdentInfo -> (TLB.Builder, [PersistValue])) -> SqlExpr (Insertion a)
  EInsertFinal :: PersistEntity a => SqlExpr (Insertion a) -> SqlExpr InsertFinal

-- | Phantom type used to mark a @INSERT INTO@ query.
data InsertFinal

data NeedParens = Parens | Never

parensM :: NeedParens -> TLB.Builder -> TLB.Builder
parensM Never  = id
parensM Parens = parens

data OrderByType = ASC | DESC


instance ToSomeValues (SqlExpr (Value a)) where
  toSomeValues a = [SomeValue a]

fieldName :: (PersistEntity val, PersistField typ)
          => IdentInfo -> EntityField val typ -> TLB.Builder
fieldName info = fromDBName info . fieldDB . persistFieldDef

-- FIXME: Composite/non-id pKS not supported on set
setAux :: (PersistEntity val, PersistField typ)
       => EntityField val typ
       -> (SqlExpr (Entity val) -> SqlExpr (Value typ))
       -> SqlExpr (Update val)
setAux field mkVal = ESet $ \ent -> unsafeSqlBinOp " = " name (mkVal ent)
  where name = ERaw Never $ \info -> (fieldName info field, mempty)

sub :: PersistField a => Mode -> SqlQuery (SqlExpr (Value a)) -> SqlExpr (Value a)
sub mode query = ERaw Parens $ \info -> toRawSql mode info query

fromDBName :: IdentInfo -> DBName -> TLB.Builder
fromDBName (conn, _) = TLB.fromText . connEscapeName conn

existsHelper :: SqlQuery () -> SqlExpr (Value Bool)
existsHelper = sub SELECT . (>> return true)
  where
    true :: SqlExpr (Value Bool)
    true = val True

ifNotEmptyList :: SqlExpr (ValueList a) -> Bool -> SqlExpr (Value Bool) -> SqlExpr (Value Bool)
ifNotEmptyList EEmptyList b _ = val b
ifNotEmptyList (EList _)  _ x = x

countHelper :: Num a => TLB.Builder -> TLB.Builder -> SqlExpr (Value typ) -> SqlExpr (Value a)
countHelper open close (ERaw _ f) = ERaw Never $ first (\b -> "COUNT" <> open <> parens b <> close) . f
countHelper _ _ (ECompositeKey _) = countRows -- Assumes no NULLs on a PK


----------------------------------------------------------------------


-- | (Internal) Create a case statement.
--
-- Since: 2.1.1
unsafeSqlCase :: PersistField a => [(SqlExpr (Value Bool), SqlExpr (Value a))] -> SqlExpr (Value a) -> SqlExpr (Value a)
unsafeSqlCase when (ERaw p1 f1) = ERaw Never buildCase
  where
    buildCase :: IdentInfo -> (TLB.Builder, [PersistValue])
    buildCase info =
        let (b1, vals1) = f1 info
            (b2, vals2) = mapWhen when info
        in ( "CASE" <> b2 <> " ELSE " <> parensM p1 b1 <> " END", vals2 <> vals1)

    mapWhen :: [(SqlExpr (Value Bool), SqlExpr (Value a))] -> IdentInfo -> (TLB.Builder, [PersistValue])
    mapWhen []    _    = throw (UnexpectedCaseErr UnsafeSqlCaseError)
    mapWhen when' info = foldl (foldHelp info) (mempty, mempty) when'

    foldHelp :: IdentInfo -> (TLB.Builder, [PersistValue]) -> (SqlExpr (Value Bool), SqlExpr (Value a)) -> (TLB.Builder, [PersistValue])
    foldHelp info (b0, vals0) (ERaw p1' f1', ERaw p2 f2) =
        let (b1, vals1) = f1' info
            (b2, vals2) = f2 info
        in ( b0 <> " WHEN " <> parensM p1' b1 <> " THEN " <> parensM p2 b2, vals0 <> vals1 <> vals2 )
    foldHelp _ _ _ = throw (CompositeKeyErr FoldHelpError)
unsafeSqlCase _ (ECompositeKey _) = throw (CompositeKeyErr SqlCaseError)


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
unsafeSqlBinOp op (ERaw p1 f1) (ERaw p2 f2) = ERaw Parens f
  where
    f info = let (b1, vals1) = f1 info
                 (b2, vals2) = f2 info
             in ( parensM p1 b1 <> op <> parensM p2 b2
                , vals1 <> vals2 )
unsafeSqlBinOp op a b = unsafeSqlBinOp op (construct a) (construct b)
    where construct :: SqlExpr (Value a) -> SqlExpr (Value a)
          construct (ERaw p f)        = ERaw Parens $ \info ->
            let (b1, vals) = f info
                build ("?", [PersistList vals']) =
                  (uncommas $ replicate (length vals') "?", vals')
                build expr = expr
             in  build (parensM p b1, vals)
          construct (ECompositeKey f) =
            ERaw Parens $ \info -> (uncommas $ f info, mempty)
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
unsafeSqlBinOpComposite op _ a@(ERaw _ _) b@(ERaw _ _) = unsafeSqlBinOp op a b
unsafeSqlBinOpComposite op sep a b = ERaw Parens $ compose (listify a) (listify b)
  where
    listify :: SqlExpr (Value x) -> IdentInfo -> ([TLB.Builder], [PersistValue])
    listify (ECompositeKey f) = flip (,) [] . f
    listify (ERaw _ f)        = deconstruct . f

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
unsafeSqlValue v = ERaw Never $ const (v, mempty)
{-# INLINE unsafeSqlValue #-}


-- | (Internal) A raw SQL function.  Once again, the same warning
-- from 'unsafeSqlBinOp' applies to this function as well.
unsafeSqlFunction :: UnsafeSqlFunctionArgument a =>
                     TLB.Builder -> a -> SqlExpr (Value b)
unsafeSqlFunction name arg =
  ERaw Never $ \info ->
    let (argsTLB, argsVals) =
          uncommas' $ map (\(ERaw _ f) -> f info) $ toArgList arg
    in (name <> parens argsTLB, argsVals)

-- | (Internal) An unsafe SQL function to extract a subfield from a compound
-- field, e.g. datetime. See 'unsafeSqlBinOp' for warnings.
--
-- Since: 1.3.6.
unsafeSqlExtractSubField :: UnsafeSqlFunctionArgument a =>
                     TLB.Builder -> a -> SqlExpr (Value b)
unsafeSqlExtractSubField subField arg =
  ERaw Never $ \info ->
    let (argsTLB, argsVals) =
          uncommas' $ map (\(ERaw _ f) -> f info) $ toArgList arg
    in ("EXTRACT" <> parens (subField <> " FROM " <> argsTLB), argsVals)

-- | (Internal) A raw SQL function. Preserves parentheses around arguments.
-- See 'unsafeSqlBinOp' for warnings.
unsafeSqlFunctionParens :: UnsafeSqlFunctionArgument a =>
                           TLB.Builder -> a -> SqlExpr (Value b)
unsafeSqlFunctionParens name arg =
  ERaw Never $ \info ->
    let (argsTLB, argsVals) =
          uncommas' $ map (\(ERaw p f) -> first (parensM p) (f info)) $ toArgList arg
    in (name <> parens argsTLB, argsVals)

-- | (Internal) An explicit SQL type cast using CAST(value as type).
-- See 'unsafeSqlBinOp' for warnings.
unsafeSqlCastAs :: T.Text -> SqlExpr (Value a) -> SqlExpr (Value b)
unsafeSqlCastAs t (ERaw p f) =
  ERaw Never $ \info ->
    let (b, v) = f info
    in ("CAST" <> parens ( parensM p b <> " AS " <> TLB.fromText t), v )
unsafeSqlCastAs _ (ECompositeKey _) = throw (CompositeKeyErr SqlCastAsError)

class UnsafeSqlFunctionArgument a where
  toArgList :: a -> [SqlExpr (Value ())]
instance (a ~ Value b) => UnsafeSqlFunctionArgument (SqlExpr a) where
  toArgList = (:[]) . veryUnsafeCoerceSqlExprValue
instance UnsafeSqlFunctionArgument a =>
         UnsafeSqlFunctionArgument [a] where
  toArgList = concatMap toArgList
instance ( UnsafeSqlFunctionArgument a
         , UnsafeSqlFunctionArgument b
         ) => UnsafeSqlFunctionArgument (a, b) where
  toArgList (a, b) = toArgList a ++ toArgList b
instance ( UnsafeSqlFunctionArgument a
         , UnsafeSqlFunctionArgument b
         , UnsafeSqlFunctionArgument c
         ) => UnsafeSqlFunctionArgument (a, b, c) where
  toArgList = toArgList . from3
instance ( UnsafeSqlFunctionArgument a
         , UnsafeSqlFunctionArgument b
         , UnsafeSqlFunctionArgument c
         , UnsafeSqlFunctionArgument d
         ) => UnsafeSqlFunctionArgument (a, b, c, d) where
  toArgList = toArgList . from4



-- | (Internal) Coerce a value's type from 'SqlExpr (Value a)' to
-- 'SqlExpr (Value b)'.  You should /not/ use this function
-- unless you know what you're doing!
veryUnsafeCoerceSqlExprValue :: SqlExpr (Value a) -> SqlExpr (Value b)
veryUnsafeCoerceSqlExprValue (ERaw p f)        = ERaw p f
veryUnsafeCoerceSqlExprValue (ECompositeKey f) = ECompositeKey f


-- | (Internal) Coerce a value's type from 'SqlExpr (ValueList
-- a)' to 'SqlExpr (Value a)'.  Does not work with empty lists.
veryUnsafeCoerceSqlExprValueList :: SqlExpr (ValueList a) -> SqlExpr (Value a)
veryUnsafeCoerceSqlExprValueList (EList v)  = v
veryUnsafeCoerceSqlExprValueList EEmptyList = throw (UnexpectedCaseErr EmptySqlExprValueList)


----------------------------------------------------------------------

-- | (Internal) Execute an @esqueleto@ @SELECT@ 'SqlQuery' inside
-- @persistent@'s 'SqlPersistT' monad.
rawSelectSource :: ( SqlSelect a r
                   , MonadIO m1
                   , MonadIO m2
                   )
                 => Mode
                 -> SqlQuery a
                 -> SqlReadT m1 (Acquire (C.ConduitT () r m2 ()))
rawSelectSource mode query =
      do
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
        case process <$> mrow of
          Just (Right r)  -> C.yield r >> massage
          Just (Left err) -> liftIO $ throwIO $ PersistMarshalError err
          Nothing         -> return ()

      process = sqlSelectProcessRow


-- | Execute an @esqueleto@ @SELECT@ query inside @persistent@'s
-- 'SqlPersistT' monad and return a 'C.Source' of rows.
selectSource :: ( SqlSelect a r
               , BackendCompatible SqlBackend backend
               , IsPersistBackend backend
               , PersistQueryRead backend
               , PersistStoreRead backend, PersistUniqueRead backend
               , MonadResource m )
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
--  'sub_select').
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
select :: ( SqlSelect a r
          , MonadIO m
          )
       => SqlQuery a -> SqlReadT m [r]
select query = do
    res <- rawSelectSource SELECT query
    conn <- R.ask
    liftIO $ with res $ flip R.runReaderT conn . runSource

-- | (Internal) Run a 'C.Source' of rows.
runSource :: Monad m =>
             C.ConduitT () r (R.ReaderT backend m) ()
          -> R.ReaderT backend m [r]
runSource src = C.runConduit $ src C..| CL.consume


----------------------------------------------------------------------


-- | (Internal) Execute an @esqueleto@ statement inside
-- @persistent@'s 'SqlPersistT' monad.
rawEsqueleto :: ( MonadIO m, SqlSelect a r, BackendCompatible SqlBackend backend)
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
delete :: ( MonadIO m )
       => SqlQuery ()
       -> SqlWriteT m ()
delete = void . deleteCount

-- | Same as 'delete', but returns the number of rows affected.
deleteCount :: ( MonadIO m )
            => SqlQuery ()
            -> SqlWriteT m Int64
deleteCount = rawEsqueleto DELETE


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
  )
  => (SqlExpr (Entity val) -> SqlQuery ())
  -> SqlWriteT m ()
update = void . updateCount

-- | Same as 'update', but returns the number of rows affected.
updateCount
  ::
  ( MonadIO m, PersistEntity val
  , BackendCompatible SqlBackend (PersistEntityBackend val)
  )
  => (SqlExpr (Entity val) -> SqlQuery ())
  -> SqlWriteT m Int64
updateCount = rawEsqueleto UPDATE . from


----------------------------------------------------------------------


builderToText :: TLB.Builder -> T.Text
builderToText = TL.toStrict . TLB.toLazyTextWith defaultChunkSize
  where
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
      SideData distinctClause
               fromClauses
               setClauses
               whereClauses
               groupByClause
               havingClause
               orderByClauses
               limitClause
               lockingClause = sd
      -- Pass the finalIdentState (containing all identifiers
      -- that were used) to the subsequent calls.  This ensures
      -- that no name clashes will occur on subqueries that may
      -- appear on the expressions below.
      info = (projectBackend conn, finalIdentState)
  in mconcat
      [ makeInsertInto info mode ret
      , makeSelect     info mode distinctClause ret
      , makeFrom       info mode fromClauses
      , makeSet        info setClauses
      , makeWhere      info whereClauses
      , makeGroupBy    info groupByClause
      , makeHaving     info havingClause
      , makeOrderBy    info orderByClauses
      , makeLimit      info limitClause orderByClauses
      , makeLocking         lockingClause
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
-- You must ensure that the 'Mode' you pass to this function corresponds with
-- the actual 'SqlQuery'. If you pass a query that uses incompatible features
-- (like an @INSERT@ statement with a @SELECT@ mode) then you'll get a weird
-- result.
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
-- You must ensure that the 'Mode' you pass to this function corresponds with
-- the actual 'SqlQuery'. If you pass a query that uses incompatible features
-- (like an @INSERT@ statement with a @SELECT@ mode) then you'll get a weird
-- result.
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
-- You must ensure that the 'Mode' you pass to this function corresponds with
-- the actual 'SqlQuery'. If you pass a query that uses incompatible features
-- (like an @INSERT@ statement with a @SELECT@ mode) then you'll get a weird
-- result.
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
-- You must ensure that the 'Mode' you pass to this function corresponds with
-- the actual 'SqlQuery'. If you pass a query that uses incompatible features
-- (like an @INSERT@ statement with a @SELECT@ mode) then you'll get a weird
-- result.
--
-- @since 3.1.1
renderQueryInsertInto
  :: (SqlSelect a r, BackendCompatible SqlBackend backend, Monad m)
  => SqlQuery a
  -- ^ The SQL query you want to render.
  -> R.ReaderT backend m (T.Text, [PersistValue])
renderQueryInsertInto = renderQueryToText INSERT_INTO

-- | (Internal) Mode of query being converted by 'toRawSql'.
data Mode =
    SELECT
  | DELETE
  | UPDATE
  | INSERT_INTO


uncommas :: [TLB.Builder] -> TLB.Builder
uncommas = intersperseB ", "

intersperseB :: TLB.Builder -> [TLB.Builder] -> TLB.Builder
intersperseB a = mconcat . intersperse a . filter (/= mempty)

uncommas' :: Monoid a => [(TLB.Builder, a)] -> (TLB.Builder, a)
uncommas' = (uncommas *** mconcat) . unzip


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
        DistinctOn exprs -> first (("SELECT DISTINCT ON (" <>) . (<> ") ")) $
                            uncommas' (processExpr <$> exprs)
      where processExpr (EDistinctOn f) = materializeExpr info f
    withCols v = v <> sqlSelectCols info ret
    plain    v = (v, [])


makeFrom :: IdentInfo -> Mode -> [FromClause] -> (TLB.Builder, [PersistValue])
makeFrom _    _    [] = mempty
makeFrom info mode fs = ret
  where
    ret = case collectOnClauses fs of
            Left expr -> throw $ mkExc expr
            Right fs' -> keyword $ uncommas' (map (mk Never) fs')
    keyword = case mode of
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

    base ident@(I identText) def =
      let db@(DBName dbText) = entityDB def
      in ( if dbText == identText
           then fromDBName info db
           else fromDBName info db <> (" AS " <> useIdent info ident)
         , mempty )

    fromKind InnerJoinKind      = " INNER JOIN "
    fromKind CrossJoinKind      = " CROSS JOIN "
    fromKind LeftOuterJoinKind  = " LEFT OUTER JOIN "
    fromKind RightOuterJoinKind = " RIGHT OUTER JOIN "
    fromKind FullOuterJoinKind  = " FULL OUTER JOIN "

    makeOnClause (ERaw _ f)        = first (" ON " <>) (f info)
    makeOnClause (ECompositeKey _) = throw (CompositeKeyErr MakeOnClauseError)

    mkExc :: SqlExpr (Value Bool) -> OnClauseWithoutMatchingJoinException
    mkExc (ERaw _ f) =
      OnClauseWithoutMatchingJoinException $
      TL.unpack $ TLB.toLazyText $ fst (f info)
    mkExc (ECompositeKey _) = throw (CompositeKeyErr MakeExcError)

makeSet :: IdentInfo -> [SetClause] -> (TLB.Builder, [PersistValue])
makeSet _    [] = mempty
makeSet info os = first ("\nSET " <>) . uncommas' $ concatMap mk os
  where
    mk (SetClause (ERaw _ f))        = [f info]
    mk (SetClause (ECompositeKey _)) = throw (CompositeKeyErr MakeSetError) -- FIXME

makeWhere :: IdentInfo -> WhereClause -> (TLB.Builder, [PersistValue])
makeWhere _    NoWhere                   = mempty
makeWhere info (Where (ERaw _ f))        = first ("\nWHERE " <>) (f info)
makeWhere _    (Where (ECompositeKey _)) = throw (CompositeKeyErr MakeWhereError)


makeGroupBy :: IdentInfo -> GroupByClause -> (TLB.Builder, [PersistValue])
makeGroupBy _ (GroupBy []) = (mempty, [])
makeGroupBy info (GroupBy fields) = first ("\nGROUP BY " <>) build
  where
    build :: (TLB.Builder, [PersistValue])
    build = uncommas' $ map match fields

    match :: SomeValue -> (TLB.Builder, [PersistValue])
    match (SomeValue (ERaw _ f)) = f info
    match (SomeValue (ECompositeKey f)) = (mconcat $ f info, mempty)

makeHaving :: IdentInfo -> WhereClause -> (TLB.Builder, [PersistValue])
makeHaving _    NoWhere                    = mempty
makeHaving info (Where (ERaw _ f))         = first ("\nHAVING " <>) (f info)
makeHaving _    (Where (ECompositeKey _)) = throw (CompositeKeyErr MakeHavingError)

-- makeHaving, makeWhere and makeOrderBy
makeOrderByNoNewline ::
     IdentInfo -> [OrderByClause] -> (TLB.Builder, [PersistValue])
makeOrderByNoNewline _    [] = mempty
makeOrderByNoNewline info os = first ("ORDER BY " <>) . uncommas' $ concatMap mk os
  where
    mk :: OrderByClause -> [(TLB.Builder, [PersistValue])]
    mk (EOrderBy t (ERaw p f)) = [first ((<> orderByType t) . parensM p) (f info)]
    mk (EOrderBy t (ECompositeKey f)) =
      let fs = f info
          vals = repeat []
      in zip (map (<> orderByType t) fs) vals
    mk EOrderRandom = [first (<> "RANDOM()") mempty]
    orderByType ASC  = " ASC"
    orderByType DESC = " DESC"

makeOrderBy :: IdentInfo -> [OrderByClause] -> (TLB.Builder, [PersistValue])
makeOrderBy _ [] = mempty
makeOrderBy info is =
  let (tlb, vals) = makeOrderByNoNewline info is
  in ("\n" <> tlb, vals)

{-# DEPRECATED EOrderRandom "Since 2.6.0: `rand` ordering function is not uniform across all databases! To avoid accidental partiality it will be removed in the next major version." #-}

makeLimit :: IdentInfo -> LimitClause -> [OrderByClause] -> (TLB.Builder, [PersistValue])
makeLimit (conn, _) (Limit ml mo) orderByClauses =
  let limitRaw = connLimitOffset conn (v ml, v mo) hasOrderClause "\n"
      hasOrderClause = not (null orderByClauses)
      v = maybe 0 fromIntegral
  in (TLB.fromText limitRaw, mempty)


makeLocking :: LockingClause -> (TLB.Builder, [PersistValue])
makeLocking = flip (,) [] . maybe mempty toTLB . Monoid.getLast
  where
    toTLB ForUpdate           = "\nFOR UPDATE"
    toTLB ForUpdateSkipLocked = "\nFOR UPDATE SKIP LOCKED"
    toTLB ForShare            = "\nFOR SHARE"
    toTLB LockInShareMode     = "\nLOCK IN SHARE MODE"



parens :: TLB.Builder -> TLB.Builder
parens b = "(" <> (b <> ")")


----------------------------------------------------------------------


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
instance SqlSelect (SqlExpr InsertFinal) InsertFinal where
  sqlInsertInto info (EInsertFinal (EInsert p _)) =
    let fields = uncommas $
                 map (fromDBName info . fieldDB) $
                 entityFields $
                 entityDef p
        table  = fromDBName info . entityDB . entityDef $ p
    in ("INSERT INTO " <> table <> parens fields <> "\n", [])
  sqlSelectCols info (EInsertFinal (EInsert _ f)) = f info
  sqlSelectColCount   = const 0
  sqlSelectProcessRow =
    const (Right (throw (UnexpectedCaseErr InsertionFinalError)))


-- | Not useful for 'select', but used for 'update' and 'delete'.
instance SqlSelect () () where
  sqlSelectCols _ _ = ("1", [])
  sqlSelectColCount _ = 1
  sqlSelectProcessRow _ = Right ()


-- | You may return an 'Entity' from a 'select' query.
instance PersistEntity a => SqlSelect (SqlExpr (Entity a)) (Entity a) where
  sqlSelectCols info expr@(EEntity ident) = ret
      where
        process ed = uncommas $
                     map ((name <>) . TLB.fromText) $
                     entityColumnNames ed (fst info)
        -- 'name' is the biggest difference between 'RawSql' and
        -- 'SqlSelect'.  We automatically create names for tables
        -- (since it's not the user who's writing the FROM
        -- clause), while 'rawSql' assumes that it's just the
        -- name of the table (which doesn't allow self-joins, for
        -- example).
        name = useIdent info ident <> "."
        ret = let ed = entityDef $ getEntityVal $ return expr
              in (process ed, mempty)
  sqlSelectColCount = entityColumnCount . entityDef . getEntityVal
  sqlSelectProcessRow = parseEntityValues ed
    where ed = entityDef $ getEntityVal (Proxy :: Proxy (SqlExpr (Entity a)))

getEntityVal :: Proxy (SqlExpr (Entity a)) -> Proxy a
getEntityVal = const Proxy


-- | You may return a possibly-@NULL@ 'Entity' from a 'select' query.
instance PersistEntity a => SqlSelect (SqlExpr (Maybe (Entity a))) (Maybe (Entity a)) where
  sqlSelectCols info (EMaybe ent) = sqlSelectCols info ent
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
materializeExpr info (ERaw p f) =
  let (b, vals) = f info
  in (parensM p b, vals)
materializeExpr info (ECompositeKey f) =
  let bs = f info
  in (uncommas $ map (parensM Parens) bs, [])


-- | You may return tuples (up to 16-tuples) and tuples of tuples
-- from a 'select' query.
instance ( SqlSelect a ra
         , SqlSelect b rb
         ) => SqlSelect (a, b) (ra, rb) where
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

to16 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p)) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
to16 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p)) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)


-- | Insert a 'PersistField' for every selected value.
--
-- /Since: 2.4.2/
insertSelect :: (MonadIO m, PersistEntity a) =>
  SqlQuery (SqlExpr (Insertion a)) -> SqlWriteT m ()
insertSelect = void . insertSelectCount

-- | Insert a 'PersistField' for every selected value, return the count afterward
insertSelectCount :: (MonadIO m, PersistEntity a) =>
  SqlQuery (SqlExpr (Insertion a)) -> SqlWriteT m Int64
insertSelectCount = rawEsqueleto INSERT_INTO . fmap EInsertFinal
