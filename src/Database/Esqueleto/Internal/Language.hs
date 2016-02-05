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
-- | This is an internal module, anything exported by this module
-- may change without a major version bump.  Please use only
-- "Database.Esqueleto" if possible.
module Database.Esqueleto.Internal.Language
  ( -- * The pretty face
    Esqueleto(..)
  , from
  , Value(..)
  , unValue
  , ValueList(..)
  , SomeValue(..)
  , ToSomeValues(..)
  , InnerJoin(..)
  , CrossJoin(..)
  , LeftOuterJoin(..)
  , RightOuterJoin(..)
  , FullOuterJoin(..)
  , OnClauseWithoutMatchingJoinException(..)
  , OrderBy
  , DistinctOn
  , Update
  , Insertion
  , LockingKind(..)
  , SqlString
  , ToBaseId(..)
    -- * The guts
  , JoinKind(..)
  , IsJoinKind(..)
  , PreprocessedFrom
  , From
  , FromPreprocess
  , when_
  , then_
  , else_
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Exception (Exception)
import Data.Int (Int64)
import Data.Typeable (Typeable)
import Database.Esqueleto.Internal.PersistentImport
import Text.Blaze.Html (Html)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL


-- | Finally tagless representation of @esqueleto@'s EDSL.
class (Functor query, Applicative query, Monad query) =>
      Esqueleto query expr backend | query -> expr backend, expr -> query backend where
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
       , PersistEntityBackend a ~ backend )
    => query (expr (PreprocessedFrom (expr (Entity a))))
  -- | (Internal) Same as 'fromStart', but entity may be missing.
  fromStartMaybe
    :: ( PersistEntity a
       , PersistEntityBackend a ~ backend )
    => query (expr (PreprocessedFrom (expr (Maybe (Entity a)))))
  -- | (Internal) Do a @JOIN@.
  fromJoin
    :: IsJoinKind join
    => expr (PreprocessedFrom a)
    -> expr (PreprocessedFrom b)
    -> query (expr (PreprocessedFrom (join a b)))
  -- | (Internal) Finish a @JOIN@.
  fromFinish
    :: expr (PreprocessedFrom a)
    -> query a

  -- | @WHERE@ clause: restrict the query's result.
  where_ :: expr (Value Bool) -> query ()

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
  on :: expr (Value Bool) -> query ()

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
  -- @SqlExpr (Value Int)@ to avoid ambiguity---the second use of
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
  groupBy :: (ToSomeValues expr a) => a -> query ()

  -- | @ORDER BY@ clause. See also 'asc' and 'desc'.
  --
  -- Multiple calls to 'orderBy' get concatenated on the final
  -- query, including 'distinctOnOrderBy'.
  orderBy :: [expr OrderBy] -> query ()

  -- | Ascending order of this field or expression.
  asc :: PersistField a => expr (Value a) -> expr OrderBy

  -- | Descending order of this field or expression.
  desc :: PersistField a => expr (Value a) -> expr OrderBy

  -- | @LIMIT@.  Limit the number of returned rows.
  limit :: Int64 -> query ()

  -- | @OFFSET@.  Usually used with 'limit'.
  offset :: Int64 -> query ()

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
  distinct :: query a -> query a

  -- | @DISTINCT ON@.  Change the current @SELECT@ into
  -- @SELECT DISTINCT ON (expressions)@.  For example:
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
  -- Each call to 'distinctOn' adds more expressions.  Calls to
  -- 'distinctOn' override any calls to 'distinct'.
  --
  -- Note that PostgreSQL requires the expressions on @DISTINCT
  -- ON@ to be the first ones to appear on a @ORDER BY@.  This is
  -- not managed automatically by esqueleto, keeping its spirit
  -- of trying to be close to raw SQL.
  --
  -- Supported by PostgreSQL only.
  --
  -- /Since: 2.2.4/
  distinctOn :: [expr DistinctOn] -> query a -> query a

  -- | Erase an expression's type so that it's suitable to
  -- be used by 'distinctOn'.
  --
  -- /Since: 2.2.4/
  don :: expr (Value a) -> expr DistinctOn

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
  distinctOnOrderBy :: [expr OrderBy] -> query a -> query a

  -- | @ORDER BY random()@ clause.
  --
  -- /Since: 1.3.10/
  rand :: expr OrderBy

  -- | @HAVING@.
  --
  -- /Since: 1.2.2/
  having :: expr (Value Bool) -> query ()

  -- | Add a locking clause to the query.  Please read
  -- 'LockingKind' documentation and your RDBMS manual.
  --
  -- If multiple calls to 'locking' are made on the same query,
  -- the last one is used.
  --
  -- /Since: 2.2.7/
  locking :: LockingKind -> query ()

  -- | Execute a subquery @SELECT@ in an expression.  Returns a
  -- simple value so should be used only when the @SELECT@ query
  -- is guaranteed to return just one row.
  sub_select :: PersistField a => query (expr (Value a)) -> expr (Value a)

  -- | Same as 'sub_select' but using @SELECT DISTINCT@.
  sub_selectDistinct :: PersistField a => query (expr (Value a)) -> expr (Value a)

  -- | Project a field of an entity.
  (^.) :: (PersistEntity val, PersistField typ) =>
          expr (Entity val) -> EntityField val typ -> expr (Value typ)

  -- | Project a field of an entity that may be null.
  (?.) :: (PersistEntity val, PersistField typ) =>
          expr (Maybe (Entity val)) -> EntityField val typ -> expr (Value (Maybe typ))

  -- | Lift a constant value from Haskell-land to the query.
  val  :: PersistField typ => typ -> expr (Value typ)

  -- | @IS NULL@ comparison.
  isNothing :: PersistField typ => expr (Value (Maybe typ)) -> expr (Value Bool)

  -- | Analogous to 'Just', promotes a value of type @typ@ into
  -- one of type @Maybe typ@.  It should hold that @'val' . Just
  -- === just . 'val'@.
  just :: expr (Value typ) -> expr (Value (Maybe typ))

  -- | @NULL@ value.
  nothing :: expr (Value (Maybe typ))

  -- | Join nested 'Maybe's in a 'Value' into one. This is useful when
  -- calling aggregate functions on nullable fields.
  joinV :: expr (Value (Maybe (Maybe typ))) -> expr (Value (Maybe typ))

  -- | @COUNT(*)@ value.
  countRows :: Num a => expr (Value a)

  -- | @COUNT@.
  count :: Num a => expr (Value typ) -> expr (Value a)

  -- | @COUNT(DISTINCT x)@.
  --
  -- /Since: 2.4.1/
  countDistinct :: Num a => expr (Value typ) -> expr (Value a)

  not_ :: expr (Value Bool) -> expr (Value Bool)

  (==.) :: PersistField typ => expr (Value typ) -> expr (Value typ) -> expr (Value Bool)
  (>=.) :: PersistField typ => expr (Value typ) -> expr (Value typ) -> expr (Value Bool)
  (>.)  :: PersistField typ => expr (Value typ) -> expr (Value typ) -> expr (Value Bool)
  (<=.) :: PersistField typ => expr (Value typ) -> expr (Value typ) -> expr (Value Bool)
  (<.)  :: PersistField typ => expr (Value typ) -> expr (Value typ) -> expr (Value Bool)
  (!=.) :: PersistField typ => expr (Value typ) -> expr (Value typ) -> expr (Value Bool)

  (&&.) :: expr (Value Bool) -> expr (Value Bool) -> expr (Value Bool)
  (||.) :: expr (Value Bool) -> expr (Value Bool) -> expr (Value Bool)

  (+.)  :: PersistField a => expr (Value a) -> expr (Value a) -> expr (Value a)
  (-.)  :: PersistField a => expr (Value a) -> expr (Value a) -> expr (Value a)
  (/.)  :: PersistField a => expr (Value a) -> expr (Value a) -> expr (Value a)
  (*.)  :: PersistField a => expr (Value a) -> expr (Value a) -> expr (Value a)


  random_  :: (PersistField a, Num a) => expr (Value a)
  round_   :: (PersistField a, Num a, PersistField b, Num b) => expr (Value a) -> expr (Value b)
  ceiling_ :: (PersistField a, Num a, PersistField b, Num b) => expr (Value a) -> expr (Value b)
  floor_   :: (PersistField a, Num a, PersistField b, Num b) => expr (Value a) -> expr (Value b)

  sum_     :: (PersistField a, PersistField b) => expr (Value a) -> expr (Value (Maybe b))
  min_     :: (PersistField a) => expr (Value a) -> expr (Value (Maybe a))
  max_     :: (PersistField a) => expr (Value a) -> expr (Value (Maybe a))
  avg_     :: (PersistField a, PersistField b) => expr (Value a) -> expr (Value (Maybe b))

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
  castNum :: (Num a, Num b) => expr (Value a) -> expr (Value b)
  -- | Same as 'castNum', but for nullable values.
  --
  -- /Since: 2.2.9/
  castNumM :: (Num a, Num b) => expr (Value (Maybe a)) -> expr (Value (Maybe b))

  -- | @COALESCE@ function. Evaluates the arguments in order and
  -- returns the value of the first non-NULL expression, or NULL
  -- (Nothing) otherwise. Some RDBMSs (such as SQLite) require
  -- at least two arguments; please refer to the appropriate
  -- documentation.
  --
  -- /Since: 1.4.3/
  coalesce :: PersistField a => [expr (Value (Maybe a))] -> expr (Value (Maybe a))

  -- | Like @coalesce@, but takes a non-nullable expression
  -- placed at the end of the expression list, which guarantees
  -- a non-NULL result.
  --
  -- /Since: 1.4.3/
  coalesceDefault :: PersistField a => [expr (Value (Maybe a))] -> expr (Value a) -> expr (Value a)

  -- | @LOWER@ function.
  lower_ :: SqlString s => expr (Value s) -> expr (Value s)
  -- | @LIKE@ operator.
  like :: SqlString s => expr (Value s) -> expr (Value s) -> expr (Value Bool)
  -- | @ILIKE@ operator (case-insensitive @LIKE@).
  --
  -- Supported by PostgreSQL only.
  --
  -- /Since: 2.2.3/
  ilike :: SqlString s => expr (Value s) -> expr (Value s) -> expr (Value Bool)
  -- | The string @'%'@.  May be useful while using 'like' and
  -- concatenation ('concat_' or '++.', depending on your
  -- database).  Note that you always have to type the parenthesis,
  -- for example:
  --
  -- @
  -- name `'like`` (%) ++. 'val' \"John\" ++. (%)
  -- @
  (%) :: SqlString s => expr (Value s)
  -- | The @CONCAT@ function with a variable number of
  -- parameters.  Supported by MySQL and PostgreSQL.
  concat_ :: SqlString s => [expr (Value s)] -> expr (Value s)
  -- | The @||@ string concatenation operator (named after
  -- Haskell's '++' in order to avoid naming clash with '||.').
  -- Supported by SQLite and PostgreSQL.
  (++.) :: SqlString s => expr (Value s) -> expr (Value s) -> expr (Value s)
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
  castString :: (SqlString s, SqlString r) => expr (Value s) -> expr (Value r)

  -- | Execute a subquery @SELECT@ in an expression.  Returns a
  -- list of values.
  subList_select :: PersistField a => query (expr (Value a)) -> expr (ValueList a)

  -- | Same as 'sublist_select' but using @SELECT DISTINCT@.
  subList_selectDistinct :: PersistField a => query (expr (Value a)) -> expr (ValueList a)

  -- | Lift a list of constant value from Haskell-land to the query.
  valList :: PersistField typ => [typ] -> expr (ValueList typ)

  -- | Same as 'just' but for 'ValueList'.  Most of the time you
  -- won't need it, though, because you can use 'just' from
  -- inside 'subList_select' or 'Just' from inside 'valList'.
  --
  -- /Since: 2.2.12/
  justList :: expr (ValueList typ) -> expr (ValueList (Maybe typ))

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
  -- 'where_' $ person '^.' PersonId `in_` 'valList' personIds
  -- return person
  -- @
  --
  -- Where @personIds@ is of type @[Key Person]@.
  in_ :: PersistField typ => expr (Value typ) -> expr (ValueList typ) -> expr (Value Bool)

  -- | @NOT IN@ operator.
  notIn :: PersistField typ => expr (Value typ) -> expr (ValueList typ) -> expr (Value Bool)

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
  exists :: query () -> expr (Value Bool)

  -- | @NOT EXISTS@ operator.
  notExists :: query () -> expr (Value Bool)

  -- | @SET@ clause used on @UPDATE@s.  Note that while it's not
  -- a type error to use this function on a @SELECT@, it will
  -- most certainly result in a runtime error.
  set :: PersistEntity val => expr (Entity val) -> [expr (Update val)] -> query ()

  (=.)  :: (PersistEntity val, PersistField typ) => EntityField val typ -> expr (Value typ) -> expr (Update val)
  (+=.) :: (PersistEntity val, PersistField a) => EntityField val a -> expr (Value a) -> expr (Update val)
  (-=.) :: (PersistEntity val, PersistField a) => EntityField val a -> expr (Value a) -> expr (Update val)
  (*=.) :: (PersistEntity val, PersistField a) => EntityField val a -> expr (Value a) -> expr (Update val)
  (/=.) :: (PersistEntity val, PersistField a) => EntityField val a -> expr (Value a) -> expr (Update val)

  -- | Apply a 'PersistField' constructor to @expr Value@ arguments.
  (<#) :: (a -> b) -> expr (Value a) -> expr (Insertion b)

  -- | Apply extra @expr Value@ arguments to a 'PersistField' constructor
  (<&>) :: expr (Insertion (a -> b)) -> expr (Value a) -> expr (Insertion b)

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
  case_ :: PersistField a => [(expr (Value Bool), expr (Value a))] -> expr (Value a) -> expr (Value a)

  -- | Convert an entity's key into another entity's.
  --
  -- This function is to be used when you change an entity's @Id@ to be
  -- that of another entity.  For example:
  --
  -- @
  -- Bar
  --   barNum Int
  -- Foo
  --   Id BarId
  --   fooNum Int
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
  toBaseId :: ToBaseId ent => expr (Value (Key ent)) -> expr (Value (Key (BaseEnt ent)))

{-# DEPRECATED sub_selectDistinct "Since 2.2.4: use 'sub_select' and 'distinct'." #-}
{-# DEPRECATED subList_selectDistinct "Since 2.2.4: use 'subList_select' and 'distinct'." #-}


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
data Value a = Value a deriving (Eq, Ord, Show, Typeable)
-- Note: because of GHC bug #6124 we use @data@ instead of @newtype@.
-- <https://ghc.haskell.org/trac/ghc/ticket/6124>


-- | /Since: 1.4.4/
instance Functor Value where
  fmap f (Value a) = Value (f a)


-- | Unwrap a 'Value'.
--
-- /Since: 1.4.1/
unValue :: Value a -> a
unValue (Value a) = a


-- | A list of single values.  There's a limited set of functions
-- able to work with this data type (such as 'subList_select',
-- 'valList', 'in_' and 'exists').
data ValueList a = ValueList a deriving (Eq, Ord, Show, Typeable)
-- Note: because of GHC bug #6124 we use @data@ instead of @newtype@.
-- <https://ghc.haskell.org/trac/ghc/ticket/6124>


-- | A wrapper type for for any @expr (Value a)@ for all a.
data SomeValue expr where
  SomeValue :: Esqueleto query expr backend => expr (Value a) -> SomeValue expr

-- | A class of things that can be converted into a list of SomeValue. It has
-- instances for tuples and is the reason why 'groupBy' can take tuples, like
-- @'groupBy' (foo '^.' FooId, foo '^.' FooName, foo '^.' FooType)@.
class ToSomeValues expr a where
  toSomeValues :: a -> [SomeValue expr]

instance ( ToSomeValues expr a
         , ToSomeValues expr b
         ) => ToSomeValues expr (a, b) where
  toSomeValues (a,b) = toSomeValues a ++ toSomeValues b

instance ( ToSomeValues expr a
         , ToSomeValues expr b
         , ToSomeValues expr c
         ) => ToSomeValues expr (a, b, c) where
  toSomeValues (a,b,c) = toSomeValues a ++ toSomeValues b ++ toSomeValues c

instance ( ToSomeValues expr a
         , ToSomeValues expr b
         , ToSomeValues expr c
         , ToSomeValues expr d
         ) => ToSomeValues expr (a, b, c, d) where
  toSomeValues (a,b,c,d) = toSomeValues a ++ toSomeValues b ++ toSomeValues c ++
    toSomeValues d

instance ( ToSomeValues expr a
         , ToSomeValues expr b
         , ToSomeValues expr c
         , ToSomeValues expr d
         , ToSomeValues expr e
         ) => ToSomeValues expr (a, b, c, d, e) where
  toSomeValues (a,b,c,d,e) = toSomeValues a ++ toSomeValues b ++
    toSomeValues c ++ toSomeValues d ++ toSomeValues e

instance ( ToSomeValues expr a
         , ToSomeValues expr b
         , ToSomeValues expr c
         , ToSomeValues expr d
         , ToSomeValues expr e
         , ToSomeValues expr f
         ) => ToSomeValues expr (a, b, c, d, e, f) where
  toSomeValues (a,b,c,d,e,f) = toSomeValues a ++ toSomeValues b ++
    toSomeValues c ++ toSomeValues d ++ toSomeValues e ++ toSomeValues f

instance ( ToSomeValues expr a
         , ToSomeValues expr b
         , ToSomeValues expr c
         , ToSomeValues expr d
         , ToSomeValues expr e
         , ToSomeValues expr f
         , ToSomeValues expr g
         ) => ToSomeValues expr (a, b, c, d, e, f, g) where
  toSomeValues (a,b,c,d,e,f,g) = toSomeValues a ++ toSomeValues b ++
    toSomeValues c ++ toSomeValues d ++ toSomeValues e ++ toSomeValues f ++
    toSomeValues g

instance ( ToSomeValues expr a
         , ToSomeValues expr b
         , ToSomeValues expr c
         , ToSomeValues expr d
         , ToSomeValues expr e
         , ToSomeValues expr f
         , ToSomeValues expr g
         , ToSomeValues expr h
         ) => ToSomeValues expr (a, b, c, d, e, f, g, h) where
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
from :: From query expr backend a => (a -> query b) -> query b
from = (from_ >>=)


-- | (Internal) Class that implements the tuple 'from' magic (see
-- 'fromStart').
class Esqueleto query expr backend => From query expr backend a where
  from_ :: query a

instance ( Esqueleto query expr backend
         , FromPreprocess query expr backend (expr (Entity val))
         ) => From query expr backend (expr (Entity val)) where
  from_ = fromPreprocess >>= fromFinish

instance ( Esqueleto query expr backend
         , FromPreprocess query expr backend (expr (Maybe (Entity val)))
         ) => From query expr backend (expr (Maybe (Entity val))) where
  from_ = fromPreprocess >>= fromFinish

instance ( Esqueleto query expr backend
         , FromPreprocess query expr backend (InnerJoin a b)
         ) => From query expr backend (InnerJoin a b) where
  from_ = fromPreprocess >>= fromFinish

instance ( Esqueleto query expr backend
         , FromPreprocess query expr backend (CrossJoin a b)
         ) => From query expr backend (CrossJoin a b) where
  from_ = fromPreprocess >>= fromFinish

instance ( Esqueleto query expr backend
         , FromPreprocess query expr backend (LeftOuterJoin a b)
         ) => From query expr backend (LeftOuterJoin a b) where
  from_ = fromPreprocess >>= fromFinish

instance ( Esqueleto query expr backend
         , FromPreprocess query expr backend (RightOuterJoin a b)
         ) => From query expr backend (RightOuterJoin a b) where
  from_ = fromPreprocess >>= fromFinish

instance ( Esqueleto query expr backend
         , FromPreprocess query expr backend (FullOuterJoin a b)
         ) => From query expr backend (FullOuterJoin a b) where
  from_ = fromPreprocess >>= fromFinish

instance ( From query expr backend a
         , From query expr backend b
         ) => From query expr backend (a, b) where
  from_ = (,) <$> from_ <*> from_

instance ( From query expr backend a
         , From query expr backend b
         , From query expr backend c
         ) => From query expr backend (a, b, c) where
  from_ = (,,) <$> from_ <*> from_ <*> from_

instance ( From query expr backend a
         , From query expr backend b
         , From query expr backend c
         , From query expr backend d
         ) => From query expr backend (a, b, c, d) where
  from_ = (,,,) <$> from_ <*> from_ <*> from_ <*> from_

instance ( From query expr backend a
         , From query expr backend b
         , From query expr backend c
         , From query expr backend d
         , From query expr backend e
         ) => From query expr backend (a, b, c, d, e) where
  from_ = (,,,,) <$> from_ <*> from_ <*> from_ <*> from_ <*> from_

instance ( From query expr backend a
         , From query expr backend b
         , From query expr backend c
         , From query expr backend d
         , From query expr backend e
         , From query expr backend f
         ) => From query expr backend (a, b, c, d, e, f) where
  from_ = (,,,,,) <$> from_ <*> from_ <*> from_ <*> from_ <*> from_ <*> from_

instance ( From query expr backend a
         , From query expr backend b
         , From query expr backend c
         , From query expr backend d
         , From query expr backend e
         , From query expr backend f
         , From query expr backend g
         ) => From query expr backend (a, b, c, d, e, f, g) where
  from_ = (,,,,,,) <$> from_ <*> from_ <*> from_ <*> from_ <*> from_ <*> from_ <*> from_

instance ( From query expr backend a
         , From query expr backend b
         , From query expr backend c
         , From query expr backend d
         , From query expr backend e
         , From query expr backend f
         , From query expr backend g
         , From query expr backend h
         ) => From query expr backend (a, b, c, d, e, f, g, h) where
  from_ = (,,,,,,,) <$> from_ <*> from_ <*> from_ <*> from_ <*> from_ <*> from_ <*> from_ <*> from_



-- | (Internal) Class that implements the @JOIN@ 'from' magic
-- (see 'fromStart').
class Esqueleto query expr backend => FromPreprocess query expr backend a where
  fromPreprocess :: query (expr (PreprocessedFrom a))

instance ( Esqueleto query expr backend
         , PersistEntity val
         , PersistEntityBackend val ~ backend
         ) => FromPreprocess query expr backend (expr (Entity val)) where
  fromPreprocess = fromStart

instance ( Esqueleto query expr backend
         , PersistEntity val
         , PersistEntityBackend val ~ backend
         ) => FromPreprocess query expr backend (expr (Maybe (Entity val))) where
  fromPreprocess = fromStartMaybe

instance ( Esqueleto query expr backend
         , FromPreprocess query expr backend a
         , FromPreprocess query expr backend b
         , IsJoinKind join
         ) => FromPreprocess query expr backend (join a b) where
  fromPreprocess = do
    a <- fromPreprocess
    b <- fromPreprocess
    fromJoin a b
