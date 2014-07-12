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
  , Update
  , Insertion
    -- * The guts
  , JoinKind(..)
  , IsJoinKind(..)
  , PreprocessedFrom
  , From
  , FromPreprocess
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Exception (Exception)
import Data.Int (Int64)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Database.Esqueleto.Internal.PersistentImport


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
  -- from $ \\(foo ``InnerJoin`` bar) -> do
  --   on (foo ^. FooId ==. bar ^. BarFooId)
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
  -- from $ \\(foo ``InnerJoin`` bar ``InnerJoin`` baz) -> do
  --   on (baz ^. BazId ==. bar ^. BarBazId)
  --   on (foo ^. FooId ==. bar ^. BarFooId)
  --   ...
  -- @
  --
  -- The order is /reversed/ in order to improve composability.
  -- For example, consider @query1@ and @query2@ below:
  --
  -- @
  -- let query1 =
  --       from $ \\(foo ``InnerJoin`` bar) -> do
  --         on (foo ^. FooId ==. bar ^. BarFooId)
  --     query2 =
  --       from $ \\(mbaz ``LeftOuterJoin`` quux) -> do
  --         return (mbaz ?. BazName, quux)
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
  -- select $ from \\(foo ``InnerJoin`` bar) -> do
  --   on (foo ^. FooBarId ==. bar ^. BarId)
  --   groupBy (bar ^. BarId, bar ^. BarName)
  --   return (bar ^. BarId, bar ^. BarName, countRows)
  -- @
  --
  -- With groupBy you can sort by aggregate functions, like so (we
  -- used @let@ to restrict the more general `countRows` to
  -- @SqlExpr (Value Int)@ to avoid ambiguity):
  --
  -- @
  -- r \<- select $ from \\(foo ``InnerJoin`` bar) -> do
  --   on (foo ^. FooBarId ==. bar ^. BarId)
  --   groupBy $ bar ^. BarName
  --   let countRows' = countRows
  --   orderBy [asc countRows']
  --   return (bar ^. BarName, countRows')
  -- forM_ r $ \\((Value name), (Value count)) -> do
  --   print name
  --   print (count :: Int)
  -- @
  groupBy :: (ToSomeValues expr a) => a -> query ()

  -- | @ORDER BY@ clause. See also 'asc' and 'desc'.
  orderBy :: [expr OrderBy] -> query ()

  -- | Ascending order of this field or expression.
  asc :: PersistField a => expr (Value a) -> expr OrderBy

  -- | Descending order of this field or expression.
  desc :: PersistField a => expr (Value a) -> expr OrderBy

  -- | @LIMIT@.  Limit the number of returned rows.
  limit :: Int64 -> query ()

  -- | @OFFSET@.  Usually used with 'limit'.
  offset :: Int64 -> query ()

  -- | @ORDER BY random()@ clause.
  --
  -- /Since: 1.3.10/
  rand :: expr OrderBy

  -- | @HAVING@.
  --
  -- /Since: 1.2.2/
  having :: expr (Value Bool) -> query ()

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
  -- one of type @Maybe typ@.  It should hold that @val . Just
  -- === just . val@.
  just :: expr (Value typ) -> expr (Value (Maybe typ))

  -- | @NULL@ value.
  nothing :: expr (Value (Maybe typ))

  -- | Join nested 'Maybe's in a 'Value' into one. This is useful when
  -- calling aggregate functions on nullable fields.
  joinV :: expr (Value (Maybe (Maybe typ))) -> expr (Value (Maybe typ))

  -- | @COUNT(*)@ value.
  countRows :: Num a => expr (Value a)

  -- | @COUNT@.
  count :: (Num a) => expr (Value typ) -> expr (Value a)

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

  -- | @LIKE@ operator.
  like :: (PersistField s, IsString s) => expr (Value s) -> expr (Value s) -> expr (Value Bool)
  -- | The string @'%'@.  May be useful while using 'like' and
  -- concatenation ('concat_' or '++.', depending on your
  -- database).  Note that you always to type the parenthesis,
  -- for example:
  --
  -- @
  -- name ``'like'`` (%) ++. val "John" ++. (%)
  -- @
  (%) :: (PersistField s, IsString s) => expr (Value s)
  -- | The @CONCAT@ function with a variable number of
  -- parameters.  Supported by MySQL and PostgreSQL.
  concat_ :: (PersistField s, IsString s) => [expr (Value s)] -> expr (Value s)
  -- | The @||@ string concatenation operator (named after
  -- Haskell's '++' in order to avoid naming clash with '||.').
  -- Supported by SQLite and PostgreSQL.
  (++.) :: (PersistField s, IsString s) => expr (Value s) -> expr (Value s) -> expr (Value s)

  -- | Execute a subquery @SELECT@ in an expression.  Returns a
  -- list of values.
  subList_select :: PersistField a => query (expr (Value a)) -> expr (ValueList a)

  -- | Same as 'sublist_select' but using @SELECT DISTINCT@.
  subList_selectDistinct :: PersistField a => query (expr (Value a)) -> expr (ValueList a)

  -- | Lift a list of constant value from Haskell-land to the query.
  valList :: PersistField typ => [typ] -> expr (ValueList typ)

  -- | @IN@ operator.
  in_ :: PersistField typ => expr (Value typ) -> expr (ValueList typ) -> expr (Value Bool)

  -- | @NOT IN@ operator.
  notIn :: PersistField typ => expr (Value typ) -> expr (ValueList typ) -> expr (Value Bool)

  -- | @EXISTS@ operator.  For example:
  --
  -- @
  -- select $
  -- from $ \\person -> do
  -- where_ $ exists $
  --          from $ \\post -> do
  --          where_ (post ^. BlogPostAuthorId ==. person ^. PersonId)
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


-- Fixity declarations
infixl 9 ^.
infixl 7 *., /.
infixl 6 +., -.
infixr 5 ++.
infix  4 ==., >=., >., <=., <., !=.
infixr 3 &&., =., +=., -=., *=., /=.
infixr 2 ||., `InnerJoin`, `CrossJoin`, `LeftOuterJoin`, `RightOuterJoin`, `FullOuterJoin`, `like`


-- | A single value (as opposed to a whole entity).  You may use
-- @('^.')@ or @('?.')@ to get a 'Value' from an 'Entity'.
data Value a = Value a deriving (Eq, Ord, Show, Typeable)
-- Note: because of GHC bug #6124 we use @data@ instead of @newtype@.
-- <https://ghc.haskell.org/trac/ghc/ticket/6124>


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
-- instances for tuples and is the reason why groupBy can take tuples, like
-- @groupBy (foo ^. FooId, foo ^. FooName, foo ^. FooType)@.
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
-- from $ \\(person ``LeftOuterJoin`` pet) ->
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


-- | Phantom type for a @SET@ operation on an entity of the given
-- type (see 'set' and '(=.)').
data Update typ


-- | Phantom type used by 'insertSelect'.
data Insertion a


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
--  The @JOINs@ have right fixity, the same as in SQL.
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
-- from $ \\person -> ...
-- from $ \\(person, blogPost) -> ...
-- from $ \\(p ``LeftOuterJoin`` mb) -> ...
-- from $ \\(p1 ``InnerJoin`` f ``InnerJoin`` p2) -> ...
-- from $ \\((p1 ``InnerJoin`` f) ``InnerJoin`` p2) -> ...
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
-- (p ``LeftOuterJoin`` mb)
--   :: (...) => InnerJoin (expr (Entity Person)) (expr (Maybe (Entity BlogPost)))
-- (p1 ``InnerJoin`` f ``InnerJoin`` p2)
--   :: (...) => InnerJoin
--                 (expr (Entity Person))
--                 (InnerJoin (expr (Entity Follow))
--                            (expr (Entity Person)))
-- ((p1 ``InnerJoin`` f) ``InnerJoin`` p2) ::
--   :: (...) => InnerJoin
--                 (InnerJoin (expr (Entity Person))
--                            (expr (Entity Follow)))
--                 (expr (Entity Person))
-- @
--
-- Note that some backends may not support all kinds of @JOIN@s.
-- For example, when using the SQL backend with SQLite, it will
-- not accept the last example above (which is associated to the
-- left, instead of being to the right) and will not accept
-- 'RightOuterJoin's or 'FullOuterJoin's.
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
