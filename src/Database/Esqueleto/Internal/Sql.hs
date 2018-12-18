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
module Database.Esqueleto.Internal.Sql
  ( -- * The pretty face
    SqlQuery
  , SqlExpr(..)
  , SqlEntity
  , select
  , selectSource
  , delete
  , deleteCount
  , update
  , updateCount
  , insertSelect
  , insertSelectCount
    -- * The guts
  , unsafeSqlCase
  , unsafeSqlBinOp
  , unsafeSqlBinOpComposite
  , unsafeSqlValue
  , unsafeSqlCastAs
  , unsafeSqlFunction
  , unsafeSqlExtractSubField
  , UnsafeSqlFunctionArgument
  , OrderByClause
  , rawSelectSource
  , runSource
  , rawEsqueleto
  , toRawSql
  , Mode(..)
  , NeedParens(..)
  , IdentState
  , initialIdentState
  , IdentInfo
  , SqlSelect(..)
  , veryUnsafeCoerceSqlExprValue
  , veryUnsafeCoerceSqlExprValueList
  -- * Helper functions
  , makeOrderByNoNewline
  , uncommas'
  , parens
  , toArgList
  , builderToText
  ) where

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
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB

import Database.Esqueleto.Internal.Language

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
  | SqlBinOpError
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
newtype GroupByClause = GroupBy [SomeValue SqlExpr]

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


instance Esqueleto SqlQuery SqlExpr SqlBackend where
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

  fromStartMaybe = maybelize <$> fromStart
    where
      maybelize :: SqlExpr (PreprocessedFrom (SqlExpr (Entity a)))
                -> SqlExpr (PreprocessedFrom (SqlExpr (Maybe (Entity a))))
      maybelize (EPreprocessedFrom ret from_) = EPreprocessedFrom (EMaybe ret) from_

  fromJoin (EPreprocessedFrom lhsRet lhsFrom)
           (EPreprocessedFrom rhsRet rhsFrom) = Q $ do
    let ret   = smartJoin lhsRet rhsRet
        from_ = FromJoin lhsFrom             -- LHS
                         (reifyJoinKind ret) -- JOIN
                         rhsFrom             -- RHS
                         Nothing             -- ON
    return (EPreprocessedFrom ret from_)

  fromFinish (EPreprocessedFrom ret from_) = Q $ do
    W.tell mempty { sdFromClause = [from_] }
    return ret

  where_ expr = Q $ W.tell mempty { sdWhereClause = Where expr }

  on expr = Q $ W.tell mempty { sdFromClause = [OnClause expr] }

  groupBy expr = Q $ W.tell mempty { sdGroupByClause = GroupBy $ toSomeValues expr }

  having expr = Q $ W.tell mempty { sdHavingClause = Where expr }

  locking kind = Q $ W.tell mempty { sdLockingClause = Monoid.Last (Just kind) }

  orderBy exprs = Q $ W.tell mempty { sdOrderByClause = exprs }
  asc  = EOrderBy ASC
  desc = EOrderBy DESC

  rand = EOrderRandom

  limit  n = Q $ W.tell mempty { sdLimitClause = Limit (Just n) Nothing  }
  offset n = Q $ W.tell mempty { sdLimitClause = Limit Nothing  (Just n) }

  distinct         act = Q (W.tell mempty { sdDistinctClause = DistinctStandard }) >> act
  distinctOn exprs act = Q (W.tell mempty { sdDistinctClause = DistinctOn exprs }) >> act
  don = EDistinctOn
  distinctOnOrderBy exprs act =
    distinctOn (toDistinctOn <$> exprs) $ do
      orderBy exprs
      act
    where
      toDistinctOn :: SqlExpr OrderBy -> SqlExpr DistinctOn
      toDistinctOn (EOrderBy _ f) = EDistinctOn f
      toDistinctOn EOrderRandom =
        error "We can't select distinct by a random order!"

  sub_select         = sub SELECT

  (^.) :: forall val typ. (PersistEntity val, PersistField typ)
       => SqlExpr (Entity val) -> EntityField val typ -> SqlExpr (Value typ)
  EEntity ident ^. field
    | isComposite = ECompositeKey $ \info ->  dot info <$> compositeFields pdef
    | otherwise   = ERaw Never    $ \info -> (dot info  $  persistFieldDef field, [])
    where
      isComposite = isIdField field && hasCompositeKey ed
      dot info x  = useIdent info ident <> "." <> fromDBName info (fieldDB x)
      ed          = entityDef $ getEntityVal (Proxy :: Proxy (SqlExpr (Entity val)))
      Just pdef   = entityPrimary ed

  withNonNull :: PersistField typ
              => SqlExpr (Value (Maybe typ))
              -> (SqlExpr (Value typ) -> SqlQuery a)
              -> SqlQuery a
  withNonNull field f = do
    where_ $ not_ $ isNothing field
    f $ veryUnsafeCoerceSqlExprValue field

  EMaybe r ?. field = just (r ^. field)

  val v = ERaw Never $ const ("?", [toPersistValue v])

  isNothing (ERaw p f)        = ERaw Parens $ first ((<> " IS NULL") . parensM p) . f
  isNothing (ECompositeKey f) = ERaw Parens $ flip (,) [] . (intersperseB " AND " . map (<> " IS NULL")) . f
  just (ERaw p f)        = ERaw p f
  just (ECompositeKey f) = ECompositeKey f
  nothing = unsafeSqlValue "NULL"
  joinV (ERaw p f)        = ERaw p f
  joinV (ECompositeKey f) = ECompositeKey f
  countRows     = unsafeSqlValue "COUNT(*)"
  count         = countHelper ""           ""
  countDistinct = countHelper "(DISTINCT " ")"

  not_ (ERaw p f) = ERaw Never $ \info -> let (b, vals) = f info
                                          in ("NOT " <> parensM p b, vals)
  not_ (ECompositeKey _) = throw (CompositeKeyErr NotError)

  (==.) = unsafeSqlBinOpComposite " = " " AND "
  (!=.) = unsafeSqlBinOpComposite " != " " OR "
  (>=.) = unsafeSqlBinOp " >= "
  (>.)  = unsafeSqlBinOp " > "
  (<=.) = unsafeSqlBinOp " <= "
  (<.)  = unsafeSqlBinOp " < "
  (&&.) = unsafeSqlBinOp " AND "
  (||.) = unsafeSqlBinOp " OR "
  (+.)  = unsafeSqlBinOp " + "
  (-.)  = unsafeSqlBinOp " - "
  (/.)  = unsafeSqlBinOp " / "
  (*.)  = unsafeSqlBinOp " * "

  random_  = unsafeSqlValue "RANDOM()"
  round_   = unsafeSqlFunction "ROUND"
  ceiling_ = unsafeSqlFunction "CEILING"
  floor_   = unsafeSqlFunction "FLOOR"

  sum_     = unsafeSqlFunction "SUM"
  min_     = unsafeSqlFunction "MIN"
  max_     = unsafeSqlFunction "MAX"
  avg_     = unsafeSqlFunction "AVG"

  castNum  = veryUnsafeCoerceSqlExprValue
  castNumM = veryUnsafeCoerceSqlExprValue

  coalesce              = unsafeSqlFunctionParens "COALESCE"
  coalesceDefault exprs = unsafeSqlFunctionParens "COALESCE" . (exprs ++) . return . just

  lower_  = unsafeSqlFunction "LOWER"
  like    = unsafeSqlBinOp    " LIKE "
  ilike   = unsafeSqlBinOp    " ILIKE "
  (%)     = unsafeSqlValue    "'%'"
  concat_ = unsafeSqlFunction "CONCAT"
  (++.)   = unsafeSqlBinOp    " || "
  castString = veryUnsafeCoerceSqlExprValue

  subList_select         = EList . sub_select

  valList []   = EEmptyList
  valList vals = EList $ ERaw Parens $ const ( uncommas ("?" <$ vals)
                                             , map toPersistValue vals )

  justList EEmptyList = EEmptyList
  justList (EList v)  = EList (just v)

  v `in_`   e = ifNotEmptyList e False $ unsafeSqlBinOp     " IN " v (veryUnsafeCoerceSqlExprValueList e)
  v `notIn` e = ifNotEmptyList e True  $ unsafeSqlBinOp " NOT IN " v (veryUnsafeCoerceSqlExprValueList e)

  exists    = unsafeSqlFunction     "EXISTS " . existsHelper
  notExists = unsafeSqlFunction "NOT EXISTS " . existsHelper

  set ent upds = Q $ W.tell mempty { sdSetClause = map apply upds }
    where
      apply (ESet f) = SetClause (f ent)

  field  =. expr = setAux field (const expr)
  field +=. expr = setAux field (\ent -> ent ^. field +. expr)
  field -=. expr = setAux field (\ent -> ent ^. field -. expr)
  field *=. expr = setAux field (\ent -> ent ^. field *. expr)
  field /=. expr = setAux field (\ent -> ent ^. field /. expr)

  (<#) _ (ERaw _ f)        = EInsert Proxy f
  (<#) _ (ECompositeKey _) = throw (CompositeKeyErr ToInsertionError)

  (EInsert _ f) <&> (ERaw _ g) = EInsert Proxy $ \x ->
    let (fb, fv) = f x
        (gb, gv) = g x
    in (fb <> ", " <> gb, fv ++ gv)
  (EInsert _ _) <&> (ECompositeKey _) = throw (CompositeKeyErr CombineInsertionError)

  case_ = unsafeSqlCase
  toBaseId = veryUnsafeCoerceSqlExprValue


instance ToSomeValues SqlExpr (SqlExpr (Value a)) where
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
unsafeSqlBinOp _ _ _ = throw (CompositeKeyErr SqlBinOpError)
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
  ( PersistEntityBackend val ~ backend
  , PersistEntity val
  , PersistUniqueWrite backend
  , PersistQueryWrite backend
  , BackendCompatible SqlBackend backend
  , PersistEntity val
  , MonadIO m
  )
  => (SqlExpr (Entity val) -> SqlQuery ())
  -> R.ReaderT backend m ()
update = void . updateCount

-- | Same as 'update', but returns the number of rows affected.
updateCount :: ( MonadIO m
               , PersistEntity val
               , PersistEntityBackend val ~ backend
               , BackendCompatible SqlBackend backend
               , PersistQueryWrite backend
               , PersistUniqueWrite backend)
            => (SqlExpr (Entity val) -> SqlQuery ())
            -> R.ReaderT backend m Int64
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
-- possible but tedious), you may just turn on query logging of
-- @persistent@.
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

    match :: SomeValue SqlExpr -> (TLB.Builder, [PersistValue])
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
