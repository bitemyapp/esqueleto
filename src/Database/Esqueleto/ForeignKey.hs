{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Database.Esqueleto.ForeignKey
  ( ForeignPair
  , ForeignKey
  , mkForeignInstances
  , onForeignKey
  , foreignKey
  , foreignKeyL
  , foreignKeyR
  , foreignKeyLR
  , foreignKeyLMaybe
  , foreignKeyRMaybe
  , foreignKeyLRMaybe
  )

where

import qualified Data.Char                                    as Char
import qualified Data.Function                                as Function
import qualified Data.List                                    as List
import           Data.Monoid
import qualified Data.Ord                                     as Ord
import qualified Data.Text                                    as Text
import           Data.Traversable                             (forM)
import qualified Language.Haskell.TH                          as TH

import           Database.Esqueleto.Internal.Language
import           Database.Esqueleto.Internal.PersistentImport

-- | Describe a Pair of keys that are part of a foreign key relationship.
--
-- The first element is the entity field that holds the foreign key. The second
-- element is the entity field that holds the references primary key. Type of
-- primary key and foreign key have to coincide.
--
-- For example, give the following Entity definition:
--
-- @
-- Employee
--     num Int
--     Primary numid
--     name Text
--
-- Team
--     teamId Int
--     employee Int
--     Foreign Employee fkEmployee employee
-- @
--
-- The Following ForeignPair would capture the foreign key relationship
--
-- @ForeignPair TeamEmployee EmployeeNum@

data ForeignPair :: * -> * -> * where
    ForeignPair :: forall a b f.
                   (PersistEntity a, PersistEntity b, PersistField f) =>
                    EntityField a f
                 -> EntityField b f
                 -> ForeignPair a b

-- | Describe a unique, canonical foreign key relationship between entities. For
-- example, given the entity definitions from 'ForeignPair', there is exactly
-- one foreign key relationship between Employee and Team, so we can capture it
-- in a type class:
--
-- @
-- instance ForeignKey Team Employee where
--     foreignPairs = [ForeignPair TeamEmployee EmployeeNum]
-- @
--
-- Note that the entity with the foreign key is the _first_ parameter of the
-- type class, the target entity the second
class ForeignKey a b where
  foreignPairs :: [ForeignPair a b]

-- | Apply f to each pair of foreign fields (most likely some variant of equality)
withForeignPairs ::
     (ForeignKey a b, Esqueleto query expr backend)
  => (forall f. (PersistField f, PersistEntity a, PersistEntity b)  =>
                EntityField a f
             -> EntityField b f
             -> expr (Value Bool))
  -> expr (Value Bool)
withForeignPairs f = andL . flip map foreignPairs $ \case
  (ForeignPair xk yk) -> f xk yk

-- | A foreign key constraint between two entities.
--
-- Example:
--
-- @
-- from $ \(team, employee) ->
--   where_ (foreignKey team employee)
--   [...]
-- @
foreignKey :: (ForeignKey a b, Esqueleto query expr backend) =>
              expr (Entity a) -> expr (Entity b) -> expr (Value Bool)
foreignKey x y = withForeignPairs $ \xk yk -> x ^. xk ==. y ^. yk

-- | 'foreignKey' for 'RightOuterJoin'
foreignKeyR  :: (ForeignKey a b, Esqueleto query expr backend) =>
               expr (Entity a) -> expr (Maybe (Entity b)) -> expr (Value Bool)
foreignKeyR x y = withForeignPairs $ \xk yk ->just (x ^. xk) ==. y ?. yk

-- | 'foreignKey' for 'LeftOuterJoin'
foreignKeyL  :: (ForeignKey a b, Esqueleto query expr backend) =>
               expr (Maybe (Entity a)) -> expr (Entity b) -> expr (Value Bool)
foreignKeyL x y = withForeignPairs $ \xk yk ->(x ?. xk) ==. just (y ^. yk)

-- | 'foreignKey' for 'FullOuterJoin'
foreignKeyLR  :: (ForeignKey a b, Esqueleto query expr backend) =>
               expr (Maybe (Entity a)) -> expr (Maybe (Entity b)) -> expr (Value Bool)
foreignKeyLR x y = withForeignPairs $ \xk yk ->(x ?. xk) ==. (y ?. yk)

-- | Like foreignKeyL, but also matches if the foreign reference is NULL
foreignKeyLMaybe :: (Esqueleto query expr backend, ForeignKey a b) =>
                    expr (Maybe( Entity a))
                 -> expr (Entity b)
                 -> expr (Value Bool)
foreignKeyLMaybe x y =
  withForeignPairs $ \xk yk ->
         orL [ isNothing (x ?. xk)
             , x ?. xk ==. just (y ^. yk)
             ]

-- | Like foreignKeyR, but also matches if the target key field is NULL
foreignKeyRMaybe :: (Esqueleto query expr backend, ForeignKey a b) =>
                    expr (Entity a)
                 -> expr (Maybe (Entity b))
                 -> expr (Value Bool)
foreignKeyRMaybe x y =
  withForeignPairs $ \xk yk ->
         orL [ isNothing (y ?. yk)
             , just (x ^. xk) ==. y ?. yk
             ]

-- | Like foreignKeyLR, but also matches if foreign reference or target key are
-- null
foreignKeyLRMaybe :: (Esqueleto query expr backend, ForeignKey a b) =>
                    expr (Maybe( Entity a))
                 -> expr (Maybe (Entity b))
                 -> expr (Value Bool)
foreignKeyLRMaybe x y =
  withForeignPairs $ \xk yk ->
         orL [ isNothing (x ?. xk)
             , isNothing (y ?. yk)
             , x ?. xk ==. y ?. yk
             ]

-- | ON for a foreign key pair
--
-- @onForeignKey a b === on (foreignKey a b)@
--
-- Example:
--
-- @
-- from $ \(team \`InnerJoin\` employee) ->
--   onForeignKey team employee
-- @
onForeignKey :: (Esqueleto query expr backend, ForeignKey a b) =>
                expr (Entity a) -> expr (Entity b) -> query ()
onForeignKey x y = on $ foreignKey x y

--------------------------------------------------------------------------------
-- Automatic Generation of Foreign Key Pairs -----------------------------------
--------------------------------------------------------------------------------

-- | Calculate the foreign relationships from entity definitions.  The resulting
-- list is for each entity the entity it refers to and a list of field pairs
foreignEnts :: [EntityDef] -> [((String, String), [[(String, String)]])]
foreignEnts ents = merge $ do
  ent <- ents
  let entName = unHaskellName $ entityHaskell ent
  -- References to the implicit EntityId fields
  let implicits = do
        field <- entityFields ent
        let nm = unHaskellName $ fieldHaskell field
        ref <- fromForeignRefs $ fieldReference field
        return ( (Text.unpack entName, Text.unpack ref)
               , [(toField entName nm
                  , Text.unpack $  ref <> "Id")])
  -- References that use explicit »Primary« and »Foreign» declarations
      explicits = do
        frgn <- entityForeigns ent
        let remote = unHaskellName $ foreignRefTableHaskell frgn
        return . ((Text.unpack entName,  Text.unpack remote), ) $ do
          ((HaskellName f, _), (HaskellName t, _)) <- foreignFields frgn
          return (toField entName f, toField remote t)
  implicits <> explicits
  where
    merge =
      -- Head is OK here because group never returns emtpty lists.
      map (\xs -> (fst $ head xs, snd <$> xs)) .
      List.groupBy ((==) `Function.on` fst)
      . List.sortBy (Ord.comparing fst)

    fromForeignRefs (ForeignRef x _ ) = pure $ unHaskellName x
    fromForeignRefs _ = mempty
    upcase' = upcase . Text.unpack
    upcase [] = []
    upcase (c:cs) = Char.toUpper c : cs
    toField ent name = Text.unpack ent <> (upcase' name)

-- | Automatically create ForeignKey instances
mkForeignInstances :: [EntityDef] -> TH.Q [TH.Dec]
mkForeignInstances ents = do
  let defs = foreignEnts ents
  concatForM defs $ \((f, t), pairss) ->
    case pairss of
      [] -> error "mkForeignInstances: Empty group"
      [pairs'] ->
        let foreignPairs' =
              [[|ForeignPair $(TH.conE $ TH.mkName x)
                             $(TH.conE $ TH.mkName y)
                |]
               | (x,y) <- pairs'
              ]
        in [d|
          instance ForeignKey $(TH.conT $ TH.mkName f)
                              $(TH.conT $ TH.mkName t) where
            foreignPairs = $(TH.listE foreignPairs')

           |]
      _ -> do
        TH.reportWarning
              $ concat [ "More than one possible Foreign instance for "
                       , f, " => ", t , ": \n"
                       , List.intercalate "\n"
                           . map ("      " <>) . for pairss $ \pairs' ->
                           List.intercalate ", " $ for pairs' $ \(f', t') ->
                             concat [f' , " -> ", t']
                       , "\n  Please create instances by hand"
                       ]
        return []
  where
    for = flip map
    concatForM xs f = concat <$> forM xs f
