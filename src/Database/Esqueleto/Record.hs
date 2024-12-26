{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Database.Esqueleto.Record
  ( deriveEsqueletoRecord
  , deriveEsqueletoRecordWith

  , DeriveEsqueletoRecordSettings(..)
  , defaultDeriveEsqueletoRecordSettings
  , takeColumns
  , takeMaybeColumns
  ) where

import Control.Monad.Trans.State.Strict (StateT(..), evalStateT)
import Data.Proxy (Proxy(..))
import Database.Esqueleto.Experimental
       (Entity, PersistValue, SqlExpr, Value(..), (:&)(..))
import Database.Esqueleto.Experimental.ToAlias (ToAlias(..))
import Database.Esqueleto.Experimental.ToMaybe (ToMaybe(..))
import Database.Esqueleto.Experimental.ToAliasReference (ToAliasReference(..))
import Database.Esqueleto.Internal.Internal (SqlSelect(..))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Bifunctor (first)
import Data.Text (Text)
import Control.Monad (forM)
import Data.Foldable (foldl')
import GHC.Exts (IsString(fromString))
import Data.Maybe (mapMaybe, fromMaybe, listToMaybe, isJust)

-- | Takes the name of a Haskell record type and creates a variant of that
-- record prefixed with @Sql@ which can be used in esqueleto expressions. This
-- reduces the amount of pattern matching on large tuples required to interact
-- with data extracted with esqueleto.
--
-- Note that because the input record and the @Sql@-prefixed record share field
-- names, the @{-\# LANGUAGE DuplicateRecordFields \#-}@ extension is required in
-- modules that use `deriveEsqueletoRecord`. Additionally, the @{-\# LANGUAGE
-- TypeApplications \#-}@ extension is required for some of the generated code.
--
-- Given the following record:
--
-- @
-- data MyRecord = MyRecord
--   { myName    :: 'Text'
--   , myAge     :: 'Maybe' 'Int'
--   , myUser    :: 'Entity' User
--   , myAddress :: 'Maybe' ('Entity' Address)
--   }
-- @
--
-- @$('deriveEsqueletoRecord' ''MyRecord)@ will generate roughly the following code:
--
-- @
-- data SqlMyRecord =
--   SqlMyRecord { myName    :: 'SqlExpr' ('Value' Text)
--               , myAge     :: 'SqlExpr' ('Value' ('Maybe' Int))
--               , myUser    :: 'SqlExpr' ('Entity' User)
--               , myAddress :: 'SqlExpr' ('Maybe' ('Entity' Address))
--               }
--
-- instance 'SqlSelect' SqlMyRecord MyRecord where
--   'sqlSelectCols'
--     identInfo
--     SqlMyRecord { myName    = myName
--                 , myAge     = myAge
--                 , myUser    = myUser
--                 , myAddress = myAddress
--                 } =
--     'sqlSelectCols' identInfo (myName :& myAge :& myUser :& myAddress)
--
--   'sqlSelectColCount' _ =
--     'sqlSelectColCount'
--       ('Proxy' \@(   ('SqlExpr' ('Value' Text))
--                :& ('SqlExpr' ('Value' ('Maybe' Int)))
--                :& ('SqlExpr' ('Entity' User))
--                :& ('SqlExpr' ('Maybe' ('Entity' Address)))))
--
--   'sqlSelectProcessRow' columns =
--     'first' (('fromString' "Failed to parse MyRecord: ") <>)
--           ('evalStateT' process columns)
--     where
--       process = do
--         'Value' myName <- 'takeColumns' \@('SqlExpr' ('Value' Text))
--         'Value' myAge  <- 'takeColumns' \@('SqlExpr' ('Value' ('Maybe' Int)))
--         myUser       <- 'takeColumns' \@('SqlExpr' ('Entity' User))
--         myAddress    <- 'takeColumns' \@('SqlExpr' ('Maybe' ('Entity' Address)))
--         'pure' MyRecord { myName = myName
--                       , myAge = myAge
--                       , myUser = myUser
--                       , myAddress = myAddress
--                       }
-- @
--
-- Then, we could write a selection function to use the record in queries:
--
-- @
-- getMyRecord :: 'Database.Esqueleto.SqlPersistT' 'IO' [MyRecord]
-- getMyRecord = 'Database.Esqueleto.Experimental.select' myRecordQuery
--
-- myRecordQuery :: 'Database.Esqueleto.SqlQuery' SqlMyRecord
-- myRecordQuery = do
--   user ':&' address <- 'Database.Esqueleto.Experimental.from' '$'
--     'Database.Esqueleto.Experimental.table' \@User
--       \`'Database.Esqueleto.Experimental.leftJoin'\`
--       'Database.Esqueleto.Experimental.table' \@Address
--       \`'Database.Esqueleto.Experimental.on'\` (do \\(user ':&' address) -> user 'Database.Esqueleto.Experimental.^.' #address 'Database.Esqueleto.Experimental.==.' address 'Database.Esqueleto.Experimental.?.' #id)
--   'pure'
--     SqlMyRecord
--       { myName = 'Database.Esqueleto.Experimental.castString' '$' user 'Database.Esqueleto.Experimental.^.' #firstName
--       , myAge = 'Database.Esqueleto.Experimental.val' 10
--       , myUser = user
--       , myAddress = address
--       }
-- @
--
-- @since 3.5.6.0
deriveEsqueletoRecord :: Name -> Q [Dec]
deriveEsqueletoRecord = deriveEsqueletoRecordWith defaultDeriveEsqueletoRecordSettings

-- | Codegen settings for 'deriveEsqueletoRecordWith'.
--
-- @since 3.5.8.0
data DeriveEsqueletoRecordSettings = DeriveEsqueletoRecordSettings
  { sqlNameModifier :: String -> String
    -- ^ Function applied to the Haskell record's type name and constructor
    -- name to produce the SQL record's type name and constructor name.
    --
    -- @since 3.5.8.0
  , sqlMaybeNameModifier :: String -> String
    -- ^ Function applied to the Haskell record's type name and constructor
    -- name to produce the 'ToMaybe' record's type name and constructor name.
    --
    -- @since 3.5.11.0
  , sqlFieldModifier :: String -> String
    -- ^ Function applied to the Haskell record's field names to produce the
    -- SQL record's field names.
    --
    -- @since 3.5.8.0
  , sqlMaybeFieldModifier :: String -> String
    -- ^ Function applied to the Haskell record's field names to produce the
    -- 'ToMaybe' SQL record's field names.
    --
    -- @since 3.5.11.0
  }

-- | The default codegen settings for 'deriveEsqueletoRecord'.
--
-- These defaults will cause you to require @{-# LANGUAGE DuplicateRecordFields #-}@
-- in certain cases (see 'deriveEsqueletoRecord'.) If you don't want to do this,
-- change the value of 'sqlFieldModifier' so the field names of the generated SQL
-- record different from those of the Haskell record.
--
-- @since 3.5.8.0
defaultDeriveEsqueletoRecordSettings :: DeriveEsqueletoRecordSettings
defaultDeriveEsqueletoRecordSettings = DeriveEsqueletoRecordSettings
  { sqlNameModifier = ("Sql" ++)
  , sqlMaybeNameModifier = ("SqlMaybe" ++)
  , sqlFieldModifier = id
  , sqlMaybeFieldModifier = id
  }

-- | Takes the name of a Haskell record type and creates a variant of that
-- record based on the supplied settings which can be used in esqueleto
-- expressions. This reduces the amount of pattern matching on large tuples
-- required to interact with data extracted with esqueleto.
--
-- This is a variant of 'deriveEsqueletoRecord' which allows you to avoid the
-- use of @{-# LANGUAGE DuplicateRecordFields #-}@, by configuring the
-- 'DeriveEsqueletoRecordSettings' used to generate the SQL record.
--
-- @since 3.5.8.0
deriveEsqueletoRecordWith :: DeriveEsqueletoRecordSettings -> Name -> Q [Dec]
deriveEsqueletoRecordWith settings originalName = do
  info <- getRecordInfo settings originalName
  -- It would be nicer to use `mconcat` here but I don't think the right
  -- instance is available in GHC 8.
  recordDec <- makeSqlRecord info
  sqlSelectInstanceDec <- makeSqlSelectInstance info
  sqlMaybeRecordDec <- makeSqlMaybeRecord info
  toMaybeInstanceDec <- makeToMaybeInstance info
  sqlMaybeToMaybeInstanceDec <- makeSqlMaybeToMaybeInstance info
  sqlMaybeRecordSelectInstanceDec <- makeSqlMaybeRecordSelectInstance info
  toAliasInstanceDec <- makeToAliasInstance info
  sqlMaybeToAliasInstanceDec <- makeSqlMaybeToAliasInstance info
  toAliasReferenceInstanceDec <- makeToAliasReferenceInstance info
  sqlMaybeToAliasReferenceInstanceDec <- makeSqlMaybeToAliasReferenceInstance info
  pure
    [ recordDec
    , sqlSelectInstanceDec
    , sqlMaybeRecordDec
    , toMaybeInstanceDec
    , sqlMaybeToMaybeInstanceDec
    , sqlMaybeRecordSelectInstanceDec
    , toAliasInstanceDec
    , sqlMaybeToAliasInstanceDec
    , toAliasReferenceInstanceDec
    , sqlMaybeToAliasReferenceInstanceDec
    ]

-- | Information about a record we need to generate the declarations.
-- We compute this once and then pass it around to save on complexity /
-- repeated work.
data RecordInfo = RecordInfo
  { -- | The original record's name.
    name :: Name
  , -- | The generated SQL record's name.
    sqlName :: Name
  , -- | The generated SQL 'ToMaybe' record's name.
    sqlMaybeName :: Name
  , -- | The original record's constraints. If this isn't empty it'll probably
    -- cause problems, but it's easy to pass around so might as well.
    constraints :: Cxt
  , -- | The original record's type-variable-binders.
#if MIN_VERSION_template_haskell(2,21,0)
    typeVarBinders :: [TyVarBndr BndrVis]
#elif MIN_VERSION_template_haskell(2,17,0)
    typeVarBinders :: [TyVarBndr ()]
#else
    typeVarBinders :: [TyVarBndr]
#endif
  , -- | The original record's kind, I think.
    kind :: Maybe Kind
  , -- | The original record's constructor name.
    constructorName :: Name
  , -- | The generated SQL record's constructor name.
    sqlConstructorName :: Name
  , -- | The generated SQL 'ToMaybe' record's constructor name.
    sqlMaybeConstructorName :: Name
  , -- | The original record's field names and types, derived from the
    -- constructors.
    fields :: [(Name, Type)]
  , -- | The generated SQL record's field names and types, computed
    -- with 'sqlFieldType'.
    sqlFields :: [(Name, Type)]
  , -- | The generated SQL 'ToMaybe' record's field names and types, computed
    -- with 'sqlMaybeFieldType'.
    sqlMaybeFields :: [(Name, Type)]
  }

-- | Get a `RecordInfo` instance for the given record name.
getRecordInfo :: DeriveEsqueletoRecordSettings -> Name -> Q RecordInfo
getRecordInfo settings name = do
  TyConI dec <- reify name
  (constraints, typeVarBinders, kind, constructors) <-
        case dec of
          DataD constraints' _name typeVarBinders' kind' constructors' _derivingClauses ->
            pure (constraints', typeVarBinders', kind', constructors')
          NewtypeD constraints' _name typeVarBinders' kind' constructor' _derivingClauses ->
            pure (constraints', typeVarBinders', kind', [constructor'])
          _ -> fail $ "Esqueleto records can only be derived for records and newtypes, but " ++ show name ++ " is neither"
  constructor <- case constructors of
                  (c : _) -> pure c
                  [] -> fail $ "Cannot derive Esqueleto record for a type with no constructors: " ++ show name
  let constructorName =
        case head constructors of
          RecC name' _fields -> name'
          con -> error $ nonRecordConstructorMessage con
      fields = getFields constructor
      sqlName = makeSqlName settings name
      sqlMaybeName = makeSqlMaybeName settings name
      sqlConstructorName = makeSqlName settings constructorName
      sqlMaybeConstructorName = makeSqlMaybeName settings constructorName

  sqlFields <- mapM toSqlField fields
  sqlMaybeFields <- mapM toSqlMaybeField fields

  pure RecordInfo {..}
  where
    getFields :: Con -> [(Name, Type)]
    getFields (RecC _name fields) = [(fieldName', fieldType') | (fieldName', _bang, fieldType') <- fields]
    getFields con = error $ nonRecordConstructorMessage con

    toSqlField (fieldName', ty) = do
      let modifier = mkName . sqlFieldModifier settings . nameBase
      sqlTy <- sqlFieldType ty
      pure (modifier fieldName', sqlTy)

    toSqlMaybeField (fieldName', ty) = do
      let modifier = mkName . sqlMaybeFieldModifier settings . nameBase
      sqlTy <- sqlMaybeFieldType ty
      pure (modifier fieldName', sqlTy)

-- | Create a new name by prefixing @Sql@ to a given name.
makeSqlName :: DeriveEsqueletoRecordSettings -> Name -> Name
makeSqlName settings name = mkName $ sqlNameModifier settings $ nameBase name

-- | Create a new name by prefixing @SqlMaybe@ to a given name.
makeSqlMaybeName :: DeriveEsqueletoRecordSettings -> Name -> Name
makeSqlMaybeName settings name = mkName $ sqlMaybeNameModifier settings $ nameBase name

-- | Transforms a record field type into a corresponding `SqlExpr` type.
--
-- * @'Entity' x@ is transformed into @'SqlExpr' ('Entity' x)@.
-- * @'Maybe' ('Entity' x)@ is transformed into @'SqlExpr' ('Maybe' ('Entity' x))@.
-- * @x@ is transformed into @'SqlExpr' ('Value' x)@.
-- * If there exists an instance @'SqlSelect' sql x@, then @x@ is transformed into @sql@.
--
-- This function should match `sqlSelectProcessRowPat`.
sqlFieldType :: Type -> Q Type
sqlFieldType fieldType = do
  maybeSqlType <- reifySqlSelectType fieldType

  pure $
    flip fromMaybe maybeSqlType $
      case fieldType of
        -- Entity x -> SqlExpr (Entity x)
        AppT (ConT ((==) ''Entity -> True)) _innerType -> AppT (ConT ''SqlExpr) fieldType

        -- Maybe (Entity x) -> SqlExpr (Maybe (Entity x))
        (ConT ((==) ''Maybe -> True))
          `AppT` ((ConT ((==) ''Entity -> True))
                  `AppT` _innerType) -> AppT (ConT ''SqlExpr) fieldType

        -- x -> SqlExpr (Value x)
        _ -> (ConT ''SqlExpr)
                `AppT` ((ConT ''Value)
                        `AppT` fieldType)

-- | Transforms a record field type into a corresponding `SqlExpr` `ToMaybe` type.
--
-- * @'Entity' x@ is transformed into @'SqlExpr' ('Maybe' ('Entity' x))@.
-- * @'Maybe' ('Entity' x)@ is transformed into @'SqlExpr' ('Maybe' ('Maybe' ('Entity' x)))@.
-- * @x@ is transformed into @'SqlExpr' ('Value' ('Maybe' x))@.
-- * If there exists an instance @'SqlSelect' sql x@, then @x@ is transformed into @sql@.
--
-- This function should match `sqlSelectProcessRowPat`.
sqlMaybeFieldType :: Type -> Q Type
sqlMaybeFieldType fieldType = do
  maybeSqlType <- reifySqlSelectType fieldType

  pure $ maybe convertFieldType convertSqlType maybeSqlType
 where
    convertSqlType = ((ConT ''ToMaybeT) `AppT`)
    convertFieldType = case fieldType of
        -- Entity x -> SqlExpr (Entity x) -> SqlExpr (Maybe (Entity x))
        AppT (ConT ((==) ''Entity -> True)) _innerType ->
          (ConT ''SqlExpr) `AppT` ((ConT ''Maybe) `AppT` fieldType)

        -- Maybe (Entity x) -> SqlExpr (Maybe (Entity x)) -> SqlExpr (Maybe (Entity x))
        (ConT ((==) ''Maybe -> True))
          `AppT` ((ConT ((==) ''Entity -> True))
                  `AppT` _innerType) ->
                    (ConT ''SqlExpr) `AppT` fieldType

        -- Maybe x -> SqlExpr (Value (Maybe x)) -> SqlExpr (Value (Maybe x))
        inner@((ConT ((==) ''Maybe -> True)) `AppT` _inner) -> (ConT ''SqlExpr) `AppT` ((ConT ''Value) `AppT` inner)

        -- x -> SqlExpr (Value x) -> SqlExpr (Value (Maybe x))
        _ -> (ConT ''SqlExpr)
                `AppT` ((ConT ''Value)
                        `AppT` ((ConT ''Maybe) `AppT` fieldType))

-- | Generates the declaration for an @Sql@-prefixed record, given the original
-- record's information.
makeSqlRecord :: RecordInfo -> Q Dec
makeSqlRecord RecordInfo {..} = do
  let newConstructor = RecC sqlConstructorName (makeField `map` sqlFields)
      derivingClauses = []
  pure $ DataD constraints sqlName typeVarBinders kind [newConstructor] derivingClauses
  where
    makeField (fieldName', fieldType) =
      (fieldName', Bang NoSourceUnpackedness NoSourceStrictness, fieldType)

-- | Generates an `SqlSelect` instance for the given record and its
-- @Sql@-prefixed variant.
makeSqlSelectInstance :: RecordInfo -> Q Dec
makeSqlSelectInstance info@RecordInfo {..} = do
  sqlSelectColsDec' <- sqlSelectColsDec info
  sqlSelectColCountDec' <- sqlSelectColCountDec info
  sqlSelectProcessRowDec' <- sqlSelectProcessRowDec info
  let overlap = Nothing
      instanceConstraints = []
  instanceType <- [t| SqlSelect $(conT sqlName) $(conT name) |]

  pure $ InstanceD overlap instanceConstraints instanceType (sqlSelectColsDec' ++ sqlSelectColCountDec' ++ [ sqlSelectProcessRowDec'])

-- | Generates the `sqlSelectCols` declaration for an `SqlSelect` instance.
sqlSelectColsDec :: RecordInfo -> Q [Dec]
sqlSelectColsDec RecordInfo {..} = do
  -- Pairs of record field names and local variable names.
  fieldNames <- forM sqlFields (\(name', _type) -> do
    var <- newName $ nameBase name'
    pure (name', var))

  -- Patterns binding record fields to local variables.
  let fieldPatterns :: [FieldPat]
      fieldPatterns = [(name', VarP var) | (name', var) <- fieldNames]

      -- Local variables for fields joined with `:&` in a single expression.
      joinedFields :: Exp
      joinedFields =
        case snd `map` fieldNames of
          [] -> TupE []
          [f1] -> VarE f1
          f1 : rest ->
            let helper lhs field =
                  InfixE
                    (Just lhs)
                    (ConE '(:&))
                    (Just $ VarE field)
             in foldl' helper (VarE f1) rest

  identInfo <- newName "identInfo"
  [d| $(varP 'sqlSelectCols) = \ $(varP identInfo) $(pure $ RecP sqlName fieldPatterns) ->
        sqlSelectCols $(varE identInfo) $(pure joinedFields)
    |]

-- | Generates the `sqlSelectColCount` declaration for an `SqlSelect` instance.
sqlSelectColCountDec :: RecordInfo -> Q [Dec]
sqlSelectColCountDec RecordInfo {..} = do
  let joinedTypes =
        case snd `map` sqlFields of
          [] -> TupleT 0
          t1 : rest ->
            let helper lhs ty =
                  InfixT lhs ''(:&) ty
             in foldl' helper t1 rest

  [d| $(varP 'sqlSelectColCount) = \ _ -> sqlSelectColCount (Proxy @($(pure joinedTypes))) |]

-- | Generates the `sqlSelectProcessRow` declaration for an `SqlSelect`
-- instance.
sqlSelectProcessRowDec :: RecordInfo -> Q Dec
sqlSelectProcessRowDec RecordInfo {..} = do
  -- Binding statements and field expressions (used in record construction) to
  -- fill out the body of the main generated `do` expression.
  --
  -- Each statement is like:
  --     Value fooName' <- takeColumns @(SqlExpr (Value Text))
  -- A corresponding field expression would be:
  --     fooName = fooName'
  --
  -- See `sqlSelectProcessRowPat` for the left-hand side of the patterns.
  (statements, fieldExps) <-
    unzip <$> forM (zip fields sqlFields) (\((fieldName', fieldType), (_, sqlType')) -> do
      valueName <- newName (nameBase fieldName')
      pattern <- sqlSelectProcessRowPat fieldType valueName
      pure
        ( BindS
            pattern
            (AppTypeE (VarE 'takeColumns) sqlType')
        , (mkName $ nameBase fieldName', VarE valueName)
        ))

  colsName <- newName "columns"
  processName <- newName "process"

  -- Roughly:
  -- sqlSelectProcessRow $colsName =
  --   first ((fromString "Failed to parse $name: ") <>)
  --         (evalStateT $processName $colsName)
  --   where $processName = do $statements
  --                           pure $name {$fieldExps}
  bodyExp <- [e|
    first (fromString ("Failed to parse " ++ $(lift $ nameBase name) ++ ": ") <>)
          (evalStateT $(varE processName) $(varE colsName))
    |]

  pure $
    FunD
      'sqlSelectProcessRow
      [ Clause
          [VarP colsName]
          (NormalB bodyExp)
          -- `where` clause
          [ ValD
              (VarP processName)
              ( NormalB $
                  DoE
#if MIN_VERSION_template_haskell(2,17,0)
                    Nothing
#endif
                    (statements ++ [NoBindS $ AppE (VarE 'pure) (RecConE constructorName fieldExps)])
              )
              []
          ]
      ]

-- | Get the left-hand side pattern of a statement in a @do@ block for binding
-- to the result of `sqlSelectProcessRow`.
--
-- * A type of @'Entity' x@ gives a pattern of @var@.
-- * A type of @'Maybe' ('Entity' x)@ gives a pattern of @var@.
-- * A type of @x@ gives a pattern of @'Value' var@.
-- * If there exists an instance @'SqlSelect' sql x@, then a type of @x@ gives a pattern of @var@.
--
-- This function should match `sqlFieldType`.
sqlSelectProcessRowPat :: Type -> Name -> Q Pat
sqlSelectProcessRowPat fieldType var = do
  maybeSqlType <- reifySqlSelectType fieldType

  case maybeSqlType of
    Just _ -> pure $ VarP var
    Nothing -> case fieldType of
        -- Entity x -> var
        AppT (ConT ((==) ''Entity -> True)) _innerType -> pure $ VarP var
        -- Maybe (Entity x) -> var
        (ConT ((==) ''Maybe -> True))
          `AppT` ((ConT ((==) ''Entity -> True))
                  `AppT` _innerType) -> pure $ VarP var
        -- x -> Value var
        _ -> [p| Value $(varP var) |]

-- Given a type, find the corresponding SQL type.
--
-- If there exists an instance `SqlSelect sql ty`, then the SQL type for `ty`
-- is `sql`.
--
-- This function definitely works for records and instances generated by this
-- module, and might work for instances outside of it.
reifySqlSelectType :: Type -> Q (Maybe Type)
reifySqlSelectType originalType = do
  -- Here we query the compiler for Instances of `SqlSelect a $(originalType)`;
  -- the API for this is super weird, it interprets a list of types as being
  -- applied as successive arguments to the typeclass name.
  --
  -- See: https://gitlab.haskell.org/ghc/ghc/-/issues/21825
  --
  -- >>> reifyInstances ''SqlSelect [VarT (mkName "a"), ConT ''MyRecord]
  -- [ InstanceD Nothing
  --             []
  --             (AppT (AppT (ConT Database.Esqueleto.Internal.Internal.SqlSelect)
  --                         (ConT Ghci3.SqlMyRecord))
  --                   (ConT Ghci3.MyRecord))
  --             []
  -- ]
  tyVarName <- newName "a"
  instances <- reifyInstances ''SqlSelect [VarT tyVarName, originalType]

  -- Given the original type (`originalType`) and an instance type for a
  -- `SqlSelect` instance, get the SQL type which corresponds to the original
  -- type.
  let extractSqlRecord :: Type -> Type -> Maybe Type
      extractSqlRecord originalTy instanceTy =
        case instanceTy of
          (ConT ((==) ''SqlSelect -> True))
            `AppT` sqlTy
            `AppT` ((==) originalTy -> True) -> Just sqlTy
          _ -> Nothing

      -- Filter `instances` to the instances which match `originalType`.
      filteredInstances :: [Type]
      filteredInstances =
        flip mapMaybe instances
          (\case InstanceD _overlap
                           _constraints
                           (extractSqlRecord originalType -> Just sqlRecord)
                           _decs ->
                             Just sqlRecord
                 _ -> Nothing)

  pure $ listToMaybe filteredInstances

-- | Statefully parse some number of columns from a list of `PersistValue`s,
-- where the number of columns to parse is determined by `sqlSelectColCount`
-- for @a@.
--
-- This is used to implement `sqlSelectProcessRow` for records created with
-- `deriveEsqueletoRecord`.
takeColumns ::
  forall a b.
  SqlSelect a b =>
  StateT [PersistValue] (Either Text) b
takeColumns = StateT (\pvs ->
  let targetColCount =
        sqlSelectColCount (Proxy @a)
      (target, other) =
        splitAt targetColCount pvs
   in if length target == targetColCount
        then do
          value <- sqlSelectProcessRow target
          Right (value, other)
        else Left "Insufficient columns when trying to parse a column")

-- | Get an error message for a non-record constructor.
-- This module does not yet support non-record constructors, so we'll tell the
-- user what sort of constructor they provided that we can't use, along with
-- the name of that constructor. This turns out to require recursion, but you
-- can't win every battle.
nonRecordConstructorMessage :: Con -> String
nonRecordConstructorMessage con =
  case con of
    (RecC {}) -> error "Record constructors are not an error"
    (NormalC {}) -> helper "non-record data constructor"
    (InfixC {}) -> helper "infix constructor"
    (ForallC {}) -> helper "constructor qualified by type variables / class contexts"
    (GadtC {}) -> helper "GADT constructor"
    (RecGadtC {}) -> helper "record GADT constructor"
  where
    helper constructorType =
      "Esqueleto records can only be derived for record constructors, but "
        ++ show (constructorName con)
        ++ " is a "
        ++ constructorType

    constructorName constructor =
      case constructor of
        (RecC name _) -> name
        (NormalC name _fields) -> name
        (InfixC _ty1 name _ty2) -> name
        (ForallC _vars _constraints innerConstructor) -> constructorName innerConstructor
        -- If there's GADTs where multiple constructors are declared with the
        -- same type signature you're evil and furthermore this diagnostic will
        -- only show you the first name.
        (GadtC names _fields _ret) -> head names
        (RecGadtC names _fields _ret) -> head names

makeToAliasInstance :: RecordInfo -> Q Dec
makeToAliasInstance RecordInfo {..} = makeToAliasInstanceFor sqlName sqlFields

makeSqlMaybeToAliasInstance :: RecordInfo -> Q Dec
makeSqlMaybeToAliasInstance RecordInfo {..} = makeToAliasInstanceFor sqlMaybeName sqlMaybeFields

makeToAliasInstanceFor :: Name -> [(Name, Type)] -> Q Dec
makeToAliasInstanceFor name fields = do
  toAliasDec' <- toAliasDec name fields
  let overlap = Nothing
      instanceConstraints = []
      instanceType = (ConT ''ToAlias) `AppT` (ConT name)
  pure $ InstanceD overlap instanceConstraints instanceType [toAliasDec']

toAliasDec :: Name -> [(Name, Type)] -> Q Dec
toAliasDec name fields = do
  (statements, fieldPatterns, fieldExps) <-
    unzip3 <$> forM fields (\(fieldName', _) -> do
      fieldPatternName <- newName (nameBase fieldName')
      boundValueName <- newName (nameBase fieldName')
      pure
        ( BindS
            (VarP boundValueName)
            (VarE 'toAlias `AppE` VarE fieldPatternName)
        , (fieldName', VarP fieldPatternName)
        , (fieldName', VarE boundValueName)
        ))

  pure $
    FunD
      'toAlias
      [ Clause
          [ RecP name fieldPatterns
          ]
          ( NormalB $
              DoE
#if MIN_VERSION_template_haskell(2,17,0)
                Nothing
#endif
                (statements ++ [NoBindS $ AppE (VarE 'pure) (RecConE name fieldExps)])
          )
          -- `where` clause.
          []
      ]

makeToAliasReferenceInstance :: RecordInfo -> Q Dec
makeToAliasReferenceInstance RecordInfo {..} = makeToAliasReferenceInstanceFor sqlName sqlFields

makeSqlMaybeToAliasReferenceInstance :: RecordInfo -> Q Dec
makeSqlMaybeToAliasReferenceInstance RecordInfo {..} =
  makeToAliasReferenceInstanceFor sqlMaybeName sqlMaybeFields

makeToAliasReferenceInstanceFor :: Name -> [(Name, Type)] -> Q Dec
makeToAliasReferenceInstanceFor name fields = do
  toAliasReferenceDec' <- toAliasReferenceDec name fields
  let overlap = Nothing
      instanceConstraints = []
      instanceType = (ConT ''ToAliasReference) `AppT` (ConT name)
  pure $ InstanceD overlap instanceConstraints instanceType [toAliasReferenceDec']

toAliasReferenceDec :: Name -> [(Name, Type)] -> Q Dec
toAliasReferenceDec name fields = do
  identInfo <- newName "identInfo"

  (statements, fieldPatterns, fieldExps) <-
    unzip3 <$> forM fields (\(fieldName', _) -> do
      fieldPatternName <- newName (nameBase fieldName')
      boundValueName <- newName (nameBase fieldName')
      pure
        ( BindS
            (VarP boundValueName)
            (VarE 'toAliasReference `AppE` VarE identInfo `AppE` VarE fieldPatternName)
        , (fieldName', VarP fieldPatternName)
        , (fieldName', VarE boundValueName)
        ))

  pure $
    FunD
      'toAliasReference
      [ Clause
          [ VarP identInfo
          , RecP name fieldPatterns
          ]
          ( NormalB $
              DoE
#if MIN_VERSION_template_haskell(2,17,0)
                Nothing
#endif
                (statements ++ [NoBindS $ AppE (VarE 'pure) (RecConE name fieldExps)])
          )
          -- `where` clause.
          []
      ]

-- | Generates the declaration for an @SqlMaybe@-prefixed record, given the original
-- record's information.
makeSqlMaybeRecord :: RecordInfo -> Q Dec
makeSqlMaybeRecord  RecordInfo {..} = do
  let newConstructor = RecC sqlMaybeConstructorName (makeField `map` sqlMaybeFields)
      derivingClauses = []
  pure $ DataD constraints sqlMaybeName typeVarBinders kind [newConstructor] derivingClauses
  where
    makeField (fieldName', fieldType) =
      (fieldName', Bang NoSourceUnpackedness NoSourceStrictness, fieldType)


-- | Generates a `ToMaybe` instance for the given record.
makeToMaybeInstance :: RecordInfo -> Q Dec
makeToMaybeInstance info@RecordInfo {..} = do
  toMaybeTDec' <- toMaybeTDec sqlName sqlMaybeName 
  toMaybeDec' <- toMaybeDec info
  let overlap = Nothing
      instanceConstraints = []
      instanceType = (ConT ''ToMaybe) `AppT` (ConT sqlName)

  pure $ InstanceD overlap instanceConstraints instanceType (toMaybeTDec' ++ toMaybeDec')

-- | Generates a `ToMaybe` instance for the SqlMaybe of the given record.
makeSqlMaybeToMaybeInstance :: RecordInfo -> Q Dec
makeSqlMaybeToMaybeInstance RecordInfo {..} = do
  sqlMaybeToMaybeTDec' <- toMaybeTDec sqlMaybeName sqlMaybeName
  let toMaybeIdDec = FunD 'toMaybe [ Clause [] (NormalB (VarE 'id)) []]
      overlap = Nothing
      instanceConstraints = []
      instanceType = (ConT ''ToMaybe) `AppT` (ConT sqlMaybeName)
  pure $ InstanceD overlap instanceConstraints instanceType (toMaybeIdDec:sqlMaybeToMaybeTDec')

-- | Generates a `type ToMaybeT ... = ...` declaration for the given names.
toMaybeTDec :: Name -> Name -> Q [Dec]
toMaybeTDec nameLeft nameRight =
  [d| type instance ToMaybeT $(conT nameLeft) = $(conT nameRight) |]

-- | Generates a `toMaybe value = ...` declaration for the given record.
toMaybeDec :: RecordInfo -> Q [Dec]
toMaybeDec RecordInfo {..} = do
  (fieldPatterns, fieldExps) <-
    unzip <$> forM (zip sqlFields sqlMaybeFields) (\((fieldName', _), (maybeFieldName', _)) -> do
        fieldPatternName <- newName (nameBase fieldName')
        pure
            ( (fieldName', VarP fieldPatternName)
            , (maybeFieldName', VarE 'toMaybe `AppE` VarE fieldPatternName)
            ))

  [d| $(varP 'toMaybe) = \ $(pure $ RecP sqlName fieldPatterns) ->
        $(pure $ RecConE sqlMaybeName fieldExps)
    |]

-- | Generates an `SqlSelect` instance for the given record and its
-- @Sql@-prefixed variant.
makeSqlMaybeRecordSelectInstance :: RecordInfo -> Q Dec
makeSqlMaybeRecordSelectInstance info@RecordInfo {..} = do
  sqlSelectColsDec' <- sqlMaybeSelectColsDec info
  sqlSelectColCountDec' <- sqlMaybeSelectColCountDec info
  sqlSelectProcessRowDec' <- sqlMaybeSelectProcessRowDec info
  let overlap = Nothing
      instanceConstraints = []
  instanceType <- [t| SqlSelect $(conT sqlMaybeName) (Maybe $(conT name)) |]
  pure $ InstanceD overlap instanceConstraints instanceType (sqlSelectColsDec' ++ sqlSelectColCountDec' ++ [sqlSelectProcessRowDec'])

-- | Generates the `sqlSelectCols` declaration for an `SqlSelect` instance.
sqlMaybeSelectColsDec :: RecordInfo -> Q [Dec]
sqlMaybeSelectColsDec RecordInfo {..} = do
  -- Pairs of record field names and local variable names.
  fieldNames <- forM sqlMaybeFields (\(name', _type) -> do
    var <- newName $ nameBase name'
    pure (name', var))

  -- Patterns binding record fields to local variables.
  let fieldPatterns :: [FieldPat]
      fieldPatterns = [(name', VarP var) | (name', var) <- fieldNames]

      -- Local variables for fields joined with `:&` in a single expression.
      joinedFields :: Exp
      joinedFields =
        case snd `map` fieldNames of
          [] -> TupE []
          [f1] -> VarE f1
          f1 : rest ->
            let helper lhs field =
                  InfixE
                    (Just lhs)
                    (ConE '(:&))
                    (Just $ VarE field)
             in foldl' helper (VarE f1) rest

  identInfo <- newName "identInfo"
  [d| $(varP 'sqlSelectCols) = \ $(varP identInfo) $(pure $ RecP sqlMaybeName fieldPatterns) ->
        sqlSelectCols $(varE identInfo) $(pure joinedFields)
    |]

-- | Generates the `sqlSelectProcessRow` declaration for an `SqlSelect`
-- instance for a SqlMaybe.
sqlMaybeSelectProcessRowDec :: RecordInfo -> Q Dec
sqlMaybeSelectProcessRowDec RecordInfo {..} = do
  -- See sqlSelectProcessRowDec, which is similar but does not have special handling for Maybe
  (statements, fieldExps) <-
    unzip <$> forM (zip fields sqlMaybeFields) (\((fieldName', fieldType), (_, sqlType')) -> do
      valueName <- newName (nameBase fieldName')
      pattern <- sqlSelectProcessRowPat fieldType valueName
      pure
        ( BindS
            pattern
            (AppTypeE (VarE 'takeColumns) sqlType')
        , (valueName, wrapJust fieldType $ VarE valueName)
        ))

  colsName <- newName "columns"
  processName <- newName "process"

  bodyExp <- [e|
    first (fromString ("Failed to parse " ++ $(lift $ nameBase sqlMaybeName) ++ ": ") <>)
          (evalStateT $(varE processName) $(varE colsName))
    |]

  pure $
    FunD
      'sqlSelectProcessRow
      [ Clause
          [VarP colsName]
          (NormalB bodyExp)
          -- `where`
          [ ValD
              (VarP processName)
              (NormalB $
                  DoE
#if MIN_VERSION_template_haskell(2,17,0)
                    Nothing
#endif
                    (statements ++ [
                      NoBindS $ AppE (VarE 'pure) (
                        CondE
                          (AppE
                            (VarE 'or)
                            (ListE $ fmap (\(n, _) -> AppE (VarE 'isJust) (VarE n)) fieldExps))
                          (case snd <$> fieldExps of
                              [] -> ConE constructorName
                              x:xs ->  foldl'
                                (\a b -> InfixE (Just a) (VarE '(<*>)) (Just b))
                                (InfixE (Just $ ConE constructorName) (VarE '(<$>)) (Just x))
                                xs)
                          (ConE 'Nothing)
                        )
                      ]
                    )
              )
              []
          ]
      ]
  where
    wrapJust x = case x of
      ((ConT ((==) ''Maybe -> True)) `AppT` _inner) -> AppE (ConE 'Just)
      _ -> id

-- | Generates the `sqlSelectColCount` declaration for an `SqlSelect` instance.
sqlMaybeSelectColCountDec :: RecordInfo -> Q [Dec]
sqlMaybeSelectColCountDec RecordInfo {..} = do
  let joinedTypes =
        case snd `map` sqlMaybeFields of
          [] -> TupleT 0
          t1 : rest ->
            let helper lhs ty =
                  InfixT lhs ''(:&) ty
             in foldl' helper t1 rest

  [d| $(varP 'sqlSelectColCount) = \_ -> sqlSelectColCount (Proxy @($(pure joinedTypes))) |]

-- | Statefully parse some number of columns from a list of `PersistValue`s,
-- where the number of columns to parse is determined by `sqlSelectColCount`
-- for @a@.
--
-- This is used to implement `sqlSelectProcessRow` for records created with
-- `deriveEsqueletoRecord`.
takeMaybeColumns ::
  forall a b.
  (SqlSelect a (ToMaybeT b)) =>
  StateT [PersistValue] (Either Text) (ToMaybeT b)
takeMaybeColumns = StateT (\pvs ->
  let targetColCount =
        sqlSelectColCount (Proxy @a)
      (target, other) =
        splitAt targetColCount pvs
   in if length target == targetColCount
        then do
          value <- sqlSelectProcessRow target
          Right (value, other)
        else Left "Insufficient columns when trying to parse a column")
