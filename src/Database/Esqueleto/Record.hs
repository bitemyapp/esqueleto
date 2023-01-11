{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
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
    , projectMaybeRecord
    , getFieldP
    ) where

import GHC.Records
import Data.Typeable
import Database.Esqueleto.Experimental.ToMaybe
import Control.Monad.Trans.State.Strict (StateT(..), evalStateT)
import Data.Proxy (Proxy(..))
import Database.Esqueleto.Experimental
       (just, Entity, PersistValue, SqlExpr, Value(..), (:&)(..))
import Database.Esqueleto.Experimental.ToAlias (ToAlias(..))
import Database.Esqueleto.Experimental.ToAliasReference (ToAliasReference(..))
import Database.Esqueleto.Internal.Internal (SqlSelect(..), nullsFor, noMeta, SqlExpr(..))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad (forM)
import Data.Foldable (foldl')
import GHC.Exts (IsString(fromString))
import Data.Maybe (mapMaybe, fromMaybe, listToMaybe)

-- | Takes the name of a Haskell record type and creates a variant of that
-- record prefixed with @Sql@ which can be used in esqueleto expressions. This
-- reduces the amount of pattern matching on large tuples required to interact
-- with data extracted with esqueleto.
--
-- Note that because the input record and the @Sql@-prefixed record share field
-- names, the @{-# LANGUAGE DuplicateRecordFields #-}@ extension is required in
-- modules that use `deriveEsqueletoRecord`. Additionally, the @{-# LANGUAGE
-- TypeApplications #-}@ extension is required for some of the generated code.
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
--               , myAge     :: 'SqlExpr' ('Value' Int)
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
--                :& ('SqlExpr' ('Value' Int))
--                :& ('SqlExpr' ('Entity' User))
--                :& ('SqlExpr' ('Maybe' ('Entity' Address)))))
--
--   'sqlSelectProcessRow' columns =
--     'first' (('fromString' "Failed to parse MyRecord: ") <>)
--           ('evalStateT' process columns)
--     where
--       process = do
--         'Value' myName <- 'takeColumns' \@('SqlExpr' ('Value' Text))
--         'Value' myAge  <- 'takeColumns' \@('SqlExpr' ('Value' Int))
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
  , sqlFieldModifier :: String -> String
    -- ^ Function applied to the Haskell record's field names to produce the
    -- SQL record's field names.
    --
    -- @since 3.5.8.0
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
  , sqlFieldModifier = id
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
    toAliasInstanceDec <- makeToAliasInstance info
    let sqlNameTyp = conT (sqlName info)
        nameTyp = conT (name info)
        -- sym = varT (mkName "sym")
    maybeInstances <-
        [d|
        instance SqlSelect (Maybe $(sqlNameTyp)) (Maybe $(nameTyp)) where
            sqlSelectProcessRow = sqlSelectProcessRowOptional
            sqlSelectColCount = sqlSelectColCount . unMaybeProxy
            sqlSelectCols = $(sqlSelectColsExpMaybe info)

        instance ToAlias (Maybe $(sqlNameTyp)) where
            toAlias = traverse toAlias

        instance ToAliasReference (Maybe $(sqlNameTyp)) where
            toAliasReference aliasSource = traverse (toAliasReference aliasSource)

        instance ToMaybe $(sqlNameTyp) where
            type ToMaybeT $(sqlNameTyp) = Maybe $(sqlNameTyp)
            toMaybe = Just

        instance HasNulls $(sqlNameTyp) where
            mkNothing _ = Nothing
                |]


    maybeHasFieldInstances <- fmap mconcat $ forM (sqlFields info) $ \(sqlFieldName, sqlFieldType) -> do
        nm <- newName (show sqlFieldName)
        let binding =
                LamE [RecP (sqlName info) [ (sqlFieldName, VarP nm) ]] (VarE nm)
        [d|
            instance (r ~ ToMaybeT $(pure sqlFieldType) ) => HasField $(litT (strTyLit $ show sqlFieldName)) (Maybe $(sqlNameTyp)) r where
                    getField mrec =
                        projectMaybeRecord mrec $(pure binding)
            |]

    toAliasReferenceInstanceDec <- makeToAliasReferenceInstance info
    pure $
        [ recordDec
        , sqlSelectInstanceDec
        , toAliasInstanceDec
        , toAliasReferenceInstanceDec
        ] <> maybeInstances <> maybeHasFieldInstances

-- | Information about a record we need to generate the declarations.
-- We compute this once and then pass it around to save on complexity /
-- repeated work.
data RecordInfo = RecordInfo
  { -- | The original record's name.
    name :: Name
  , -- | The generated SQL record's name.
    sqlName :: Name
  , -- | The original record's constraints. If this isn't empty it'll probably
    -- cause problems, but it's easy to pass around so might as well.
    constraints :: Cxt
  , -- | The original record's type-variable-binders.
#if MIN_VERSION_template_haskell(2,17,0)
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
  , -- | The original record's field names and types, derived from the
    -- constructors.
    fields :: [(Name, Type)]
  , -- | The generated SQL record's field names and types, computed
    -- with 'sqlFieldType'.
    sqlFields :: [(Name, Type)]
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
      sqlConstructorName = makeSqlName settings constructorName

  sqlFields <- mapM toSqlField fields

  pure RecordInfo {..}
  where
    getFields :: Con -> [(Name, Type)]
    getFields (RecC _name fields) = [(fieldName', fieldType') | (fieldName', _bang, fieldType') <- fields]
    getFields con = error $ nonRecordConstructorMessage con

    toSqlField (fieldName', ty) = do
      let modifier = mkName . sqlFieldModifier settings . nameBase
      sqlTy <- sqlFieldType ty
      pure (modifier fieldName', sqlTy)

-- | Create a new name by prefixing @Sql@ to a given name.
makeSqlName :: DeriveEsqueletoRecordSettings -> Name -> Name
makeSqlName settings name = mkName $ sqlNameModifier settings $ nameBase name

-- | Transforms a record field type into a corresponding `SqlExpr` type.
--
-- * If there exists an instance @'SqlSelect' sql x@, then @x@ is transformed into @sql@.
-- * @'Entity' x@ is transformed into @'SqlExpr' ('Entity' x)@.
-- * @'Maybe' ('Entity' x)@ is transformed into @'SqlExpr' ('Maybe' ('Entity' x))@.
-- * @x@ is transformed into @'SqlExpr' ('Value' x)@.
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

applyMaybeInsideValue :: Type -> Type
applyMaybeInsideValue typ =
    fromMaybe typ $ do
        inner <- takeValueType typ
        let mkValue = AppT (ConT ''Value)
            mkMaybe = AppT (ConT ''Maybe)
        pure $ mkValue (mkMaybe inner)

takeTypeConstructorApplicationName :: Name -> Type -> Maybe Type
takeTypeConstructorApplicationName nm typ = do
    AppT (ConT ((==) nm -> True)) rest <- Just typ
    pure rest

takeValueType :: Type -> Maybe Type
takeValueType =
    takeTypeConstructorApplicationName ''Value

takeEntityType :: Type -> Maybe Type
takeEntityType =
    takeTypeConstructorApplicationName ''Entity

takeMaybeType :: Type -> Maybe Type
takeMaybeType =
    takeTypeConstructorApplicationName ''Maybe

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
      instanceType =
        (ConT ''SqlSelect)
          `AppT` (ConT sqlName)
          `AppT` (ConT name)

  pure $ InstanceD overlap instanceConstraints instanceType [sqlSelectColsDec', sqlSelectColCountDec', sqlSelectProcessRowDec']

makeSqlSelectMaybeInstance :: RecordInfo -> Q [Dec]
makeSqlSelectMaybeInstance info@RecordInfo {..} = do
    sqlTypeT <- [t| Maybe $(conT sqlName) |]
    regularTypeT <- [t| Maybe $(conT name) |]
    [d|
        instance SqlSelect (Maybe $(conT sqlName)) (Maybe $(conT name)) where
            sqlSelectProcessRow = sqlSelectProcessRowOptional
            sqlSelectColCount = sqlSelectColCount . unMaybeProxy
            sqlSelectCols = $(sqlSelectColsExpMaybe info)
        |]

-- | Generates the `sqlSelectCols` declaration for an `SqlSelect`  for a 'Maybe'
-- of a record.
--
-- If the record is present, then we delegate to the underlying functions. If
-- the record is absent, then we summon up sufficient @NULL@ values to account
-- for each column using 'nullsFor'.
sqlSelectColsExpMaybe :: RecordInfo -> Q Exp
sqlSelectColsExpMaybe RecordInfo {..} = do
    let mkNullForType typ =
            [e|nullsFor (Proxy @( $(pure typ) )) |]
        nullFields =
            map (mkNullForType . snd) $ sqlFields

    [e|
            maybe (uncommas' $(listE nullFields)) . sqlSelectCols
        |]

-- | Generates the `sqlSelectCols` declaration for an `SqlSelect` instance.
sqlSelectColsDec :: RecordInfo -> Q Dec
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
  -- Roughly:
  -- sqlSelectCols $identInfo SqlFoo{..} = sqlSelectCols $identInfo $joinedFields
  pure $
    FunD
      'sqlSelectCols
      [ Clause
          [ VarP identInfo
          , RecP sqlName fieldPatterns
          ]
          ( NormalB $
              (VarE 'sqlSelectCols)
                `AppE` (VarE identInfo)
                `AppE` (ParensE joinedFields)
          )
          -- `where` clause.
          []
      ]

unMaybeProxy :: Proxy (Maybe a) -> Proxy a
unMaybeProxy _ = Proxy

-- | Generates the `sqlSelectColCount` declaration for an `SqlSelect` instance.
sqlSelectColCountDec :: RecordInfo -> Q Dec
sqlSelectColCountDec RecordInfo {..} = do
  let joinedTypes =
        case snd `map` sqlFields of
          [] -> TupleT 0
          t1 : rest ->
            let helper lhs ty =
                  InfixT lhs ''(:&) ty
             in foldl' helper t1 rest

  -- Roughly:
  -- sqlSelectColCount _ = sqlSelectColCount (Proxy @($joinedTypes))
  pure $
    FunD
      'sqlSelectColCount
      [ Clause
          [WildP]
          ( NormalB $
              AppE (VarE 'sqlSelectColCount) $
                ParensE $
                  AppTypeE
                    (ConE 'Proxy)
                    joinedTypes
          )
          -- `where` clause.
          []
      ]

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
#if MIN_VERSION_template_haskell(2,18,0)
        _ -> pure $ ConP 'Value [] [VarP var]
#else
        _ -> pure $ ConP 'Value [VarP var]
#endif

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
takeColumns = StateT $ \pvs -> do
    let targetColCount =
            sqlSelectColCount (Proxy @a)
        (target, other) =
            splitAt targetColCount pvs
        targetLength =
            length target
    if targetLength == targetColCount
        then do
            value <- sqlSelectProcessRow target
            pure (value, other)
        else
            Left $ mconcat
                [ "Insufficient columns when trying to parse a column: "
                , "Expected ", tshow targetColCount, " columns, but got: "
                , tshow targetLength, ".\n\n"
                , "Columns:\n\t", tshow target
                , "Other:\n\t", tshow other
                ]

tshow :: Show a => a -> Text
tshow = Text.pack . show

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
makeToAliasInstance info@RecordInfo {..} = do
  toAliasDec' <- toAliasDec info
  let overlap = Nothing
      instanceConstraints = []
      instanceType =
        (ConT ''ToAlias)
          `AppT` (ConT sqlName)
  pure $ InstanceD overlap instanceConstraints instanceType [toAliasDec']

toAliasDec :: RecordInfo -> Q Dec
toAliasDec RecordInfo {..} = do
  (statements, fieldPatterns, fieldExps) <-
    unzip3 <$> forM sqlFields (\(fieldName', _) -> do
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
          [ RecP sqlName fieldPatterns
          ]
          ( NormalB $
              DoE
#if MIN_VERSION_template_haskell(2,17,0)
                Nothing
#endif
                (statements ++ [NoBindS $ AppE (VarE 'pure) (RecConE sqlName fieldExps)])
          )
          -- `where` clause.
          []
      ]

makeToAliasReferenceInstance :: RecordInfo -> Q Dec
makeToAliasReferenceInstance info@RecordInfo {..} = do
  toAliasReferenceDec' <- toAliasReferenceDec info
  let overlap = Nothing
      instanceConstraints = []
      instanceType =
        (ConT ''ToAliasReference)
          `AppT` (ConT sqlName)
  pure $ InstanceD overlap instanceConstraints instanceType [toAliasReferenceDec']

toAliasReferenceDec :: RecordInfo -> Q Dec
toAliasReferenceDec RecordInfo {..} = do
  identInfo <- newName "identInfo"

  (statements, fieldPatterns, fieldExps) <-
    unzip3 <$> forM sqlFields (\(fieldName', _) -> do
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
          , RecP sqlName fieldPatterns
          ]
          ( NormalB $
              DoE
#if MIN_VERSION_template_haskell(2,17,0)
                Nothing
#endif
                (statements ++ [NoBindS $ AppE (VarE 'pure) (RecConE sqlName fieldExps)])
          )
          -- `where` clause.
          []
      ]

sqlSelectProcessRowOptional
    :: forall r a. (Typeable a, Typeable r, SqlSelect a r)
    => [PersistValue]
    -> Either Text (Maybe r)
sqlSelectProcessRowOptional pvs =
    case sqlSelectProcessRow pvs of
        Left err -> do
            let actualLength =
                    length pvs
                expectedLength =
                    sqlSelectColCount (Proxy :: Proxy a)

            if actualLength == expectedLength
            then pure Nothing -- assuming that the problem is an "unexpected null"
            else Left $ mconcat
                [ "Column count incorrect: expected "
                , tshow expectedLength
                , " but got ", tshow actualLength," when trying to parse a "
                , Text.pack (show (typeRep (Proxy @r))), " from a "
                , Text.pack (show (typeRep (Proxy @a)))
                , ".\nGiven error: ", err
                ]
        Right a ->
            pure (Just a)


{- Uh oh...

So, we start with a record.

    data X = X { x :: Int }

Then we turn that into a SQL record.

    data SqlX = SqlX { x :: SqlExpr (Value Int) }

With `OverloadedRecordDot`, you can refer to `sqlX.x` and get back what you
want.

You can carry `Maybe`, `Entity`, and other records, and it works out fine.

However, we need *something* for dealing with a nullable `SqlX`. Otherwise, we
cannot bring them into scope from left joins.

My first attempt used `Maybe`. The nice thing here is that we get a totally
reasonable implementation, right up until we need to do field access. A first
attempt is something like:

    fmap (\SqlMyRecord {..} -> myName) myRecord

However, this gives us a @Maybe (SqlExpr (Value Text))@, and not a @SqlExpr
(Value (Maybe Text))@.

Consider the type of the maybe projection operator:

    (?.) :: ( PersistEntity val , PersistField typ)
        => SqlExpr (Maybe (Entity val))
        -> EntityField val typ
        -> SqlExpr (Value (Maybe typ))

`SqlExpr (Maybe (Entity val))` is merely `Maybe val` for a record.
So the type we need for a function is something like:

    _f
        :: Maybe val
        -> (val -> SqlExpr ret)
        -> SqlExpr (Maybe ret)

We cannot write an instance of `HasField` for `Maybe`, at all, due to
restrictions around what can or cannot be a valid instance. So `Maybe` isn't
a suitable type.

However, we can't even assume `SqlExpr`. We don't have a `SqlExpr` that wraps
a record - only entity and maybe. So, we have to start with this:

    _f
        ::
         ( ToMaybe ret
         )
        => Maybe val
        -> (val -> ret)
        -> ToMaybeT ret

If the field has type `Entity a`, then the Sql record will have type `SqlExpr
(Entity a)`, and that tracks. If the field has type `Int`, then we get `SqlExpr
(Value Int)`, and that tracks fine too. If the field has type `Record`, then
we'll get `ToMaybeT Record`, which is `Maybe Record` - which is also fine.

But what do we provide if the value is `Nothing`?

If the value is 'Nothing', then we need to figure out how to produce a @ToMaybeT ret@ such that we get a @NULL@ for it back.

This suggests that 'ToMaybeT' needs to evolve to give us a @nulls@, or
something...

But, for now, let's make a separate class.

 -}

projectMaybeRecord
    :: forall val val' ret .
        (ToMaybe ret, HasNulls ret )
    => Maybe val
    -> (val -> ret)
    -> ToMaybeT ret
projectMaybeRecord mrecord k =
    case mrecord of
        Nothing ->
            mkNothing k
        Just record ->
            toMaybe $ k record

getFieldP :: forall sym rec typ. HasField sym rec typ => Proxy sym -> rec -> typ
getFieldP _ = getField @sym

projectMaybeRecordHasField
    :: forall val ret sym.
        (ToMaybe ret, HasNulls ret, HasField sym val ret)
    => Proxy ret -> Proxy sym
    -> Maybe val
    -> ToMaybeT ret
projectMaybeRecordHasField _ _ mrec =
    projectMaybeRecord mrec (getField @sym)

data SqlX = SqlX { x :: SqlExpr (Value Int) }

instance
    ( maybeR ~ ToMaybeT r
    , HasField sym SqlX r
    , ToMaybe r
    , HasNulls r
    )
    => HasField sym (Maybe SqlX) maybeR where
    getField mrec =
        projectMaybeRecord  mrec (getField @sym)

x' :: (HasField "x" (Maybe SqlX) r) => Proxy r
x' = Proxy

-- blah :: Maybe SqlX -> ToMaybeT (SqlExpr (Value Int))
-- blah a = getField @"x" a
