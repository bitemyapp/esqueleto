{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Database.Esqueleto.Record
  ( deriveEsqueletoRecord
  ) where

import Control.Monad.Trans.State.Strict (StateT(..), evalStateT)
import Data.Proxy (Proxy(..))
import Database.Esqueleto.Experimental
       (Entity, PersistValue, SqlExpr, Value(..), (:&)(..))
import Database.Esqueleto.Internal.Internal (SqlSelect(..))
import Language.Haskell.TH
import Data.Bifunctor (first)
import Data.Text (Text)
import Control.Monad (forM)
import Data.Foldable (foldl')
import GHC.Exts (IsString(fromString))

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
deriveEsqueletoRecord originalName = do
  info <- getRecordInfo originalName
  -- It would be nicer to use `mconcat` here but I don't think the right
  -- instance is available in GHC 8.
  recordDec <- makeSqlRecord info
  instanceDec <- makeSqlSelectInstance info
  pure
    [ recordDec
    , instanceDec
    ]

-- | Information about a record we need to generate the declarations.
-- We compute this once and then pass it around to save on complexity /
-- repeated work.
data RecordInfo = RecordInfo
  { -- | The original record's name.
    name :: Name
  , -- | The generated @Sql@-prefixed record's name.
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
  , -- | The original record's field names and types, derived from the
    -- constructors.
    fields :: [(Name, Type)]
  , -- | The generated @Sql@-prefixed record's field names and types, computed
    -- with 'sqlFieldType'.
    sqlFields :: [(Name, Type)]
  }

-- | Get a `RecordInfo` instance for the given record name.
getRecordInfo :: Name -> Q RecordInfo
getRecordInfo name = do
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
      sqlFields = toSqlField `map` fields
      sqlName = makeSqlName name

  pure RecordInfo {..}
  where
    getFields :: Con -> [(Name, Type)]
    getFields (RecC _name fields) = [(fieldName', fieldType') | (fieldName', _bang, fieldType') <- fields]
    getFields con = error $ nonRecordConstructorMessage con

    toSqlField (fieldName', ty) = (fieldName', sqlFieldType ty)

-- | Create a new name by prefixing @Sql@ to a given name.
makeSqlName :: Name -> Name
makeSqlName name = mkName $ "Sql" ++ nameBase name

-- | Transforms a record field type into a corresponding `SqlExpr` type.
--
-- * @'Entity' x@ is transformed into @'SqlExpr' ('Entity' x)@.
-- * @'Maybe' ('Entity' x)@ is transformed into @'SqlExpr' ('Maybe' ('Entity' x))@.
-- * @x@ is transformed into @'SqlExpr' ('Value' x)@.
sqlFieldType :: Type -> Type
sqlFieldType fieldType =
  case fieldType of
    -- Entity x -> SqlExpr (Entity x)
    AppT (ConT ((==) ''Entity -> True)) _innerType -> AppT (ConT ''SqlExpr) fieldType
    -- Maybe (Entity x) -> SqlExpr (Maybe (Entity x))
    AppT
      (ConT ((==) ''Maybe -> True))
      (AppT (ConT ((==) ''Entity -> True)) _innerType) -> AppT (ConT ''SqlExpr) fieldType
    -- x -> SqlExpr (Value x)
    _ -> AppT (ConT ''SqlExpr) (AppT (ConT ''Value) fieldType)

-- | Generates the declaration for an @Sql@-prefixed record, given the original
-- record's information.
makeSqlRecord :: RecordInfo -> Q Dec
makeSqlRecord RecordInfo {..} = do
  let newConstructor = RecC (makeSqlName constructorName) (makeField `map` sqlFields)
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
        AppT
          (AppT (ConT ''SqlSelect) (ConT sqlName))
          (ConT name)

  pure $ InstanceD overlap instanceConstraints instanceType [sqlSelectColsDec', sqlSelectColCountDec', sqlSelectProcessRowDec']

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
              AppE
                (AppE (VarE 'sqlSelectCols) (VarE identInfo))
                (ParensE joinedFields)
          )
          -- `where` clause.
          []
      ]

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
      pure
        ( BindS
            (sqlSelectProcessRowPat fieldType valueName)
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
  pure $
    FunD
      'sqlSelectProcessRow
      [ Clause
          [WildP, VarP colsName]
          ( NormalB $
              AppE
                ( AppE
                    (VarE 'first)
                    ( InfixE
                        (Just $ AppE
                          (VarE 'fromString)
                          (LitE $ StringL $ "Failed to parse " ++ nameBase name ++ ": "))
                        (VarE '(<>))
                        Nothing
                    )
                )
                ( AppE
                    ( AppE
                        (VarE 'evalStateT)
                        (VarE processName)
                    )
                    (VarE colsName)
                )
          )
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
sqlSelectProcessRowPat :: Type -> Name -> Pat
sqlSelectProcessRowPat fieldType var =
  case fieldType of
    -- Entity x -> var
    AppT (ConT ((==) ''Entity -> True)) _innerType -> VarP var
    -- Maybe (Entity x) -> var
    AppT
      (ConT ((==) ''Maybe -> True))
      (AppT (ConT ((==) ''Entity -> True)) _innerType) -> VarP var
    -- x -> Value var
#if MIN_VERSION_template_haskell(2,18,0)
    _ -> ConP 'Value [] [VarP var]
#else
    _ -> ConP 'Value [VarP var]
#endif

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
          value <- sqlSelectProcessRow (Proxy @a) target
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
