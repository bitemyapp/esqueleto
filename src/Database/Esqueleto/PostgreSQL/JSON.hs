{-# LANGUAGE OverloadedStrings #-}
{-|
  This module contains PostgreSQL-specific JSON functions.

  A couple of things to keep in mind about this module:

      * The @Type@ column in the PostgreSQL documentation tables
      are the types of the right operand, the left is always @jsonb@.
      * Since these operators can all take @NULL@ values as their input,
      and most can also output @NULL@ values (even when the inputs are
      guaranteed to not be NULL), all 'JSONB' values are wrapped in
      'Maybe'. This also makes it easier to chain them. (cf. 'JSONBExpr')
      Just use the 'just' function to lift any non-'Maybe' JSONB values
      in case it doesn't type check.
      * As long as the previous operator's resulting value is
      a 'JSONBExpr', any other JSON operator can be used to transform
      the JSON further. (e.g. @[1,2,3] -> 1 \@> 2@)

  /The PostgreSQL version the functions work with are included/
  /in their description./

  @since 3.1.0
-}
module Database.Esqueleto.PostgreSQL.JSON
  ( -- * JSONB Newtype
    --
    -- | With 'JSONB', you can use your Haskell types in your
    -- database table models as long as your type has 'FromJSON'
    -- and 'ToJSON' instances.
    --
    -- @
    -- import Database.Persist.TH
    --
    -- share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
    --   Example
    --     json (JSONB MyType)
    -- |]
    -- @
    --
    -- CAUTION: Remember that changing the 'FromJSON' instance
    -- of your type might result in old data becoming unparsable!
    -- You can use (@JSONB Data.Aeson.Value@) for unstructured/variable JSON.
    JSONB(..)
  , JSONBExpr
  , jsonbVal
  -- * JSONAccessor
  , JSONAccessor(..)
  -- * Arrow operators
  --
  -- | /Better documentation included with individual functions/
  --
  -- The arrow operators are selection functions to select values
  -- from JSON arrays or objects.
  --
  -- === PostgreSQL Documentation
  --
  -- /Requires PostgreSQL version >= 9.3/
  --
  -- @
  --      | Type   | Description                                |  Example                                         | Example Result
  -- -----+--------+--------------------------------------------+--------------------------------------------------+----------------
  --  ->  | int    | Get JSON array element (indexed from zero, | '[{"a":"foo"},{"b":"bar"},{"c":"baz"}]'::json->2 | {"c":"baz"}
  --      |        | negative integers count from the end)      |                                                  |
  --  ->  | text   | Get JSON object field by key               | '{"a": {"b":"foo"}}'::json->'a'                  | {"b":"foo"}
  --  ->> | int    | Get JSON array element as text             | '[1,2,3]'::json->>2                              | 3
  --  ->> | text   | Get JSON object field as text              | '{"a":1,"b":2}'::json->>'b'                      | 2
  --  \#>  | text[] | Get JSON object at specified path          | '{"a": {"b":{"c": "foo"}}}'::json#>'{a,b}'       | {"c": "foo"}
  --  \#>> | text[] | Get JSON object at specified path as text  | '{"a":[1,2,3],"b":[4,5,6]}'::json#>>'{a,2}'      | 3
  -- @
  , (->.)
  , (->>.)
  , (#>.)
  , (#>>.)
  -- * Filter operators
  --
  -- | /Better documentation included with individual functions/
  --
  -- These functions test certain properties of JSON values
  -- and return booleans, so are mainly used in WHERE clauses.
  --
  -- === PostgreSQL Documentation
  --
  -- /Requires PostgreSQL version >= 9.4/
  --
  -- @
  --     | Type   | Description                                                     |  Example
  -- ----+--------+-----------------------------------------------------------------+---------------------------------------------------
  --  \@> | jsonb  | Does the left JSON value contain within it the right value?     | '{"a":1, "b":2}'::jsonb \@> '{"b":2}'::jsonb
  --  <\@ | jsonb  | Is the left JSON value contained within the right value?        | '{"b":2}'::jsonb <\@ '{"a":1, "b":2}'::jsonb
  --  ?  | text   | Does the string exist as a top-level key within the JSON value? | '{"a":1, "b":2}'::jsonb ? 'b'
  --  ?| | text[] | Do any of these array strings exist as top-level keys?          | '{"a":1, "b":2, "c":3}'::jsonb ?| array['b', 'c']
  --  ?& | text[] | Do all of these array strings exist as top-level keys?          | '["a", "b"]'::jsonb ?& array['a', 'b']
  -- @
  , (@>.)
  , (<@.)
  , (?.)
  , (?|.)
  , (?&.)
  -- * Deletion and concatenation operators
  --
  -- | /Better documentation included with individual functions/
  --
  -- These operators change the shape of the JSON value and
  -- also have the highest risk of throwing an exception.
  -- Please read the descriptions carefully before using these functions.
  --
  -- === PostgreSQL Documentation
  --
  -- /Requires PostgreSQL version >= 9.5/
  --
  -- @
  --     | Type    | Description                                                            |  Example
  -- ----+---------+------------------------------------------------------------------------+-------------------------------------------------
  --  || | jsonb   | Concatenate two jsonb values into a new jsonb value                    | '["a", "b"]'::jsonb || '["c", "d"]'::jsonb
  --  -  | text    | Delete key/value pair or string element from left operand.             | '{"a": "b"}'::jsonb - 'a'
  --     |         | Key/value pairs are matched based on their key value.                  |
  --  -  | integer | Delete the array element with specified index (Negative integers count | '["a", "b"]'::jsonb - 1
  --     |         | from the end). Throws an error if top level container is not an array. |
  --  \#- | text[]  | Delete the field or element with specified path                        | '["a", {"b":1}]'::jsonb \#- '{1,b}'
  --     |         | (for JSON arrays, negative integers count from the end)                |
  -- @
  --
  -- /Requires PostgreSQL version >= 10/
  --
  -- @
  --     | Type    | Description                                                            |  Example
  -- ----+---------+------------------------------------------------------------------------+-------------------------------------------------
  --  -  | text[]  | Delete multiple key/value pairs or string elements from left operand.  | '{"a": "b", "c": "d"}'::jsonb - '{a,c}'::text[]
  --     |         | Key/value pairs are matched based on their key value.                  |
  -- @
  , (-.)
  , (--.)
  , (#-.)
  , (||.)
  ) where

import Data.Text (Text)
import Database.Esqueleto.Internal.Language hiding ((?.), (-.), (||.))
import Database.Esqueleto.Internal.PersistentImport
import Database.Esqueleto.Internal.Sql
import Database.Esqueleto.PostgreSQL.JSON.Instances


infixl 6 ->., ->>., #>., #>>.
infixl 6 @>., <@., ?., ?|., ?&.
infixl 6 ||., -., --., #-.


-- | /Requires PostgreSQL version >= 9.3/
--
-- This function extracts the jsonb value from a JSON array or object,
-- depending on whether you use an @int@ or a @text@. (cf. 'JSONAccessor')
--
-- As long as the left operand is @jsonb@, this function will not
-- throw an exception, but will return @NULL@ when an @int@ is used on
-- anything other than a JSON array, or a @text@ is used on anything
-- other than a JSON object.
--
-- === __PostgreSQL Documentation__
--
-- @
--     | Type | Description                                |  Example                                         | Example Result
-- ----+------+--------------------------------------------+--------------------------------------------------+----------------
--  -> | int  | Get JSON array element (indexed from zero) | '[{"a":"foo"},{"b":"bar"},{"c":"baz"}]'::json->2 | {"c":"baz"}
--  -> | text | Get JSON object field by key               | '{"a": {"b":"foo"}}'::json->'a'                  | {"b":"foo"}
-- @
--
-- @since 3.1.0
(->.) :: JSONBExpr a -> JSONAccessor -> JSONBExpr b
(->.) value (JSONKey txt) = unsafeSqlBinOp " -> " value $ val txt
(->.) value (JSONIndex i) = unsafeSqlBinOp " -> " value $ val i

-- | /Requires PostgreSQL version >= 9.3/
--
-- Identical to '->.', but the resulting DB type is a @text@,
-- so it could be chained with anything that uses @text@.
--
-- __CAUTION: if the "scalar" JSON value @null@ is the result__
-- __of this function, PostgreSQL will interpret it as a__
-- __PostgreSQL @NULL@ value, and will therefore be 'Nothing'__
-- __instead of (Just "null")__
--
-- === __PostgreSQL Documentation__
--
-- @
--      | Type | Description                    |  Example                    | Example Result
-- -----+------+--------------------------------+-----------------------------+----------------
--  ->> | int  | Get JSON array element as text | '[1,2,3]'::json->>2         | 3
--  ->> | text | Get JSON object field as text  | '{"a":1,"b":2}'::json->>'b' | 2
-- @
--
-- @since 3.1.0
(->>.) :: JSONBExpr a -> JSONAccessor -> SqlExpr (Value (Maybe Text))
(->>.) value (JSONKey txt) = unsafeSqlBinOp " ->> " value $ val txt
(->>.) value (JSONIndex i) = unsafeSqlBinOp " ->> " value $ val i

-- | /Requires PostgreSQL version >= 9.3/
--
-- This operator can be used to select a JSON value from deep inside another one.
-- It only works on objects and arrays and will result in @NULL@ ('Nothing') when
-- encountering any other JSON type.
--
-- The 'Text's used in the right operand list will always select an object field, but
-- can also select an index from a JSON array if that text is parsable as an integer.
--
-- Consider the following:
--
-- @
-- x ^. TestBody #>. ["0","1"]
-- @
--
-- The following JSON values in the @test@ table's @body@ column will be affected:
--
-- @
--  Values in column                     | Resulting value
-- --------------------------------------+----------------------------
-- {"0":{"1":"Got it!"}}                 | "Got it!"
-- {"0":[null,["Got it!","Even here!"]]} | ["Got it!", "Even here!"]
-- [{"1":"Got it again!"}]               | "Got it again!"
-- [[null,{\"Wow\":"so deep!"}]]           | {\"Wow\": "so deep!"}
-- false                                 | NULL
-- "nope"                                | NULL
-- 3.14                                  | NULL
-- @
--
-- === __PostgreSQL Documentation__
--
-- @
--      | Type   | Description                       |  Example                                   | Example Result
-- -----+--------+-----------------------------------+--------------------------------------------+----------------
--  \#>  | text[] | Get JSON object at specified path | '{"a": {"b":{"c": "foo"}}}'::json#>'{a,b}' | {"c": "foo"}
-- @
--
-- @since 3.1.0
(#>.) :: JSONBExpr a -> [Text] -> JSONBExpr b
(#>.) value = unsafeSqlBinOp " #> " value . mkTextArray


-- | /Requires PostgreSQL version >= 9.3/
--
-- This function is to '#>.' as '->>.' is to '->.'
--
-- __CAUTION: if the "scalar" JSON value @null@ is the result__
-- __of this function, PostgreSQL will interpret it as a__
-- __PostgreSQL @NULL@ value, and will therefore be 'Nothing'__
-- __instead of (Just "null")__
--
-- === __PostgreSQL Documentation__
--
-- @
--      | Type   | Description                               |  Example                                    | Example Result
-- -----+--------+-------------------------------------------+---------------------------------------------+----------------
--  \#>> | text[] | Get JSON object at specified path as text | '{"a":[1,2,3],"b":[4,5,6]}'::json#>>'{a,2}' | 3
-- @
--
-- @since 3.1.0
(#>>.) :: JSONBExpr a -> [Text] -> SqlExpr (Value (Maybe Text))
(#>>.)  value = unsafeSqlBinOp " #>> " value . mkTextArray

-- | /Requires PostgreSQL version >= 9.4/
--
-- This operator checks for the JSON value on the right to be a subset
-- of the JSON value on the left.
--
-- Examples of the usage of this operator can be found in
-- the Database.Persist.Postgresql.JSON module.
--
-- (here: <https://hackage.haskell.org/package/persistent-postgresql-2.10.0/docs/Database-Persist-Postgresql-JSON.html>)
--
-- === __PostgreSQL Documentation__
--
-- @
--     | Type  | Description                                                 |  Example
-- ----+-------+-------------------------------------------------------------+---------------------------------------------
--  \@> | jsonb | Does the left JSON value contain within it the right value? | '{"a":1, "b":2}'::jsonb \@> '{"b":2}'::jsonb
-- @
--
-- @since 3.1.0
(@>.) :: JSONBExpr a -> JSONBExpr b -> SqlExpr (Value Bool)
(@>.) = unsafeSqlBinOp " @> "

-- | /Requires PostgreSQL version >= 9.4/
--
-- This operator works the same as '@>.', just with the arguments flipped.
-- So it checks for the JSON value on the left to be a subset of JSON value on the right.
--
-- Examples of the usage of this operator can be found in
-- the Database.Persist.Postgresql.JSON module.
--
-- (here: <https://hackage.haskell.org/package/persistent-postgresql-2.10.0/docs/Database-Persist-Postgresql-JSON.html>)
--
-- === __PostgreSQL Documentation__
--
-- @
--     | Type  | Description                                              |  Example
-- ----+-------+----------------------------------------------------------+---------------------------------------------
--  <\@ | jsonb | Is the left JSON value contained within the right value? | '{"b":2}'::jsonb <\@ '{"a":1, "b":2}'::jsonb
-- @
--
-- @since 3.1.0
(<@.) :: JSONBExpr a -> JSONBExpr b -> SqlExpr (Value Bool)
(<@.) = unsafeSqlBinOp " <@ "

-- | /Requires PostgreSQL version >= 9.4/
--
-- This operator checks if the given text is a top-level member of the
-- JSON value on the left. This means a top-level field in an object, a
-- top-level string in an array or just a string value.
--
-- Examples of the usage of this operator can be found in
-- the Database.Persist.Postgresql.JSON module.
--
-- (here: <https://hackage.haskell.org/package/persistent-postgresql-2.10.0/docs/Database-Persist-Postgresql-JSON.html>)
--
-- === __PostgreSQL Documentation__
--
-- @
--    | Type | Description                                                     |  Example
-- ---+------+-----------------------------------------------------------------+-------------------------------
--  ? | text | Does the string exist as a top-level key within the JSON value? | '{"a":1, "b":2}'::jsonb ? 'b'
-- @
--
-- @since 3.1.0
(?.) :: JSONBExpr a -> Text -> SqlExpr (Value Bool)
(?.) value = unsafeSqlBinOp " ?? " value . val

-- | /Requires PostgreSQL version >= 9.4/
--
-- This operator checks if __ANY__ of the given texts is a top-level member
-- of the JSON value on the left. This means any top-level field in an object,
-- any top-level string in an array or just a string value.
--
-- Examples of the usage of this operator can be found in
-- the Database.Persist.Postgresql.JSON module.
--
-- (here: <https://hackage.haskell.org/package/persistent-postgresql-2.10.0/docs/Database-Persist-Postgresql-JSON.html>)
--
-- === __PostgreSQL Documentation__
--
-- @
--     | Type   | Description                                            |  Example
-- ----+--------+--------------------------------------------------------+---------------------------------------------------
--  ?| | text[] | Do any of these array strings exist as top-level keys? | '{"a":1, "b":2, "c":3}'::jsonb ?| array['b', 'c']
-- @
--
-- @since 3.1.0
(?|.) :: JSONBExpr a -> [Text] -> SqlExpr (Value Bool)
(?|.) value = unsafeSqlBinOp " ??| " value . mkTextArray

-- | /Requires PostgreSQL version >= 9.4/
--
-- This operator checks if __ALL__ of the given texts are top-level members
-- of the JSON value on the left. This means a top-level field in an object,
-- a top-level string in an array or just a string value.
--
-- Examples of the usage of this operator can be found in
-- the Database.Persist.Postgresql.JSON module.
--
-- (here: <https://hackage.haskell.org/package/persistent-postgresql-2.10.0/docs/Database-Persist-Postgresql-JSON.html>)
--
-- === __PostgreSQL Documentation__
--
-- @
--     | Type   | Description                                            |  Example
-- ----+--------+--------------------------------------------------------+----------------------------------------
--  ?& | text[] | Do all of these array strings exist as top-level keys? | '["a", "b"]'::jsonb ?& array['a', 'b']
-- @
--
-- @since 3.1.0
(?&.) :: JSONBExpr a -> [Text] -> SqlExpr (Value Bool)
(?&.) value = unsafeSqlBinOp " ??& " value . mkTextArray

-- | /Requires PostgreSQL version >= 9.5/
--
-- This operator concatenates two JSON values. The behaviour is
-- self-evident when used on two arrays, but the behaviour on different
-- combinations of JSON values might behave unexpectedly.
--
-- __CAUTION: THIS FUNCTION THROWS AN EXCEPTION WHEN CONCATENATING__
-- __A JSON OBJECT WITH A JSON SCALAR VALUE!__
--
-- === __Arrays__
--
-- This operator is a standard concatenation function when used on arrays:
--
-- @
-- [1,2]   || [2,3]   == [1,2,2,3]
-- []      || [1,2,3] == [1,2,3]
-- [1,2,3] || []      == [1,2,3]
-- @
--
-- === __Objects__
-- When concatenating JSON objects with other JSON objects, the fields
-- from the JSON object on the right are added to the JSON object on the
-- left. When concatenating a JSON object with a JSON array, the object
-- will be inserted into the array; either on the left or right, depending
-- on the position relative to the operator.
--
-- When concatening an object with a scalar value, an exception is thrown.
--
-- @
-- {"a": 3.14}                    || {"b": true}         == {"a": 3.14, "b": true}
-- {"a": "b"}                     || {"a": null}         == {"a": null}
-- {"a": {"b": true, "c": false}} || {"a": {"b": false}} == {"a": {"b": false}}
-- {"a": 3.14}                    || [1,null]            == [{"a": 3.14},1,null]
-- [1,null]                       || {"a": 3.14}         == [1,null,{"a": 3.14}]
-- 1                              || {"a": 3.14}         == ERROR: invalid concatenation of jsonb objects
-- {"a": 3.14}                    || false               == ERROR: invalid concatenation of jsonb objects
-- @
--
-- === __Scalar values__
--
-- Scalar values can be thought of as being singleton arrays when
-- used with this operator. This rule does not apply when concatenating
-- with JSON objects.
--
-- @
-- 1          || null       == [1,null]
-- true       || "a"        == [true,"a"]
-- [1,2]      || false      == [1,2,false]
-- null       || [1,"a"]    == [null,1,"a"]
-- {"a":3.14} || true       == ERROR: invalid concatenation of jsonb objects
-- 3.14       || {"a":3.14} == ERROR: invalid concatenation of jsonb objects
-- {"a":3.14} || [true]     == [{"a":3.14},true]
-- [false]    || {"a":3.14} == [false,{"a":3.14}]
-- @
--
-- === __PostgreSQL Documentation__
--
-- @
--     | Type  | Description                                         |  Example
-- ----+-------+-----------------------------------------------------+--------------------------------------------
--  || | jsonb | Concatenate two jsonb values into a new jsonb value | '["a", "b"]'::jsonb || '["c", "d"]'::jsonb
-- @
--
-- /Note: The @||@ operator concatenates the elements at the top level of/
-- /each of its operands. It does not operate recursively./
--
-- /For example, if both operands are objects with a common key field name,/
-- /the value of the field in the result will just be the value from the right/
-- /hand operand./
--
-- @since 3.1.0
(||.) :: JSONBExpr a -> JSONBExpr b -> JSONBExpr c
(||.) = unsafeSqlBinOp " || "

-- | /Requires PostgreSQL version >= 9.5/
--
-- This operator can remove a key from an object or a string element from an array
-- when using text, and remove certain elements by index from an array when using
-- integers.
--
-- Negative integers delete counting from the end of the array.
-- (e.g. @-1@ being the last element, @-2@ being the second to last, etc.)
--
-- __CAUTION: THIS FUNCTION THROWS AN EXCEPTION WHEN USED ON ANYTHING OTHER__
-- __THAN OBJECTS OR ARRAYS WHEN USING TEXT, AND ANYTHING OTHER THAN ARRAYS__
-- __WHEN USING INTEGERS!__
--
-- === __Objects and arrays__
--
-- @
-- {"a": 3.14}            - "a"         == {}
-- {"a": "b"}             - "b"         == {"a": "b"}
-- {"a": 3.14}            - "a"         == {}
-- {"a": 3.14, "c": true} - "a"         == {"c": true}
-- ["a", 2, "c"]          - "a"         == [2, "c"] -- can remove strings from arrays
-- [true, "b", 5]         - 0           == ["b", 5]
-- [true, "b", 5]         - 3           == [true, "b", 5]
-- [true, "b", 5]         - -1          == [true, "b"]
-- [true, "b", 5]         - -4          == [true, "b", 5]
-- []                     - 1           == []
-- {"1": true}            - 1           == ERROR: cannot delete from object using integer index
-- 1                      - \<anything\>  == ERROR: cannot delete from scalar
-- "a"                    - \<anything\>  == ERROR: cannot delete from scalar
-- true                   - \<anything\>  == ERROR: cannot delete from scalar
-- null                   - \<anything\>  == ERROR: cannot delete from scalar
-- @
--
-- === __PostgreSQL Documentation__
--
-- @
--    | Type    | Description                                                            |  Example
-- ---+---------+------------------------------------------------------------------------+-------------------------------------------------
--  - | text    | Delete key/value pair or string element from left operand.             | '{"a": "b"}'::jsonb - 'a'
--    |         | Key/value pairs are matched based on their key value.                  |
--  - | integer | Delete the array element with specified index (Negative integers count | '["a", "b"]'::jsonb - 1
--    |         | from the end). Throws an error if top level container is not an array. |
-- @
--
-- @since 3.1.0
(-.) :: JSONBExpr a -> JSONAccessor -> JSONBExpr b
(-.) value (JSONKey txt) = unsafeSqlBinOp " - " value $ val txt
(-.) value (JSONIndex i) = unsafeSqlBinOp " - " value $ val i

-- | /Requires PostgreSQL version >= 10/
--
-- Removes a set of keys from an object, or string elements from an array.
--
-- This is the same operator internally as `-.`, but the option to use a @text
-- array@, instead of @text@ or @integer@ was only added in version 10.
-- That's why this function is seperate from `-.`
--
-- NOTE: The following is equivalent:
--
-- @{some JSON expression} -. "a" -. "b"@
--
-- is equivalent to
--
-- @{some JSON expression} --. ["a","b"]@
--
-- === __PostgreSQL Documentation__
--
-- @
--    | Type    | Description                                                            |  Example
-- ---+---------+------------------------------------------------------------------------+-------------------------------------------------
--  - | text[]  | Delete multiple key/value pairs or string elements from left operand.  | '{"a": "b", "c": "d"}'::jsonb - '{a,c}'::text[]
--    |         | Key/value pairs are matched based on their key value.                  |
-- @
--
-- @since 3.1.0
(--.) :: JSONBExpr a -> [Text] -> JSONBExpr b
(--.) value = unsafeSqlBinOp " - " value . mkTextArray

-- | /Requires PostgreSQL version >= 9.5/
--
-- This operator can remove elements nested in an object.
--
-- If a 'Text' is not parsable as a number when selecting in an array
-- (even when halfway through the selection) an exception will be thrown.
--
-- Negative integers delete counting from the end of an array.
-- (e.g. @-1@ being the last element, @-2@ being the second to last, etc.)
--
-- __CAUTION: THIS FUNCTION THROWS AN EXCEPTION WHEN USED__
-- __ON ANYTHING OTHER THAN OBJECTS OR ARRAYS, AND WILL__
-- __ALSO THROW WHEN TRYING TO SELECT AN ARRAY ELEMENT WITH__
-- __A NON-INTEGER TEXT__
--
-- === __Objects__
--
-- @
-- {"a": 3.14, "b": null}        #- []        == {"a": 3.14, "b": null}
-- {"a": 3.14, "b": null}        #- ["a"]     == {"b": null}
-- {"a": 3.14, "b": null}        #- ["a","b"] == {"a": 3.14, "b": null}
-- {"a": {"b":false}, "b": null} #- ["a","b"] == {"a": {}, "b": null}
-- @
--
-- === __Arrays__
--
-- @
-- [true, {"b":null}, 5]       #- []            == [true, {"b":null}, 5]
-- [true, {"b":null}, 5]       #- ["0"]         == [{"b":null}, 5]
-- [true, {"b":null}, 5]       #- ["b"]         == ERROR: path element at position 1 is not an integer: "b"
-- [true, {"b":null}, 5]       #- ["1","b"]     == [true, {}, 5]
-- [true, {"b":null}, 5]       #- ["-2","b"]    == [true, {}, 5]
-- {"a": {"b":[false,4,null]}} #- ["a","b","2"] == {"a": {"b":[false,4]}}
-- {"a": {"b":[false,4,null]}} #- ["a","b","c"] == ERROR: path element at position 3 is not an integer: "c"
-- @
--
-- === __Other values__
--
-- @
-- 1    \#- {anything} == ERROR: cannot delete from scalar
-- "a"  \#- {anything} == ERROR: cannot delete from scalar
-- true \#- {anything} == ERROR: cannot delete from scalar
-- null \#- {anything} == ERROR: cannot delete from scalar
-- @
--
-- === __PostgreSQL Documentation__
--
-- @
--     | Type   | Description                                             |  Example
-- ----+--------+---------------------------------------------------------+------------------------------------
--  \#- | text[] | Delete the field or element with specified path         | '["a", {"b":1}]'::jsonb \#- '{1,b}'
--     |        | (for JSON arrays, negative integers count from the end) |
-- @
--
-- @since 3.1.0
(#-.) :: JSONBExpr a -> [Text] -> JSONBExpr b
(#-.) value = unsafeSqlBinOp " #- " value . mkTextArray

mkTextArray :: [Text] -> SqlExpr (Value PersistValue)
mkTextArray = val . PersistArray . fmap toPersistValue
