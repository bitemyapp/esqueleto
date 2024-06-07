3.5.13.1
========
- @TeofilC
  - [#394](https://github.com/bitemyapp/esqueleto/pull/394)
    - Use TH quotes to eliminate some CPP.

3.5.13.0
========
- @ac251
    - [#402](https://github.com/bitemyapp/esqueleto/pull/402)
      - Add `forNoKeyUpdate` and `forKeyShare` locking kinds for postgres

3.5.12.0
========
- @csamak
    - [#405](https://github.com/bitemyapp/esqueleto/pull/405)
        - `ToMaybe` instances are now derived for Maybe records.
          See [Issue #401](https://github.com/bitemyapp/esqueleto/issues/401).

3.5.11.2
========
- @arguri
    - [#387](https://github.com/bitemyapp/esqueleto/pull/387)
        - Fix build for ghc 9.8.1 / template-haskell 2.18

3.5.11.0
========
- @9999years, @halogenandtoast
    - [#378](https://github.com/bitemyapp/esqueleto/pull/378)
        - `ToMaybe` instances are now derived for records so you can now left
          join them in queries

3.5.10.3
========
- @ttuegel
    - [#377](https://github.com/bitemyapp/esqueleto/pull/377)
        - Fix Postgres syntax for `noWait`

3.5.10.2
========
- @parsonsmatt
    - [#376](https://github.com/bitemyapp/esqueleto/pull/376)
        - When using Postgres 15, `LIMIT`, and the `locking` functions, you
          could accidentally construct SQL code like:

          > ... LIMIT 1FOR UPDATE ...

          This parsed on Postgres <15, but the new Postgres parser is more
          strict, and fails to parse. This PR introduces newlines between each
          query chunk, which fixes the issue.

3.5.10.1
========
- @9999years
    - [#369](https://github.com/bitemyapp/esqueleto/pull/369)
        - Fix `myAge` type in `deriveEsqueletoRecord` documentation

3.5.10.0
========
- @ivanbakel
    - [#328](https://github.com/bitemyapp/esqueleto/pull/328)
        - Add `ToAlias` instances for 9- to 16-tuples
        - Add `ToAliasReference` instances for 9- to 16-tuples
- @parsonsmatt
    - [#365](https://github.com/bitemyapp/esqueleto/pull/365)
        - Add `isNothing_` and `groupBy_` to avoid name conflicts with
          `Data.List` and `Data.Maybe`.

3.5.9.1
=======
- @duplode
    - [#363](https://github.com/bitemyapp/esqueleto/pull/363)
      - Add missing `just` to left join examples in the Haddocks


3.5.9.0
=======
- @9999years
    - [#350](https://github.com/bitemyapp/esqueleto/pull/350)
      - Add `GetFirstTable`, `getTable`, `getTableMaybe` helpers for selecting
        tables from `:&` chains
- @josephsumabat
    - [#339](https://github.com/bitemyapp/esqueleto/pull/333)
      - Add `forUpdateOf`, `forShareOf` locking kinds for postgres
- @parsonsmatt
    - [#342](https://github.com/bitemyapp/esqueleto/pull/342)
        - Create a `TypeError` instance for `Functor SqlExpr`, adding
          documentation and work arounds for the need.
- @9999years
    - [#327](https://github.com/bitemyapp/esqueleto/pull/327)
        - Fixed a Haddock typo causing documentation to render incorrectly

3.5.8.1
=======
- @belevy
    - [#336](https://github.com/bitemyapp/esqueleto/pull/336)
        - Fix bug with multiple nested subqueries introduced in 3.5.7.1
        - Set operations will now only reuse variable names within the context of the set operation.
            a subquery that references the set operation will correctly pick up where the subquery left off
3.5.8.0
=======
- @ivanbakel
    - [#331](https://github.com/bitemyapp/esqueleto/pull/331)
        - Add `deriveEsqueletoRecordWith` to derive Esqueleto instances for
          records using custom deriving settings.
        - Add `DeriveEsqueletoRecordSettings` to control how Esqueleto record
          instances are derived.
        - Add `sqlNameModifier` to control how Esqueleto record instance
          deriving generates the SQL record type name.
        - Add `sqlFieldModifier` to control how Esqueleto record instance
          deriving generates the SQL record fields.

3.5.7.1
=======
- @belevy
    - [#334](https://github.com/bitemyapp/esqueleto/pull/334)
        - Fix alias name bug with union and subselect

3.5.7.0
=======
- @ivanbakel
    - [#329](https://github.com/bitemyapp/esqueleto/pull/329)
        - Add `ToAlias` and `ToAliasReference` instances to the type produced
          by `deriveEsqueletoRecord`, allowing in-SQL records to be used in
          CTEs

- @9999years
    - [#324](https://github.com/bitemyapp/esqueleto/pull/324)
        - Add ability to use nested records with `deriveEsqueletoRecord`

3.5.6.0
=======
- @9999years
    - [#323](https://github.com/bitemyapp/esqueleto/pull/323)
        - Add ability to derive esqueleto instances for records

3.5.5.0
=======
- @parsonsmatt
    - [#317](https://github.com/bitemyapp/esqueleto/pull/317)
        - Add `Eq` and `Show` instances to `:&`

3.5.4.2
=======
- @parsonsmatt
    - [#318](https://github.com/bitemyapp/esqueleto/pull/318)
        - Remove use of `SqlReadT` and `SqlWriteT` type alias so that Simplified
          Subsumption doesn't bite end users

3.5.4.1
=======
- @parsonsmatt
    - [#312](https://github.com/bitemyapp/esqueleto/pull/312/)
        - Support `persistent-2.14.0.0`

3.5.4.0
=======
- @parsonsmatt
    - [#310](https://github.com/bitemyapp/esqueleto/pull/310)
        - Add instances of `HasField` for `SqlExpr (Entity rec)` and `SqlExpr
          (Maybe (Entity rec))`. These instances allow you to use the
          `OverloadedRecordDot` language extension in GHC 9.2 with SQL
          representations of database entities.

3.5.3.2
=======
- @parsonsmatt
    - [#309](https://github.com/bitemyapp/esqueleto/pull/309)
        - Bump `time` version bound

3.5.3.1
=======
- @jappeace
  - [#303](https://github.com/bitemyapp/esqueleto/pull/303)
      - Added docs for delete function for new experimental API.

3.5.3.0
=======
- @m4dc4p
  - [#291](https://github.com/bitemyapp/esqueleto/pull/291)
         - Added `ToAlias` and `ToAliasReference` instaces to the `:&` type, mirroring
         the tuple instances for the same classes. See [Issue #290](https://github.com/bitemyapp/esqueleto/issues/290)
         for discussion.
- @NikitaRazmakhnin
  - [#284](https://github.com/bitemyapp/esqueleto/pull/284)
        - Add PostgreSQL-specific support of VALUES(..) literals


3.5.2.2
=======
- @NikitaRazmakhnin
  - [#278](https://github.com/bitemyapp/esqueleto/pull/278)
        - Fix generating of bad sql using nexted expressions with `distinctOnOrderBy`.

3.5.2.1
=======
- @cdparks
  - [#273](https://github.com/bitemyapp/esqueleto/pull/273)
        - Avoid generating an empty list as the left operand to `NOT IN`.

3.5.2.0
=======
- @ivanbakel
  - [#268](https://github.com/bitemyapp/esqueleto/pull/268)
        - Added `SqlSelect` instance for `(:&)`, allowing it to be returned from
          queries just like `(,)` tuples.

3.5.1.0
=======
- @ibarrae
  - [#265](https://github.com/bitemyapp/esqueleto/pull/265)
        - Added `selectOne`

3.5.0.0
=======
- @belevy
  - [#228](https://github.com/bitemyapp/esqueleto/pull/228)
        - Destroy all GADTs; Removes the From GADT and SqlExpr GADT
        - From GADT is replaced with a From data type and FromRaw
        - SqlExpr is now all defined in terms of ERaw
        - Modified ERaw to contain a SqlExprMeta with any extra information
          that may be needed
        - Experimental top level is now strictly for documentation and all the
          implementation details are in Experimental.* modules
- @parsonsmatt
    - [#259](https://github.com/bitemyapp/esqueleto/pull/259)
        - Create the `Database.Esqueleto.Legacy` module. The
          `Database.Esqueleto` module now emits a warning, directing users to
          either import `Database.Esqueleto.Legacy` to keep the old behavior or
          to import `Database.Esqueleto.Experimental` to opt in to the new
          behavior.
        - Deleted the deprecated modules
          `Database.Esqueleto.Internal.{Language,Sql}`. Please use
          `Database.Esqueleto.Internal.Internal` instead, or ideally post what
          you need from the library so we can support you safely.
        - Support GHC 9

3.4.2.2
=======
- @parsonsmatt
  - [#255](https://github.com/bitemyapp/esqueleto/pull/255)
    - Fix a bug where a composite primary key in a `groupBy` clause would break.

3.4.2.1
=======
- @parsonsmatt
  - [#245](https://github.com/bitemyapp/esqueleto/pull/245)
    - Support `persistent-2.13`

3.4.2.0
=======
- @parsonsmatt
  - [#243](https://github.com/bitemyapp/esqueleto/pull/243)
    - Support `persistent-2.12`

3.4.1.1
=======
- @MaxGabriel
  - [#240](https://github.com/bitemyapp/esqueleto/pull/240/files)
    - Improve recommend hlint to avoid doing `x = NULL` SQL queries

3.4.1.0
=======
- @arthurxavierx
  - [#238](https://github.com/bitemyapp/esqueleto/pull/238)
    - Fix non-exhaustive patterns in `unsafeSqlAggregateFunction`
- @Vlix
  - [#232](https://github.com/bitemyapp/esqueleto/pull/232)
    - Export the `ValidOnClauseValue` type family


3.4.0.1
=======
- @arthurxavierx
  - [#221](https://github.com/bitemyapp/esqueleto/pull/221)
    - Deprecate `ToAliasT` and `ToAliasReferenceT`
- @parsonsmatt
  - [#226](https://github.com/bitemyapp/esqueleto/pull/226)
    - Support `persistent-2.11`
- @belevy
  - [#225](https://github.com/bitemyapp/esqueleto/pull/225)
    - Simplify `ToFromT` extracting the overlapping and type error instances
    - Make `ToFromT` and associated type family of `ToFrom`

3.4.0.0
=======
- @belevy, @charukiewicz
  - [#215](https://github.com/bitemyapp/esqueleto/pull/215)
    - Added support for common table expressions (`with`, `withRecursive`)
    - Added support for lateral JOINs with updated example (Example #6)
    - Deprecated `SelectQuery`, removing the neccessity to tag `SqlQuery` values
    - Deprecated use of data constructors for SQL set operations (replaced with functions)
    - Refactored module structure to fix haddock build (fixes build from `3.3.4.0`)

3.3.4.1
=======
- @maxgabriel
  - [#214](https://github.com/bitemyapp/esqueleto/pull/214)
    - Add suggested hlint rules for proper `isNothing` usage


3.3.4.0
=======
- @parsonsmatt
  - [#205](https://github.com/bitemyapp/esqueleto/pull/205)
    - More documentation on the `Experimental` module
    - `Database.Esqueleto.Experimental` now reexports `Database.Esqueleto`, so
      the new "approved" import syntax is less verbose. Before, you'd write:

      ```haskell
      import Database.Esqueleto hiding (from, on)
      import Database.Esqueleto.Experimental
      ```

      Now you can merely write:

      ```haskell
      import Database.Esqueleto.Experimental
      ```

      Users will get 'redundant import' warnings if they followed the original
      syntax, the solution is evident from the error message provided.

3.3.3.3
=======
- @belevy
  - [#191](https://github.com/bitemyapp/esqueleto/pull/191) - Bugfix rollup:
    Fix issue with extra characters in generated SQL;
    Fix ToAliasReference for already referenced values;
    Fix Alias/Reference for Maybe Entity
- @maxgabriel
  - [#203](https://github.com/bitemyapp/esqueleto/pull/203) Document `isNothing`
- @sestrella
  - [#198](https://github.com/bitemyapp/esqueleto/pull/198) - Allow PostgreSQL aggregate functions to take a filter clause

3.3.3.2
========
- @maxgabriel
  - [#190](https://github.com/bitemyapp/esqueleto/pull/190) Further document and test `ToBaseId`

3.3.3.1
========
- @belevy
  - [#189](https://github.com/bitemyapp/esqueleto/pull/189) - Fix bug in function calls with
    aliased values introduced by SubQuery joins.

3.3.3.0
========
- @belevy
  - [#172](https://github.com/bitemyapp/esqueleto/pull/172) - Introduce new
    experimental module for joins, set operations (eg UNION), and safer queries
    from outer joins.

3.3.2
========

- @belevy
  - [#177](https://github.com/bitemyapp/esqueleto/pull/177) Fix natural key handling in (^.)

3.3.1.1
========

- @parsonsmatt
  - [#170](https://github.com/bitemyapp/esqueleto/pull/170) Add documentation to `groupBy` to explain tuple nesting.

3.3.1
========

- @charukiewicz, @belevy, @joemalin95
  - [#167](https://github.com/bitemyapp/esqueleto/pull/167): Exposed functions that were added in `3.3.0`

3.3.0
========

- @charukiewicz, @belevy, @joemalin95
  - [#166](https://github.com/bitemyapp/esqueleto/pull/166): Add several common SQL string functions: `upper_`, `trim_`, `ltrim_`, `rtrim_`, `length_`, `left_`, `right_`

3.2.3
========

- @hdgarrood
  - [#163](https://github.com/bitemyapp/esqueleto/pull/163): Allow `unsafeSqlFunction` to take up to 10 arguments without needing to nest tuples.

3.2.2
========

- @parsonsmatt
  - [#161](https://github.com/bitemyapp/esqueleto/pull/161/): Fix an issue where
    nested joins didn't get the right on clause.

3.2.1
========

- @parsonsmatt
  - [#159](https://github.com/bitemyapp/esqueleto/pull/159): Add an instance of `UnsafeSqlFunction ()` for 0-argument SQL
  functions.

3.2.0
========

- @parsonsmatt
  - [#153](https://github.com/bitemyapp/esqueleto/pull/153): Deprecate
    `sub_select` and introduce `subSelect`, `subSelectMaybe`, and
    `subSelectUnsafe`.
- @parsonsmatt
  - [#156](https://github.com/bitemyapp/esqueleto/pull/156): Remove the
    restriction that `on` clauses must appear in reverse order to the joining
    tables.

3.1.3
========

- @JoseD92
  - [#155](https://github.com/bitemyapp/esqueleto/pull/149): Added `insertSelectWithConflict` postgres function.

3.1.2
========

- @tippenein
  - [#149](https://github.com/bitemyapp/esqueleto/pull/157): Added `associateJoin` query helpers.

3.1.1
=======

- @JoseD92
  - [#149](https://github.com/bitemyapp/esqueleto/pull/149): Added `upsert` support.

- @parsonsmatt
  - [#133](https://github.com/bitemyapp/esqueleto/pull/133): Added `renderQueryToText` and related functions.

3.1.0
=======

- @Vlix
  - [#128](https://github.com/bitemyapp/esqueleto/pull/128): Added `Database.Esqueleto.PostgreSQL.JSON` module with JSON operators and `JSONB` data type.
- @ibarrae
  - [#127](https://github.com/bitemyapp/esqueleto/pull/127): Added `between` and support for composite keys in `unsafeSqlBinOp`.

3.0.0
=======

- @parsonsmatt
  - [#122](https://github.com/bitemyapp/esqueleto/pull/122): Support `persistent-2.10.0`. This is a breaking change due to the removal of deprecated exports from the `persistent` library.
  - [#113](https://github.com/bitemyapp/esqueleto/pull/113): Remove the `esqueleto` type class. To migrate here, use `SqlExpr`, `SqlQuery`, and `SqlBackend` instead of using the polymorphic `Esqueleto sqlExpr sqlQuery sqlBackend => ...` types.

2.7.0
=======

- @parsonsmatt
  - [#117](https://github.com/bitemyapp/esqueleto/pull/117): Removed `sqlQQ` and `executeQQ` functions from export, fixing doc build and building with `persistent` >= 2.9

2.6.1
=======

- @ChrisCoffey
  - [#114](https://github.com/bitemyapp/esqueleto/pull/114): Fix Haddock by
    working around an upstream bug.

2.6.0
========
- @bitemyapp
  - Reorganized dependencies, decided to break compatibility for Conduit 1.3, Persistent 2.8, and `unliftio`.
  - Moved tests for `random()` into database-specific test suites.
  - Deprecated Language `random_`, split it into database-specific modules.
- @parsonsmatt
  - Added support for `PersistQueryRead`/`PersistQueryWrite`, enabling type-safe differentation of read and write capabilities.
    - https://github.com/bitemyapp/esqueleto/pull/66
- @sestrella
  - Added support for `arrayAggDistinct` and `arrayRemove`.
    - https://github.com/bitemyapp/esqueleto/pull/65
    - https://github.com/bitemyapp/esqueleto/pull/66
- @mheinzel
  - Fixed JOIN syntax in the documentation https://github.com/bitemyapp/esqueleto/pull/60
- @illmade
  - Added instructions for running database specific tests
    - https://github.com/bitemyapp/esqueleto/pull/64
- @FintanH
  - Removed CPP from the test suite, split the database-specific tests into their own respective modules.
    - https://github.com/bitemyapp/esqueleto/pull/48
  - Added support for PostgreSQL's `now()`
    - https://github.com/bitemyapp/esqueleto/pull/46
  - Added a comprehensive examples project to make practical application of Esqueleto easier.
    - https://github.com/bitemyapp/esqueleto/pull/40
- @EdwardBetts
  - Fixed a spelling error
    - https://github.com/bitemyapp/esqueleto/pull/52
