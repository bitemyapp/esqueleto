3.3.3.0
========
- @belevy
  - [#177](https://github.com/bitemyapp/esqueleto/pull/172) - Introduce new
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
