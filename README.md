# Esqueleto

Esqueleto is a bare bones, type-safe EDSL for SQL queries that works with unmodified persistent SQL backends. The name of this library means "skeleton" in Portuguese and contains all three SQL letters in the correct order =). It was inspired by Scala's Squeryl but created from scratch. Its language closely resembles SQL. Currently, SELECTs, UPDATEs, INSERTs and DELETEs are supported. Not all SQL features are available, but most of them can be easily added (especially functions).

Persistent is a library for type-safe data serialization. It has many kinds of backends, such as SQL backends (persistent-mysql, persistent-postgresql, persistent-sqlite) and NoSQL backends (persistent-mongoDB). While persistent is a nice library for storing and retrieving records, including with filters, it does not try to support some of the features that are specific to SQL backends. In particular, esqueleto is the recommended library for type-safe JOINs on persistent SQL backends. (The alternative is using raw SQL, but that's error prone and does not offer any composability.). For more information read [esqueleto](http://hackage.haskell.org/package/esqueleto).



