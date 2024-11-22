

# Architecture of Esqueleto(Experimental/post GADTs)

## The 4 Languages
Esqueleto can be considered as being comprised primarily of 4 Languages, they are interconnected in fairly complex ways.
These languages are:
  - SqlQuery
  - SqlExpr
  - From
  - SqlSetOperation

The core language is the expression language expressed by SqlExpr. It is the work horse of the library, it represents values and entities
in the SQL database. If there is an operation of values it will take place in the SqlExpr language. All the other languages rely on this language.

Values need to be introduced into the query via a FROM clause. This is where the From language comes in. A From defines how to bring the data into scope.
It consists of tables, subqueries, joins, and sql set operations. To embed a From into a SqlQuery you can use `from`. Sub-queries and sql set operations are expressed 
by embeding either a SqlQuery or a SqlSetOperation into a From, this can be done directly thanks to the ToFrom type class.

A SqlSetOperation is actually more of a sub-language of From, the only way to use it is in the context of a From. It is composed of SqlQueries that have been attached 
with one of the set operations defined by the SQL spec (namely UNION, UNION ALL, INTERSECT, and EXCEPT). While the language supports all of these only 
UNION and UNION ALL are supported by some databases, the intention is to avoid getting in the users way.

All of these languages ultimately come together into a SqlQuery. The SqlQuery is responsible for reifying the query as a single `(Text, [SqlPersistValue])`
The SqlQuery has remained untouched since esqueleto-3.0 and `select` is used to run the query. SqlQuery can have multiple from clauses which will be treated as cross joins.
This is probably not ideal. All of the SqlQuery operators work in terms of SqlExpr values.

## SqlExpr
Prior to 3.5 a `SqlExpr a` was defined as a `GADT`. 
This representation was fairly limited and meant that every single feature had to find its way back into the main Internal module. When subqueries were added in 3.3 the central `GADT` had to be update to represent `AliasedValue`s, `ValueReference`s, `AliasedEntity`s, and `EntityReference`s. 
This expansion led to a proliferation of code to handle all the cases when they ultimately can be treated the same in most cases. 

At its core a `SqlExpr a` is a typed query fragment `(Builder, [SqlPersistValue])`. In addition to the main rendering the expression also carries around some pieces of metadata
These are:
  - What the alias is if the value is aliased (Maybe Builder)
	  - This is used to generate the `AS` in the select clause while allowing the value to remain unchanged for the purpose of operators. 
  - If this is an actual value or just a reference to a value
	  - A reference is merely a subquery name + an alias name. 
  - If this is a composite(i.e. composite keys) value a list of all the values that compose it
	  - This is useful for when the composite value needs to be treated as individual values like in the case of an `ORDER BY`

In addition to the metadata a SqlExpr expects to be told if its in a context where it needs parens. This inverts the relationship that existed under the old system where the SqlExpr would say if it should be parenthesized and the parent Expr would decide whether to render parentheses or not. In this approach the SqlExpr is informed if its in a parenthesized context and can decide to add parentheses or not. 

## From
In the beginning of the library there was no support for joins in the from clause, the way that joins were supported was through calling `from` multiple times in a query to get a CrossJoin(implemented in SQL as a comma). This approach relied on type inference to determine what table was being used, `from (\person -> ...)` knew to use the `Person` entity only because of how `person` was used. This type inference extended to joins, where the way that one would fetch a `Person INNER JOIN Blog` would be ```from (\(person `InnerJoin` blog) -> ...``` This approach to joins means that the `on` clause would be seperated from the join since we only ever had control of the result. 

With the new syntax introduced in 3.3 the argument to from was explicated, this leads to slightly more verbosity but opens up the whole realm of values. As a general rule of thumb, if something can be done with values then it should be done with values. The result of this was the `From` language, originally implemented using a GADT representing the different kinds of from clauses, in 3.5 it was rewritten into its "final encoding" to allow for more modularity of the `From` language. As with the `SqlExpr` there is not really a need for multiple interpretations of `From` so we can just use a single data type `From a = SqlQuery (a, RawFn)`. In short a `From` is a way to introduce variables to a `SqlQuery`, the `RawFn` provided is the content of the `FROM` clause in the SQL and is representationally equivalent to the function that lives in the `ERaw` constructor of a `SqlExpr`. Due to this new representation we are able to split out the different kinds of From values into their own modules including if we would like to add a database specific kind like `json_table`. The modules are all in the `From.` namespace with the core `table` and `selectQuery` living in the base From module. 
