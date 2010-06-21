---
title: Defining Entities - Persistent
---
Remember that Persistent is non-relational. That makes our job in defining entities much simpler: we simply work one entity at a time. (We'll describe [later on](relations.html) how to model relations.)

A single entity consists of:

* The name of the entity. This must be a valid name for a Haskell datatype.

* A list of fields.

* A list of unique keys.

* A list of typeclasses that the entity should derive.

A field in turn consists of:

* A field name. This must begin with a lower case letter.

* A datatype.

* A list of attributes.

And finally a unique key consists of:

* A constructor name.

* A list of fields included in the key.

### EntityDef

[EntityDef](http://docs.yesodweb.com/haddock/persistent/Database-Persist-Helper.html#t%3AEntityDef) is the most direct means of defining an entity. This is the data type used as input for all the template haskell code.

### Quasi-quotation

As a convenience, there is also a quasi-quoted syntax which is a little easier to use. It is merely a simpler way to define your entities. The quasiquoter is the [persist](http://docs.yesodweb.com/haddock/persistent/Database-Persist-Quasi.html) function.

The syntax is very straight forward: each non-indented line is the name of an entity. All indented lines following it belong to that entity.

* If a line begins with the word "deriving", then all of the following words are typeclasses which the entity should derive.

* If a line begins with an upper-case letter, the first word is a unique key constructor name and the remaining words are field names to be included in the unique key.

* Otherwise, a line declares a new field. The first word is the field name, the second word is the data type for that field, and each of the remaining words is an attribute.

### Attributes

Attributes are purposely defined as simple strings so that other tools can define their own attributes. The following attributes are used by Persistent itself:

* null: A field can be nullable. In Haskell terms, this means that the datatype will be wrapped in a Maybe.

* update: A field which can be updated. This will signal the template haskell code to create an appropriate update constructor. If the entity is named Person and the field is named age, the update constructor would be PersonAge.

* Asc, Desc: A field sortable in either ascending or descending order. The resulting constructor names would be PersonAgeAsc and PersonAgeDesc.

* Eq, Ne, Lt, Gt, Le, Ge: A field which can be filtered using one of these comparisons (equals, not equals, less than, etc). Constructor names are PersonAgeEq, PersonAgeNe, etc.
