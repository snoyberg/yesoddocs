---
title: Persistent
---
This chapter has not yet been written properly. Instead, it's a conglomeration of the old content from the persistent section. It will be cleaned up as the book is written.

[The persistent synopsis]($root/persistent/synopsis.html) gives an overview of how the library is used.

## Overview

Persistent is a persistence layer with a huge emphasis on type safety. It uses template haskell to automate the boilerplate production of data types and constructors that allow you to do regular database activities with full compile-time guarantees of type correctness.

It uses a non-relational approach for its API. This is not to say SQL is unsupported; on the contrary, SQL is a first-class target of the library. Instead, it means you are free to break out of the SQL mold and use non-relational databases as a backend. A proof-of-concept Redis backend is in the making.

While persistent is designed to integrate tightly with Yesod, it is quite usable in other contexts.

To use Persistent, you must also use a backend, which provides database-specific code. The currently available backends are:

* [persistent-sqlite 0.2.0](http://hackage.haskell.org/package/persistent-sqlite-0.2.0)

* [persistent-postgresql 0.2.0](http://hackage.haskell.org/package/persistent-postgresql-0.2.0)

There is currently work on both MongoDB and Redis backends being done.

<h2 id="defining-entities">Defining Entities</h2>

Remember that Persistent is non-relational. That makes our job in defining entities much simpler: we simply work one entity at a time. (We'll describe [later on](#relations) how to model relations.)

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

## Type classes

**NOTE: This documentation still reflects persistent version 0.0.0. It will be updated soon.**

The API for Persistent really consists of just two type classes: [PersistField](http://docs.yesodweb.com/haddock/persistent/Database-Persist.html#t%3APersistField) and [PersistEntity](http://docs.yesodweb.com/haddock/persistent/Database-Persist.html#t%3APersistEntity). The former is for individual fields which can be included in an entity; the latter is for entities which are comprised of a number of fields.

### PersistField

There are four functions in PersistField. <code>isNullable</code> indicates that a field could store a null value; the default definition returns <code>False</code>. Most users will never need to override this, as usually only the <code>Maybe</code> would have a result of <code>True</code>.

<code>sqlType</code> could in theory be part of a separate type class, but is included for convenience. This specifies the SQL datatype corresponding to this field. For example, <code>sqlType String</code> returns <code>SqlString</code>. You can see the [available SqlType constructors](http://docs.yesodweb.com/haddock/persistent/Database-Persist.html#t%3ASqlType).

The other two functions are <code>toPersistValue</code> and <code>fromPersistValue</code>. The [PersistValue](http://docs.yesodweb.com/haddock/persistent/Database-Persist.html#t%3APersistValue) datatype is very similar to HDBC's SqlValue datatype: it allows us to have heterogenous lists of values. By creating proper instances of PersistField, Persistent is able to handle all of the marshalling for you automatically, and you never need to worry about data type issues.

### PersistEntity

This is the heart of Persistent. It really has two components: a set of associated data types (see [type families](http://www.haskell.org/haskellwiki/GHC/Type_families)) and functions.

The functions are the bare minimum necessary to implement <abbr title="Create Read Update Delete">CRUD</abbr> operations:

* Create: insert and replace

* Read: get (for a single entity by its id), getBy (for a single entity by a unique key) and select (for multiple entities)

* Update: update (by id) and updateWhere (by filter)

* Delete: delete (by id), deleteBy (by unique key) and deleteWhere (by filter)

Additionally, there is an initialize function for setting up a database, where necessary. For example, in SQL backends this creates tables and indices if they do not exist.

The more exciting part of PersistEntity is the associated data types. Let's go through them:

<ul>

<li>PersistMonad is a *monad transformer* within which all database operations must occur. For example, the sqlite backend has a transformer called SqliteReader, which is simply a ReaderT monad transformer holding an sqlite database connection. Each backend provides its own mechanism for unwrapping this monad transformer.</li>

<li>Key is a datatype for the unique id for each entity. In the current backends, this is just a newtype wrapper around an Int (or Int64). However, since each entity has its own newtype wrapper, everything is fully typesafe. When you write "get (personKey :: Key Person)", the result *must* be a Person.</li>

<li>Update allows type-safe updates to entities. Let's give a little example; the same logic here applies to the remaining associated types. Continuing with our running example of a Person entity defined (using [quasi-quoted syntax](#defining-entities)) as:

    Person
        name String Eq
        age Int update Asc
        UniquePerson name

We would end up with something akin to:

    data Person = Person
        { personName :: String
        , personAge :: Int
        }
    data Update Person = PersonAge Int

So if we want to write a function to change someone's age to 32, we could write:

    fountainOfYouth :: Key Person -> PersistMonad Person IO ()
    fountainOfYouth personId = update personId [PersonAge 32]
</li>

<li>Filter is used for (duh) filtering results. In SQL, this is the equivalent of a WHERE clause. Taking the same example from above, we would have generated:

    data Filter Person = PersonNameEq String

To delete every Michael in the system:

    deleteWhere [PersonNameEq "Michael"]
</li>

<li>Order is only used in the select function, and corresponds to a SQL ORDER BY clause. From above:

    data Order Person = PersonAgeAsc

Notice how there is no argument to that constructor. To get everyone in ascending age, we'd write:

    select [] [PersonAgeAsc]

Or to get everyone named Michael in ascending age:

    select [PersonNameEq "Michael"] [PersonAgeAsc]
</li>

<li>Unique is used by the getBy and deleteBy functions. You could get the same effect by using select and deleteWhere, but these variants are powerful in that they ensure you will be acting upon at most one value. From the example above:

    data Unique Person = UniquePerson String

So to get the one and only Michael out there, you'd type:

    getBy $ UniquePerson "Michael"

which is really equivalent to:

    listToMaybe <$> select [PersonNameEq "Michael"] []
</li>
</ul>

<h2 id="relations>Relations</h2>

Persistent itself is non-relational. However, data *is* relational. As a motivating example, let's discuss a blog application, with multiple authors. Each author can have multiple entries; how does this work in Persistent?

The idea is to design things exactly as you would with SQL: for a many-to-one relationship, you'll store a reference to the "one" entity in the "many" entity. So in our example, we could define our model using quasi-quoted syntax as:

    Author
        name String Asc
        tagline String
        birthday Day
    Entry
        author AuthorId Eq
        title String
        content Html
        posted UTCTime Desc

Note: Persistent automatically declares the type synonym <code>type AuthorId = Key Author</code>.

So let's explain the non-relationality of Persistent again: Persistent doesn't do joins. The data structure itself, however, can be completely relational. The burden is on the programmer to do this.

Let's say you want to create a list of author/title pairs; the following code will do it for you:

    authors <- select [] [AuthorNameAsc]
    concat <$> forM authors $ \(authorid, author) -> do
        entries <- select [EntryAuthorEq authorid] [EntryPostedDesc]
        return $ zip (repeat $ authorName author) $ map entryTitle entries

Alternatively, you could do the following (which will sort things a little differently):

    entries <- select [] [EntryPostedDesc]
    forM entries $ \(_entryid, entry) -> do
        author <- get $ entryAuthor entry
        case author of
            Nothing -> error $ "Missing author id: " ++ show (entryAuthor entry)
            Just a -> return (authorName a, entryTitle entry)
