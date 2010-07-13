---
title: Type classes - Persistent
---
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

<li>Update allows type-safe updates to entities. Let's give a little example; the same logic here applies to the remaining associated types. Continuing with our running example of a Person entity defined (using [quasi-quoted syntax](defining-entities.html)) as:

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
