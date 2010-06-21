---
title: Relations - Persistent
---
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
