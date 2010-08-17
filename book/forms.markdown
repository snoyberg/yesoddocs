---
title: Yesod Book- Forms
---
I've mentioned the boundary issue already: whenever data enters or leaves our application, we need to validate our data. Probably the most difficult place this occurs is forms. Coding forms has a lot of complexities in it; in an ideal world, we'd like a solution that addresses the following problems:

* Ensure data is valid.

* Marshal string data in the form submission to Haskell datatypes.

* Generate HTML code for displaying the form.

* Generate Javascript to do clientside validation and provide more user-friendly widgets, such as date pickers.

* Build up more complex forms by combining together simpler forms.

The form system used in Yesod is built around many of the concepts in formlets; if you are familiar with that work, it will give you a head start here. In this chapter, we'll start by building up a low level understanding of the Yesod.Form module, and then demonstrating its usage.

## What's in a form

All user-submitted data is available as either a query-string parameter or as part of the response body. (There's the request URI and request headers as well, I know, but those are not used for form parsing.) The response body can be submitted as either URL-encoded or as multi-part mime data. If the latter, the response body can contain submitted files. All three submission methods (query string, URL-encoded and multi-part) can contain simple key-value pairs.

The first simplification: we don't care about parsing both query-string and response body information at the same time. Therefore, we can look at the input to a form as the file and non-file input. This can be represented as:

    type Env = [(String, String)]
    type FileEnv = [(String, FileInfo)]

FileInfo is declared in Yesod.Request, and contains the original filename, the file's mime-type and the actual contents.

The output of a form is really three pieces of information: the result of the form, the HTML code to be displayed to the user and the encoding type required. This last one is either URL-encoded or multi-part mime data, and essentially if any field in a form is a file input, it must be multi-part; otherwise, URL-encoded will suffice. Yesod.Form has a data type:

    data EncType = UrlEncoded | Multipart

What should the other two pieces of information look like? Let's give a motivating example: a form that simply parses a single string. Either the string is there, or it isn't. So our form function might look like:

    stringForm :: Env -> (Maybe String, Html, EncType)
    stringForm env =
        let res = lookup "field-name" env
            html = "<input type='text' name='field-name'>"
         in (res, html, UrlEncoded)

However, this is very limiting. Let's say that we want to create an intForm; now there are *two* possible reasons for the form to fail: either the data was not submitted, or the submitted data is not a valid integer. We might be tempted to use an <code>Either String Int</code> here, but that has its own problems: there's no distinction made between missing data and invalid data, and there is no obvious way to combine multiple results together.

### FormResult and Applicative

Instead, we need a new datatype:

    data FormResult a = FormMissing
                      | FormFailure [String]
                      | FormSuccess a

The great thing here is that it provides a very convenient Applicative instance, which really brings us to the core of forms. Applicatives are a strictly weaker version of Monads. In particular, they don't allow the result from one computation to be used in computing another. This actually models forms very well. Let's say that we have a datatype:

    data Person = Person { name :: String, age :: Int }

Whether or not the name parses does not affect whether the age parses, and vice-versa. However, we would like to be able to combine together the *subforms* for name and age. Using an Applicative instance, this becomes:

    Person <$> name <*> age

In this manner, the errors can propogate up the chain and be combined. This allows you easy access to **all** of the validation errors present in your form.

### HTML

So how about displaying that form? As mentioned, the Html datatype seems like the obvious choice. Unfortunately, it doesn't give us much flexibility. Suppose we use just plain ol' Html for the above-mentioned Person datatype. We would end up with two snippets of Html looking like:

    <input type="text" name="name">
    <input type="number" name="age">

Now what? Do we simply concatenate them together? That will just show two input fields without any labeling.
