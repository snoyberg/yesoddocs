---
title: Terminology -- Yesod
---
By familiarizing yourself with some of the terminology for Yesod you'll get a fairly good grasp for how the system works. It will also make it easier to give more specific details in the following sections.

Every application must define a datatype around which everything is based. This is a great location to store settings loaded from config files, memory-cached data and database connections. We will call this the **site argument**.

Each site also has an associated datatype (via type families) for each URL. This is called the **site routes**. Each constructor for the site routes datatype is a **resource pattern**. For example, in a blog site, there would be a resource pattern for all blog entries.

A specific value of the site routes datatype is a **resource**. In the blog site example, this might be a single blog entry. In code, you would have the following:

    data BlogRoutes = BlogHome | BlogEntry String

Here, there is only one possible resource for the BlogHome resource pattern. BlogEntry, on the other hand, has an infinite number of possible resources.

Each resource has a one-to-one relationship with a URL. Let's say that our **application root** was *http://www.example.com/*, then we might have the following:

    render BlogHome == "http://www.example.com/"
    render (BlogEntry "first-post") == "http://www.example.com/entry/first-post/"

Each request to a Yesod application begins with the requested path info being parsed and converted to a resource. If that fails, we respond with a 404 error. Otherwise, we call the appropriate **handler function**. The handler function can either return a special response, such as a redirect, or a normal response.

Instead of returning raw data, a handler returns something which can have multiple **representations**. A representation is the response geared for a specific content type. For example, you could represent the list "foo, bar, baz" in HTML as:

    <ul><li>foo</li><li>bar</li><li>baz</li></ul>

or JSON:

    ["foo","bar","baz"]

Yesod determines, based on request headers, which representation to serve to the user.
