---
title: Yesod Web Framework Overview
---
### Type-safe, RESTful, performant Haskell web framework

The Yesod web framework is broken into multiple pieces to allow users to pick and choose which pieces they would like to use in their own development. If you want to go with the whole package, please begin with [the Yesod documentation]($root/yesod/).

This site provides documentation for a number of the underlying projects which provide the foundation for Yesod. Here's a high-level overview of how everything fits together:

<table>
<thead><tr><th>Package</th><th>Purpose</th></tr></thead>
<tbody>
<tr>
<th>[wai](http://hackage.haskell.org/package/wai)</th>
<td>The Web Application Interface provides a standard, efficient interface between web applications and web servers. By targetting the WAI, Yesod applications can run via CGI, FastCGI or standalone servers.</td>
</tr>
<tr>
<th>[wai-extra](http://hackage.haskell.org/package/wai-extra)</th>
<td>wai-extra provides some basic functionality, such as GZIP compression, parameter parsing and a test server. All components handle data is a memory-efficient manner without resorting to lazy I/O.</td>
</tr>
<tr>
<th>[web-routes-quasi]($root/web-routes-quasi/)</th>
<td>web-routes-quasi enables fully type-safe URLs with a very concise syntax. Never again need you splice together strings to form a URL; instead, you let the type system do the heavy lifting for you.</td>
</tr>
<tr>
<th>[hamlet]($root/hamlet/)</th>
<td>Haml-inspired layout combined with Haskell type safety and enumerators for performance. This package combines very well with web-routes-quasi by allow you to specify a URL datatype. It also allows embedding of other templates without storing all the data in memory, and automatic HTML entity encoding with the HtmlContent datatype.</td>
</tr>
<tr>
<th>[persistent]($root/persistent/)</th>
<td>A persistence layer targetting multiple backends. Its main feature is very strong type safety, but uses template haskell to avoid writing a lot of boilerplate code. By taking a non-relational approach to data, it can work on both SQL and non-SQL backends.</td>
</tr>
<tr>
<th>[clientsession](http://hackage.haskell.org/package/clientsession)</th>
<td>Store session data in an encrypted, checksummed cookie. This approach allows you to avoid a database lookup for each and every user request, and make it easier to scale out your application to multiple servers.</td>
</tr>
<tr>
<th>[authenticate](http://hackage.haskell.org/package/authenticate)</th>
<td>Provides 3rd-party authentication. Currently OpenId v1, RPXnow and Facebook are supported; hopefully more systems will be added in the future.</td>
</tr>
<tr>
<th>[yesod]($root/yesod/)</th>
<td>A web framework tying together all of the other pieces mentioned on this page to provide a consistent API for quickly creating type-safe, RESTful, performant web applications.</td>
</tr>
</tbody>
</table>
