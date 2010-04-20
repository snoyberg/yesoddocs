---
title: Synopsis - Hamlet
---
\begin{code}
{-# LANGUAGE QuasiQuotes #-}

import Text.Hamlet
import Data.Text (pack)

data Person = Person
    { name :: IO HtmlContent -- maybe it requires a database lookup
    , age :: HtmlContent
    , page :: PersonUrls
    , isMarried :: Bool
    , children :: [HtmlContent]
    }
data PersonUrls = Homepage | PersonPage String

renderUrls :: PersonUrls -> String
renderUrls Homepage = "/"
renderUrls (PersonPage name) = '/' : name

footer :: Monad m => a -> Hamlet url m ()
footer = [$hamlet|
#footer Thank you, come again
|]

template :: Person -> Hamlet PersonUrls IO ()
template = [$hamlet|
!!!
%html
    %head
        %title Hamlet Demo
    %body
        %h1 Information on $*name$
        %p $*name$ is $age$ years old.
        %h2
            $if isMarried
                Married
            $else
                Not married
        %ul
            $forall children child
                %li $child$
        %p
            %a!href=@page@ See the page.
        ^footer^
|]

main :: IO ()
main = do
    let person = Person
            { name = return $ Unencoded $ pack "Michael"
            , age = Unencoded $ pack "twenty five & a half"
            , page = PersonPage "michael"
            , isMarried = True
            , children = [ Unencoded $ pack "Adam"
                         , Unencoded $ pack "Ben"
                         , Unencoded $ pack "Chris"
                         ]
            }
    printHamlet renderUrls $ template person
\end{code}

Outputs (new lines added for readability):
<code><pre>
    &lt;!DOCTYPE html&gt;
    &lt;html&gt;&lt;head&gt;&lt;title&gt;Hamlet Demo&lt;/title&gt;&lt;/head&gt;&lt;body&gt;
    &lt;h1&gt;Information on Michael&lt;/h1&gt;
    &lt;p&gt;Michael is twenty five &amp; a half years old.&lt;/p&gt;
    &lt;h2&gt;Married&lt;/h2&gt;
    &lt;ul&gt;&lt;li&gt;Adam&lt;/li&gt;&lt;li&gt;Ben&lt;/li&gt;&lt;li&gt;Chris&lt;/li&gt;&lt;/ul&gt;
    &lt;p&gt;&lt;a href="/michael"&gt;See the page.&lt;/a&gt;&lt;/p&gt;
    &lt;div id="footer"&gt;Thank you, come again&lt;/div&gt;
    &lt;/body&gt;&lt;/html&gt;
</pre></code>
