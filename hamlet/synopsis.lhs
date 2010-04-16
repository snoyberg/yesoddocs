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
    <!DOCTYPE html>
    <html><head><title>Hamlet Demo</title></head><body>
    <h1>Information on Michael</h1>
    <p>Michael is twenty five &amp; a half years old.</p>
    <h2>Married</h2>
    <ul><li>Adam</li><li>Ben</li><li>Chris</li></ul>
    <p><a href="/michael">See the page.</a></p>
    <div id="footer">Thank you, come again</div>
    </body></html>
