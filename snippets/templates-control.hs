{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses #-}
import Yesod
data HelloWorld = HelloWorld
mkYesod "HelloWorld" [$parseRoutes|/ HomeR GET|]
instance Yesod HelloWorld where approot _ = ""

getHomeR = defaultLayout $ do
-- START
    let people = ["Michael", "Miriam", "Eliezer", "Gavriella"]
    let isFather x = x == "Michael"
        isMother x = x == "Miriam"
    let getAge "Michael" = Just 26
        getAge "Eliezer" = Just 3
        getAge _         = Nothing
    [$hamlet|
<h1>People
<ul
    $forall person <- people
        <li>
            <b>#{person}
            \ #

            $if isFather person
                This is the father of the family.
            $elseif isMother person
                This is the mother of the family.
            $else
                This is a child.

            \ #

            $maybe age <- getAge person
                This person is #{show age} years old.
            $nothing
                I do not know how old this person is.
|]
-- STOP
main = warpDebug 3000 HelloWorld
