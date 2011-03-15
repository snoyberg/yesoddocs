{-# LANGUAGE QuasiQuotes, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell #-}
import Yesod
import Yesod.Form.Jquery
import Yesod.Form.Nic
import Data.Time
import Control.Applicative
import Data.List
data Js = Js
type Handler = GHandler Js Js
mkYesod "Js" [$parseRoutes|
/ RootR GET
/colors ColorsR GET
|]
instance Yesod Js where approot _ = ""
-- START
instance YesodJquery Js
instance YesodNic Js
data Survey = Survey
    { birthday :: Maybe Day
    , favoriteColor :: String
    , aboutMe :: Html
    }

surveyFormlet msurvey = fieldsToTable $ Survey
    <$> maybeJqueryDayField def "My birthday" (fmap birthday msurvey)
    <*> jqueryAutocompleteField ColorsR "Favorite color" (fmap favoriteColor msurvey)
    <*> nicHtmlField "About me"
            { ffsId = Just "about-me"
            } (fmap aboutMe msurvey)

getRootR = do
    (res, form, enctype) <- runFormGet $ surveyFormlet Nothing
    let msurvey = case res of
                    FormSuccess x -> Just x
                    _ -> Nothing
    defaultLayout $ do
        addCassius [$cassius|#about-me
    width: 400px
    height: 300px
|]
        [$hamlet|
$maybe survey <- msurvey
    <h3>Previous Entries
    $maybe bday <- birthday survey
        <p>Born on #{show bday}
    <p>Favorite color: #{favoriteColor survey}
    #{aboutMe survey}
<form enctype="#{enctype}">
    <table>
        ^{form}
        <tr>
            <td colspan="2">
                <input type="submit">
|]
-- STOP

getColorsR = do
    term <- runFormGet' $ stringInput "term"
    jsonToRepJson $ jsonList $ map jsonScalar $ filter (isPrefixOf term) colors
  where
    colors = words "red orange yellow green blue purple black brown"

main = warpDebug 3001 Js
