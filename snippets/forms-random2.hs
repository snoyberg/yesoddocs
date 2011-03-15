{-# LANGUAGE QuasiQuotes, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell #-}
import Yesod
import Yesod.Form.Core
import System.Random
import Control.Applicative
import Safe
import Data.Maybe
import Control.Monad
data Rand = Rand
type Handler = GHandler Rand Rand
mkYesod "Rand" [$parseRoutes|
/ RootR GET
|]
instance Yesod Rand where approot _ = ""
-- START
data Params = Params
    { numberRange :: (Int, Int)
    , singleWord :: String
    , pluralWord :: String
    }

paramsFormlet :: Formlet s m Params
paramsFormlet mparams = fieldsToTable $ Params
    <$> rangeField (fmap numberRange mparams)
    <*> stringField "Single word" (fmap singleWord mparams)
    <*> stringField "Plural word" (fmap pluralWord mparams)

rangeField :: Maybe (Int, Int) -> GForm s m [FieldInfo s m] (Int, Int)
-- same as rangeField :: Maybe (Int, Int) -> FormField s m (Int, Int)
-- same as rangeField :: FormletField s m (Int, Int)
rangeField initial = GForm $ do
    minId <- newFormIdent
    minName <- newFormIdent
    maxId <- newFormIdent
    maxName <- newFormIdent
    env <- askParams
    let res =
            case env of
                [] -> FormMissing -- no data provided at all
                _ ->
                    case (lookup minName env, lookup maxName env) of
                        (Just minString, Just maxString) ->
                            case (readMay minString, readMay maxString) of
                                (Just min, Just max) ->
                                    if min > max
                                        then FormFailure ["Min is greater than max"]
                                        else FormSuccess (min, max)
                                _ -> FormFailure ["Expecting two integers"]
                        _ -> FormFailure ["Range is required"]
    let minValue = fromMaybe "" $ lookup minName env `mplus` fmap (show . fst) initial
    let maxValue = fromMaybe "" $ lookup maxName env `mplus` fmap (show . snd) initial
    let fi = FieldInfo
            { fiLabel = "Number range"
            , fiTooltip = ""
            , fiIdent = minId -- for attribute of the label
            , fiInput = [$hamlet|
Between #
<input id="#{minId}" name="#{minName}" type="number" value="#{minValue}">
 and #
<input id="#{maxId}" name="#{maxName}" type="number" value="#{maxValue}">
|]
            , fiErrors =
                case res of
                    FormFailure [x] -> Just $ string x
                    _ -> Nothing
            , fiRequired = True
            }
    return (res, [fi], UrlEncoded)
-- STOP

getRootR :: Handler RepHtml
getRootR = do
    (res, form, enctype) <- runFormGet $ paramsFormlet Nothing
    output <-
        case res of
            FormMissing -> return "Please fill out the form to get a result."
            FormFailure _ -> return "Please correct the errors below."
            FormSuccess (Params range single plural) -> do
                number <- liftIO $ randomRIO range
                let word = if number == 1 then single else plural
                return $ "You got " ++ show number ++ " " ++ word
    defaultLayout $ do
        addCassius [$cassius|input[type=number]
    width: 50px
.errors
    color: red
|]
        [$hamlet|\
<p>#{output}
<form enctype="#{enctype}">
    <table>
        \^{form}
        <tr>
            <td colspan="2">
                <input type="submit" value="Randomize!">
|]

main = warpDebug 3001 Rand
