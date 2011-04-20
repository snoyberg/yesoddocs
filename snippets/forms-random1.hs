-- START
{-# LANGUAGE QuasiQuotes, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell #-}
import Yesod
import System.Random
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T

data Rand = Rand
type Handler = GHandler Rand Rand

mkYesod "Rand" [$parseRoutes|
/ RootR GET
|]

instance Yesod Rand where
    approot _ = ""

data Params = Params
    { minNumber :: Int
    , maxNumber :: Int
    , singleWord :: Text
    , pluralWord :: Text
    }

paramsFormlet :: Maybe Params -> Form s m Params
-- Same as: paramsFormlet :: Formlet s m Params
paramsFormlet mparams = fieldsToTable $ Params
    <$> intField "Minimum number" (fmap minNumber mparams)
    <*> intField "Maximum number" (fmap maxNumber mparams)
    <*> stringField "Single word" (fmap singleWord mparams)
    <*> stringField "Plural word" (fmap pluralWord mparams)

getRootR :: Handler RepHtml
getRootR = do
    (res, form, enctype) <- runFormGet $ paramsFormlet Nothing
    output <-
        case res of
            FormMissing -> return "Please fill out the form to get a result."
            FormFailure _ -> return "Please correct the errors below."
            FormSuccess (Params min max single plural) -> do
                number <- liftIO $ randomRIO (min, max)
                let word = if number == 1 then single else plural
                return $ T.concat ["You got ", T.pack $ show number, " ",  word]
    defaultLayout [$hamlet|
<p>#{output}
<form enctype="#{enctype}">
    <table>
        ^{form}
        <tr>
            <td colspan="2">
                <input type="submit" value="Randomize!">
|]
-- STOP

main = warpDebug 3001 Rand
