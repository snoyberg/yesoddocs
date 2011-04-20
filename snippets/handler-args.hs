{-# LANGUAGE TypeFamilies, QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import Yesod
import Data.Text (Text)
import qualified Data.Text as T
data Args = Args
-- START
type Handler = GHandler Args Args

type Texts = [Text]
mkYesod "Args" [$parseRoutes|
/person/#Text PersonR GET
/year/#Integer/month/#Text/day/#Int DateR
/wiki/*Texts WikiR GET
|]

getPersonR :: Text -> Handler RepHtml
getPersonR name = defaultLayout [$hamlet|<h1>Hello #{name}!|]

handleDateR :: Integer -> Text -> Int -> Handler RepPlain -- text/plain
handleDateR year month day =
    return $ RepPlain $ toContent $
        T.concat [month, " ", T.pack $ show day, ", ", T.pack $ show year]

getWikiR :: [Text] -> Handler RepPlain
getWikiR = return . RepPlain . toContent . T.unwords
-- STOP
instance Yesod Args where approot _ = ""
main = warpDebug 3000 Args
