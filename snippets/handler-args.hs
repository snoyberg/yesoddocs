{-# LANGUAGE TypeFamilies, QuasiQuotes #-}
import Yesod
data Args = Args
-- START
type Handler = GHandler Args Args

mkYesod "Args" [$parseRoutes|
/person/#String PersonR GET
/year/#Integer/month/#String/day/#Int DateR
/wiki/*Strings WikiR GET
|]

getPersonR :: String -> Handler RepHtml
getPersonR name = defaultLayout [$hamlet|%h1 Hello $name$!|]

handleDateR :: Integer -> String -> Int -> Handler RepPlain -- text/plain
handleDateR year month day =
    return $ RepPlain $ toContent $
        month ++ " " ++ show day ++ ", " ++ show year

getWikiR :: [String] -> Handler RepPlain
getWikiR = return . RepPlain . toContent . unwords
-- STOP
instance Yesod Args where approot _ = ""
main = basicHandler 3000 Args
