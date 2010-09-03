-- START
{-# LANGUAGE TypeFamilies, QuasiQuotes #-}
import Yesod
data Names = Names
mkYesod "Names" [$parseRoutes|/name/#String NameR GET|]
instance Yesod Names where approot _ = ""
getNameR name = return $ RepPlain $ toContent $ "Hello " ++ name
main = basicHandler 3000 Names
-- STOP
