-- START
{-# LANGUAGE TypeFamilies, QuasiQuotes #-}
import Yesod
import Yesod.Helpers.Static

data StaticExample = StaticExample
    { getStatic :: Static
    }

-- This will create an application which only serves static files.
mkYesod "StaticExample" [$parseRoutes|
/ StaticR Static getStatic
|]
instance Yesod StaticExample where approot _ = ""
main = basicHandler 3000 $ StaticExample $ fileLookupDir "static" typeByExt
-- STOP
