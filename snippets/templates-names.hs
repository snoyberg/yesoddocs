{-# LANGUAGE TypeFamilies, QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Yesod
data Names = Names
mkYesod "Names" [$parseRoutes|/name/#String NameR GET|]
instance Yesod Names where approot _ = ""
-- START
getNameR name = hamletToRepHtml [$hamlet|\
<h1>Welcome to the Names Webapp
<p>You want me to tell you your name? Fine, it's #{name}.
|]
-- STOP
main = warpDebug 3000 Names
