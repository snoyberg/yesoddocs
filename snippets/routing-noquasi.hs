{-# LANGUAGE QuasiQuotes, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell #-}
-- START
import Yesod
import Web.Routes.Quasi.Parse
data NoQuasi = NoQuasi
mkYesod "NoQuasi"
    [ Resource "HomeR" [] ["GET"]
    , Resource "NameR" [ StaticPiece "name"
                       , SinglePiece "String"
                       ] ["GET"]
    ]
getHomeR = defaultLayout $ addHamlet [$hamlet|\Hello
|]
getNameR name = defaultLayout $ addHamlet [$hamlet|\Hello #{name}
|]
-- STOP
instance Yesod NoQuasi where approot _ = ""
main = warpDebug 3001 NoQuasi
