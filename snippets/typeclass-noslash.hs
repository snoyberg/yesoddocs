{-# LANGUAGE TypeFamilies, QuasiQuotes #-}
import Yesod
-- START
import Web.Routes.Base (encodePathInfo, decodePathInfo)
import qualified Data.ByteString.Char8 as S8
-- STOP
data NoSlash = NoSlash
mkYesod "NoSlash" [$parseRoutes|
/ RootR GET
/some/path SomePathR GET
/one/more/path.txt OneMorePathR GET
|]
-- START
instance Yesod NoSlash where
    approot _ = "http://www.example.com/foo/bar"
    joinPath _ root pieces query = root ++ '/' : encodePathInfo pieces query
    splitPath foundation orig =
        if orig == new then Right pieces else Left newComplete
      where
        pieces = filter (not . null) $ decodePathInfo $ S8.unpack orig
        new = S8.pack $ joinPath foundation "" pieces []
        newComplete = S8.pack $ joinPath foundation (approot foundation) pieces []
-- STOP
getRootR = defaultLayout [$hamlet|
%p
    %a!href=@RootR@ @RootR@
%p
    %a!href=@SomePathR@ @SomePathR@
%p
    %a!href=@OneMorePathR@ @OneMorePathR@
|]
getSomePathR = getRootR
getOneMorePathR = getRootR
main = basicHandler 4000 NoSlash
