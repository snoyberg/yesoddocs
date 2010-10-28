{-# LANGUAGE TypeFamilies, QuasiQuotes, OverloadedStrings #-}
import Yesod
data Layout = Layout
mkYesod "Layout" [$parseRoutes|/ RootR GET|]
instance Yesod Layout where
    approot _ = ""
-- START
    errorHandler NotFound = redirect RedirectTemporary RootR
    errorHandler other = defaultErrorHandler other
-- STOP
getRootR = defaultLayout [$hamlet|Hello World|]
main = basicHandler 4000 Layout
