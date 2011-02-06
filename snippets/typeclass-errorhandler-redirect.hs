{-# LANGUAGE TypeFamilies, QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Yesod
data Layout = Layout
mkYesod "Layout" [$parseRoutes|/ RootR GET|]
instance Yesod Layout where
    approot _ = ""
-- START
    errorHandler NotFound = redirect RedirectTemporary RootR
    errorHandler other = defaultErrorHandler other
-- STOP
getRootR = defaultLayout [$hamlet|\Hello World
|]
main = warpDebug 4000 Layout
