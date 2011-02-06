{-# LANGUAGE TypeFamilies, QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Yesod
data Layout = Layout
mkYesod "Layout" [$parseRoutes|/ RootR GET|]
instance Yesod Layout where
    approot _ = ""
-- START
    errorHandler NotFound = fmap chooseRep $ defaultLayout $ do
        setTitle "Request page not located"
        addWidget [$hamlet|\
<h1>Not Found
<p>We appologize for the inconvenience, but the requested page could not be located.
|]
    errorHandler other = defaultErrorHandler other
-- STOP
getRootR = defaultLayout [$hamlet|\Hello World
|]
main = warpDebug 4000 Layout
