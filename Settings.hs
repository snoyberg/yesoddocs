module Settings where

import qualified Text.Hamlet
import qualified Text.Cassius
import qualified Text.Julius
import Yesod.Helpers.Static

hamletFile x = Text.Hamlet.hamletFileDebug $ "hamlet/" ++ x ++ ".hamlet"
cassiusFile x = Text.Cassius.cassiusFileDebug $ "cassius/" ++ x ++ ".cassius"
juliusFile x = Text.Julius.juliusFileDebug $ "julius/" ++ x ++ ".julius"

staticFiles "static"
