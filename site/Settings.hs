{-# LANGUAGE TemplateHaskell #-}
module Settings where

import qualified Text.Hamlet
import qualified Text.Cassius
import qualified Text.Julius
import Yesod.Helpers.Static
import Language.Haskell.TH.Syntax (Q, Exp)

hamletFile :: String -> Q Exp
hamletFile x = Text.Hamlet.hamletFile $ "hamlet/" ++ x ++ ".hamlet"

cassiusFile :: String -> Q Exp
cassiusFile x = Text.Cassius.cassiusFileDebug $ "cassius/" ++ x ++ ".cassius"

juliusFile :: String -> Q Exp
juliusFile x = Text.Julius.juliusFileDebug $ "julius/" ++ x ++ ".julius"

staticFiles "static"
