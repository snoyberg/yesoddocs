{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import Yesod
import Data.Text (Text)
data Fibs = Fibs
-- START
data Page = Page Text Text [Text] -- 2 or more
instance MultiPiece Page where
    toMultiPiece (Page x y z) = x : y : z
    fromMultiPiece (x:y:z) = Just $ Page x y z
    fromMultiPiece _ = Nothing
-- STOP
main = return ()
