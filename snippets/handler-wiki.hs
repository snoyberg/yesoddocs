{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell #-}
import Yesod
data Fibs = Fibs
-- START
data Page = Page String String [String] -- 2 or more
instance MultiPiece Page where
    toMultiPiece (Page x y z) = x : y : z
    fromMultiPiece (x:y:z) = Right $ Page x y z
    fromMultiPiece _ = Left "I need at least 2 pieces"
-- STOP
main = return ()
