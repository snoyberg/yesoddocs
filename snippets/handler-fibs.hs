{-# LANGUAGE TypeFamilies, QuasiQuotes, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import Yesod
import qualified Data.Text as T
import Web.Routes.Quasi
data Fibs = Fibs
-- START
newtype Natural = Natural Int -- we might even like to go with Word here
-- STOP
    deriving (Show, Read, Eq, Num, Ord)
-- START
instance SinglePiece Natural where
    toSinglePiece (Natural i) = T.pack $ show i
    fromSinglePiece s =
        case reads $ T.unpack s of
            (i, _):_
                | i < 1 -> Nothing
                | otherwise -> Just $ Natural i
            [] -> Nothing
-- STOP
mkYesod "Fibs" [$parseRoutes|
/fibs/#Natural FibsR GET
|]
instance Yesod Fibs where approot _ = ""
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
getFibsR :: Natural -> GHandler Fibs Fibs RepPlain
getFibsR (Natural i) = return $ RepPlain $ toContent $ show $ fibs !! (i - 1)
main = warpDebug 3000 Fibs
