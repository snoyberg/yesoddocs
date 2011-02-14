{-# LANGUAGE TypeFamilies, QuasiQuotes, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Yesod
data Fibs = Fibs
-- START
newtype Natural = Natural Int -- we might even like to go with Word here
-- STOP
    deriving (Show, Read, Eq, Num, Ord)
-- START
instance SinglePiece Natural where
    toSinglePiece (Natural i) = show i
    fromSinglePiece s =
        case reads s of
            (i, _):_
                | i < 1 -> Left "0 and negative numbers are unnatural"
                | otherwise -> Right $ Natural i
            [] -> Left "That's not even an integer!"
-- STOP
mkYesod "Fibs" [$parseRoutes|
/fibs/#Natural FibsR GET
|]
instance Yesod Fibs where approot _ = ""
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
getFibsR :: Natural -> GHandler Fibs Fibs RepPlain
getFibsR (Natural i) = return $ RepPlain $ toContent $ show $ fibs !! (i - 1)
main = warpDebug 3000 Fibs
