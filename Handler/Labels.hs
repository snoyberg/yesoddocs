{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Labels
    ( getLabelsR
    , postLabelsR
    , postNewLabelR
    , LTree (..)
    , getLTree
    ) where

import Wiki
import Control.Monad (unless)
import Data.Aeson (json, Value (..))
import Data.Text.Encoding (encodeUtf8)
import Data.Attoparsec (parse, maybeResult)
import qualified Data.Map as Map
import qualified Data.Vector as V

newLabelForm :: Handler ((FormResult Text, Widget ()), Enctype)
newLabelForm = runFormPost $ renderTable $ id
    <$> areq textField (FieldSettings MsgLabelName Nothing Nothing Nothing) Nothing

data LTree = LTree
    { lid :: LabelId
    , lname :: Text
    , lchildren :: [LTree]
    }

showLTree :: [LTree] -> Widget ()
showLTree ls = [whamlet|
<ul>
    $forall l <- ls
        <li #label#{toSinglePiece $ lid l}>
            <span>#{lname l}
            ^{showLTree $ lchildren l}
|]

getLTree :: Handler [LTree]
getLTree = do
    allLabels <- runDB $ selectList [] [] 0 0
    return $ map (go allLabels) $ filter (filt Nothing) (allLabels :: [(LabelId, Label)])
  where
    filt :: Maybe LabelId -> (LabelId, Label) -> Bool
    filt x (_, y) = labelParent y == x
    go :: [(LabelId, Label)] -> (LabelId, Label) -> LTree
    go m (lid', l) = LTree lid' (labelName l) $ map (go m) $ filter (filt $ Just lid') m

getLabelsR :: Handler RepHtml
getLabelsR = do
    (_, user) <- requireAuth
    unless (userAdmin user) $ permissionDenied ""
    ((_, newform), _) <- newLabelForm
    ltree <- getLTree
    defaultLayout $ do
        addScript $ StaticR jquery_js
        addLucius $(luciusFile "edit-map")
        $(widgetFile "labels")

postLabelsR :: Handler ()
postLabelsR = do
    text <- runInputPost $ ireq textField "tree"
    case maybeResult (parse json $ encodeUtf8 text) >>= go of
        Nothing -> invalidArgsI [MsgInvalidJsonInput]
        Just x -> do
            runDB $ mapM_ (fix Nothing) x
            setMessageI MsgLabelsUpdated
            redirect RedirectTemporary LabelsR
  where
    fix parent lt = do
        update (lid lt) [LabelParent parent]
        mapM_ (fix $ Just $ lid lt) $ lchildren lt
    go (Array x) = mapM go' $ V.toList x
    go _ = Nothing
    go' (Object o) = do
        t <- Map.lookup "id" o
        t' <- go'' t
        c <- Map.lookup "children" o
        c' <- go c
        return $ LTree t' "ignored" c'
    go' _ = Nothing
    go'' (String t) = fromSinglePiece t
    go'' _ = Nothing

postNewLabelR :: Handler ()
postNewLabelR = do
    ((res, _), _) <- newLabelForm
    case res of
        FormSuccess name -> runDB (insert $ Label Nothing name) >> return ()
        _ -> return ()
    redirect RedirectTemporary LabelsR
