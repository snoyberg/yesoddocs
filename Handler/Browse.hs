{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Browse
    ( getBrowseR
    ) where

import Wiki
import Handler.Labels (getLTree)
import Handler.Topic (showLTree)
import Data.Maybe (mapMaybe, catMaybes)
import qualified Data.Set as Set
import Data.List (sortBy)
import Data.Ord (comparing)

data Entry = Entry
    { etitle :: Text
    , eauthor :: User
    , eurl :: WikiRoute
    }

getBrowseR :: Handler RepHtml
getBrowseR = do
    ltree <- getLTree
    sel <- mapMaybe (fromSinglePiece . fst) . reqGetParams <$> getRequest
    let active = flip elem sel
    entriesT <- catMaybes <$> (runDB $ selectList [] [] 0 0 >>= mapM (\(tid, t) -> do
        lids <- (map $ topicLabelLabel . snd) <$> selectList [TopicLabelTopicEq tid] [] 0 0
        if applyFilter sel lids
            then do
                u <- get404 $ topicOwner t
                return $ Just Entry
                    { etitle = topicTitle t
                    , eauthor = u
                    , eurl = TopicR tid
                    }
            else return Nothing))
    entriesM <- catMaybes <$> (runDB $ selectList [] [] 0 0 >>= mapM (\(mid, m) -> do
        lids <- (map $ mapLabelLabel . snd) <$> selectList [MapLabelMapEq mid] [] 0 0
        if applyFilter sel lids
            then do
                u <- get404 $ tMapOwner m
                return $ Just Entry
                    { etitle = tMapTitle m
                    , eauthor = u
                    , eurl = ShowMapR mid
                    }
            else return Nothing))
    let entries = sortBy (comparing etitle) $ entriesT ++ entriesM
    defaultLayout $ do
        addScript $ StaticR jquery_js
        $(widgetFile "browse")

applyFilter :: [LabelId] -- ^ selected
            -> [LabelId] -- ^ this topic
            -> Bool
applyFilter sel top = Set.fromList sel `Set.isSubsetOf` Set.fromList top
