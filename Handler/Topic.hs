{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Topic
    ( getTopicR
    , getTopicR'
    , postTopicR
    , postTopicLabelsR
    , showLTree
    , comments
    , getCommentCountR
    , getCommentsR
    , postCommentsR
    , postTopicWorldWriteableR
    , postTopicNotWorldWriteableR
    , getBestTopicR
    ) where

import Wiki
import Util (renderContent, validateContent, prettyDate, formatDateTime, prettyDateTime)
import Data.Text (pack)
import Control.Monad (unless, when)
import Text.Hamlet (shamlet)
import Handler.Labels (LTree (..), getLTree)
import Data.Maybe (mapMaybe, fromJust, catMaybes, isJust)
import Handler.CreateTopic (richEdit)
import Yesod.Json
import qualified Data.Text as T
import qualified Text.Blaze.Renderer.String as S
import Text.Blaze (toHtml)
import Handler.Search (updateTerms)

topicForm :: (Text, TopicFormat, Textarea, Maybe Text, Bool)
          -> Handler ((FormResult (Text, TopicFormat, Textarea, Maybe Text, Bool), Widget), Enctype)
topicForm (a, b, c, d, e) = runFormPost $ renderTable $ (,,,,)
    <$> areq textField (fromLabel MsgTitle) (Just a) -- TRANS
    <*> areq (selectField formats) (FieldSettings MsgFormat Nothing (Just "format") Nothing) (Just b)
    <*> areq textareaField (FieldSettings MsgContent Nothing (Just "contentarea") Nothing) (Just c)
    <*> aopt textField (fromLabel MsgSummary) (Just d)
    <*> areq boolField (FieldSettings MsgMinorUpdate (Just MsgMinorUpdateTooltip) Nothing Nothing) (Just e)

getTopicR :: TopicId -> Handler RepHtml
getTopicR = getTopicR' True

getTopicR' :: Bool -> TopicId -> Handler RepHtml
getTopicR' showAuthor tid = do
    Topic {..} <- runDB $ get404 tid
    TopicContent {..} <- runDB $ do
        x <- selectList [TopicContentTopic ==. tid] [Desc TopicContentChanged, LimitTo 1]
        case x of
            [y] -> return $ snd y
            [] -> lift notFound
            _ -> lift $ do
                $(logError) "Should only have returned 0 or 1 results"
                notFound
    owner <- runDB $ get404 topicOwner
    mauthor <- if topicOwner == topicContentAuthor then return Nothing else fmap Just $ runDB $ get404 topicContentAuthor
    ma <- maybeAuth
    let hasFullControl =
            case ma of
                Nothing -> False
                Just (uid, u) -> userAdmin u || uid == topicOwner
    let maid = fmap fst ma
        muser = fmap snd ma
    $(logDebug) $ pack $ concat ["maid: ", show maid, ", topicOwner", show topicOwner]
    mform <-
        if maid == Just topicOwner || fmap userAdmin muser == Just True || (topicAllWrite && isJust ma)
            then Just `fmap` (do
                ((_, w), e) <- topicForm (topicTitle, topicContentFormat, (Textarea $ topicContentContent), Nothing, False)
                return (w, e)
                )
            else return Nothing
    ltree <- getLTree
    slabels <- runDB $ fmap (map $ topicLabelLabel . snd) $ selectList [TopicLabelTopic ==. tid] []
    let activeLabel = flip elem slabels
    defaultLayout $ do
        comments
        richEdit >> $(widgetFile "topic")

comments :: Widget
comments = do
    addScript $ StaticR jquery_js
    addJulius $(juliusFile "comments")
    addLucius $(luciusFile "comments")

showLTree :: (LabelId -> Bool) -> [LTree] -> Widget
showLTree al lt = [whamlet|
$if not $ null lt
    <ul>
        $forall l <- lt
            <li>
                <input #label#{toSinglePiece $ lid l} type=checkbox name=#{toSinglePiece $ lid l} :al $ lid l:checked>
                <label for=label#{toSinglePiece $ lid l}>#{lname l}
                ^{showLTree al $ lchildren l}
|]

postTopicR :: TopicId -> Handler RepHtml
postTopicR tid = do
    Topic {..} <- runDB $ get404 tid
    TopicContent {..} <- runDB $ do
        x <- selectList [TopicContentTopic ==. tid] [Desc TopicContentChanged, LimitTo 1]
        case x of
            [y] -> return $ snd y
            [] -> lift notFound
            _ -> lift $ do
                $(logError) "Should only have returned 0 or 1 results"
                notFound
    (aid, user) <- requireAuth
    unless (aid == topicOwner || userAdmin user || topicAllWrite) $ permissionDenied ""
    ((res, wform), enctype) <- topicForm (topicTitle, topicContentFormat, (Textarea $ topicContentContent), Nothing, False)
    case res of
        FormSuccess (title, format, (Textarea content), msummary, isMinor) -> do
            now <- liftIO getCurrentTime
            _ <- runDB $ do
                update tid [TopicTitle =. title]
                let tc = TopicContent tid aid msummary now format $ validateContent format content
                _ <- insert tc
                updateTerms tc
                unless isMinor $ addNewsItem ("Topic updated: " `mappend` title) (TopicR tid) Nothing [shamlet|
<p>#{userName user} updated the topic: #{title}
$maybe summary <- msummary
    <p>Update summary: #{summary}
|]
            setMessageI MsgTopicUpdated
            redirect RedirectTemporary $ TopicR tid
        _ -> defaultLayout $(widgetFile "topic_post")

postTopicLabelsR :: TopicId -> Handler ()
postTopicLabelsR tid = do
    Topic {..} <- runDB $ get404 tid
    (aid, user) <- requireAuth
    unless (aid == topicOwner || userAdmin user) $ permissionDenied ""
    (pp, _) <- runRequestBody
    let sel = mapMaybe (fromSinglePiece . fst) pp
    runDB $ do
        deleteWhere [TopicLabelTopic ==. tid]
        labels <- selectList [] []
        flip mapM_ labels $ \(lid', _) -> do -- FIXME use a Set?
            when (lid' `elem` sel) $
                insert (TopicLabel tid lid') >> return ()
    redirect RedirectTemporary $ TopicR tid

getCommentCountR :: Handler RepJson
getCommentCountR = do
    topic' <- runInputGet $ ireq textField "topic"
    topic <- maybe notFound return $ fromSinglePiece topic' :: Handler TopicId -- FIXME this type signature should be unnecessary
    element <- runInputGet $ ireq textField "element"
    x <- runDB $ count [CommentTopic ==. topic, CommentElement ==. element]
    jsonToRepJson $ jsonMap [("count", jsonScalar $ show x)]

getCommentsR :: Handler RepJson
getCommentsR = do
    muid <- maybeAuthId
    topic' <- runInputGet $ ireq textField "topic"
    let topic = fromJust $ fromSinglePiece topic'
    element <- runInputGet $ ireq textField "element"
    render <- getUrlRenderParams
    comments' <- runDB $ selectList [CommentTopic ==. topic, CommentElement ==. element] [Asc CommentTime] >>= (mapM $ \(_, c) -> do
        let tid = commentContent c
        tcs <- selectList [TopicContentTopic ==. tid] [Desc TopicContentChanged, LimitTo 1]
        case tcs of
            [] -> return Nothing
            (_, tc):_ -> do
                a <- get404 $ topicContentAuthor tc
                let ham = renderContent tid (topicContentFormat tc) (topicContentContent tc)
                let html' = ham render
                return $ Just $ jsonMap
                    [ ("name", jsonScalar $ T.unpack $ userName a)
                    , ("date", jsonScalar $ prettyDate $ topicContentChanged tc)
                    , ("content", jsonScalar $ S.renderHtml html')
                    ]
        )
    jsonToRepJson $ jsonMap
        [ ("comments", jsonList $ catMaybes comments')
        , ("loggedin", jsonScalar $ maybe "false" (const "true") muid)
        ]

postCommentsR :: Handler ()
postCommentsR = do
    (uid, u) <- requireAuth
    topic' <- runInputGet $ ireq textField "topic"
    let topic = fromJust $ fromSinglePiece topic'
    element <- runInputGet $ ireq textField "element"
    content <- runInputPost $ ireq textField "content"
    source <- runInputPost $ ireq textField "source"
    now <- liftIO getCurrentTime
    let hash = T.concat
            [ "comment-"
            , topic'
            , "-"
            , element
            ]
    runDB $ do
        src <- get404 topic
        fam <- insert $ TFamily now
        tid <- insert $ Topic uid ("Comment on " `T.append` topicTitle src) now fam False
        let tc = TopicContent tid uid Nothing now TFText content
        _ <- insert tc
        updateTerms tc
        _ <- insert $ Comment topic element tid now
        addNewsItem (T.concat
            [ userName u
            , " commented on "
            , topicTitle src
            ]) (TopicR topic) (Just hash) (toHtml content)
    redirectText RedirectTemporary $ T.concat
        [ T.takeWhile (/= '#') source
        , "#"
        , hash
        ]

postTopicWorldWriteableR :: TopicId -> Handler ()
postTopicWorldWriteableR = wwHelper True

postTopicNotWorldWriteableR :: TopicId -> Handler ()
postTopicNotWorldWriteableR = wwHelper False

wwHelper :: Bool -> TopicId -> Handler ()
wwHelper isW tid = do
    (uid, u) <- requireAuth
    t <- runDB $ get404 tid
    unless (topicAllWrite t || topicOwner t == uid || userAdmin u) $ permissionDenied ""
    runDB $ update tid [TopicAllWrite =. isW]
    redirect RedirectTemporary $ TopicR tid

-- | Redirects to the \"best\" version of the topic. For example, if the topic
-- appears in the book, it links there.
getBestTopicR :: TopicId -> Handler ()
getBestTopicR tid = do
    chains <- getChains tid
    mbook <- runDB $ selectFirst [] []
    case mbook of
        Just (_, book) ->
            case filter (\x -> chainMap x == bookMap book) chains of
                chain:_ -> do
                    let chap = atLeast (bookChunking book - 1) $ chainToChapters chain
                    render <- getUrlRender
                    redirectText RedirectTemporary $ render chap `T.append` "#topic-" `T.append` toSinglePiece tid
                [] -> return ()
        Nothing -> return ()
    redirect RedirectTemporary $ TopicR tid

atLeast :: Int -> [a] -> a
atLeast _ [] = error "atLeast on empty list"
atLeast _ [a] = a
atLeast 0 (a:_) = a
atLeast i (_:as) = atLeast (i - 1) as

data Chain = Chain
    { chainMap :: TMapId
    , chainRest :: [(MapNodeSlug, Either TopicId TMapId)]
    }
    deriving Show

chainToChapters :: Chain -> [WikiRoute]
chainToChapters =
    go id . chainRest
  where
    go _ [] = []
    go front ((mns, Left _):xs) = go' front mns : go front xs
    go front ((mns, Right _):xs) = go (front . (:) mns) xs
    go' front mns =
        case front [] of
            [] -> BookChapterR mns []
            x:xs -> BookChapterR x (xs ++ [mns])

getChains :: TopicId -> Handler [Chain]
getChains tid0 = runDB $ do
    fmap concat $ selectList [TMapNodeCtopic ==. Just tid0] [] >>= mapM (go [] . snd)
  where
    go rest mn = do
        let rest' =
                case tMapNodeCtopic mn of
                    Nothing -> rest -- FIXME
                    Just tid -> (tMapNodeSlug mn, Left tid) : rest
        go' rest' mn
    go' rest mn =
        case tMapNodeParent mn of
            Nothing -> do
                let tmid = tMapNodeMap mn
                chains <- selectList [TMapNodeCmap ==. Just tmid] [] >>= mapM (\(_, mn') -> do
                    let rest' = (tMapNodeSlug mn, Right tmid) : rest
                    go' rest' mn'
                    )
                return $ (Chain tmid rest) : concat chains
            Just mnid -> do
                mn' <- get404 mnid
                go rest mn'
