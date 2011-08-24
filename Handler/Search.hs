{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Search
    ( postRebuildSearchR
    , updateTerms
    , getSearchR
    ) where

import Wiki
import Util
import Control.Monad (unless)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.HTML.TagSoup
import Data.Enumerator (($$), run_)
import qualified Data.Enumerator.List as EL
import Data.Maybe (mapMaybe)
import Text.Blaze.Renderer.Text (renderHtml)
import Database.Persist.Base (PersistValue (PersistText, PersistInt64, PersistDouble))
import Database.Persist.GenericSql.Raw (withStmt)

postRebuildSearchR :: Handler ()
postRebuildSearchR = do
    (_uid, u) <- requireAuth
    unless (userAdmin u) $ permissionDenied ""
    runDB $ run_ $ selectKeys [] $$ flip EL.foldM () $ \() tid ->
        selectList [TopicContentTopic ==. tid] [Desc TopicContentChanged, LimitTo 1]
            >>= mapM_ (updateTerms . snd)
    setMessageI MsgSearchIndexUpdated
    redirect RedirectTemporary SettingsR

updateTerms :: TopicContent -> YesodDB sub Wiki ()
updateTerms tc = do
    let tid = topicContentTopic tc
    let ham = renderContent tid (topicContentFormat tc) (topicContentContent tc)
    render <- lift getUrlRenderParams
    let ht = renderHtml $ ham render
    let go (TagText t) = Just t
        go _ = Nothing
    let content = T.concat $ concatMap TL.toChunks $ mapMaybe go $ parseTags ht
    deleteBy $ UniqueSearchTerm tid
    _ <- insert $ SearchTerm tid content
    return ()

getSearchR :: Handler RepHtml
getSearchR = do
    query <- runInputGet $ ireq textField "query"
    results <- runDB $ withStmt sql [PersistText query] $ go id
    defaultLayout $(widgetFile "search")
  where
    sql = T.unlines
        [ "SELECT topic, ts_rank_cd(to_tsvector(\"content\"), query, 32) as rank"
        , "FROM \"SearchTerm\", plainto_tsquery($1) query"
        , "WHERE to_tsvector(\"content\") @@ query"
        , "ORDER BY rank DESC"
        ]
    go front pop = do
        x <- pop
        case x of
            Just [PersistInt64 i, PersistDouble rank] -> do
                let tid = Key $ PersistInt64 i
                t <- get404 tid
                go (front . (:) ((tid, t), prettyRank rank)) pop
            Nothing -> return $ front []
            _ -> error $ "Invalid result in getSearchR: " ++ show x
    prettyRank x = show (round (x * 100) :: Int) ++ "%"
