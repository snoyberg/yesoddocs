{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Wiki
    ( Wiki (..)
    , WikiRoute (..)
    , resourcesWiki
    , Handler
    , Widget
    , maybeAuth
    , maybeAuthId
    , requireAuth
    , requireAuthId
    , module Yesod.Core
    , module Yesod.Form
    , module Yesod.Persist
    , module Settings
    , module Model
    , module StaticFiles
    , StaticRoute (..)
    , AuthRoute (..)
    , WikiMessage (..)
    , lift
    , liftIO
    , getCurrentTime
    , UTCTime
    , (<$>)
    , (<*>)
    , Text
    , addNewsItem
    , mappend
    , fromLabel
    , getBlogPost
    , getBook
    , getMapNode
    ) where

import Data.Time
import Yesod.Core hiding (YesodBreadcrumbs (..), breadcrumbs, setMessage)
import Yesod.Form hiding (Field, MsgDelete)
import Yesod.Persist
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.OpenId
import Yesod.AtomFeed
import qualified Settings
import System.Directory
import qualified Data.ByteString.Lazy as L
import Database.Persist.GenericSql
import Settings (hamletFile, cassiusFile, juliusFile, widgetFile, luciusFile)
import Model
import StaticFiles
import Control.Monad (unless)
import Text.Jasmine (minifym)
import qualified Data.Text as T
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Yesod.Message
import Data.Text (Text, pack)
import Control.Applicative ((<$>), (<*>))
import Data.Monoid (mappend)
import Text.Hamlet (Html, ihamletFile, IHamlet)
import qualified Yesod.Auth.OpenId as OpenId
import qualified Yesod.Auth.Message as Msg

mkMessage "Wiki" "messages" "en"

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data Wiki = Wiki
    { getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Settings.ConnectionPool -- ^ Database connection pool.
    , myApproot :: Text
    }

-- | A useful synonym; most of the handler functions in your application
-- will need to be of this type.
type Handler = GHandler Wiki Wiki

-- | A useful synonym; most of the widgets functions in your application
-- will need to be of this type.
type Widget = GWidget Wiki Wiki

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://docs.yesodweb.com/book/web-routes-quasi/
--
-- This function does three things:
--
-- * Creates the route datatype WikiRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route Wiki = WikiRoute
-- * Creates the value resourcesWiki which contains information on the
--   resources declared below. This is used in Controller.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- Wiki. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the WikiRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "Wiki" $(parseRoutesFile "config/routes")

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod Wiki where
    approot = myApproot

    defaultLayout widget = do
        mmsg <- getMessage
        (title, bcs) <- breadcrumbs
        muser <- fmap (fmap snd) maybeAuth
        pc <- widgetToPageContent $ do
            setTitleI title
            addLucius $(Settings.luciusFile "html5reset")
            widget
            atomLink FeedR "Site activity"
            atomLink BlogFeedR "Blog posts"
        tm <- getRouteToMaster
        mcurr <- getCurrentRoute
        let isHome = fmap tm mcurr == Just RootR
        ihamletToRepHtml ($(ihamletFile "hamlet/default-layout.hamlet") :: IHamlet WikiMessage WikiRoute)

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticroot setting in Settings.hs
    urlRenderOverride a (StaticR s) =
        Just $ uncurry (joinPath a $ Settings.staticroot $ myApproot a) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext' _ content = do
        let fn = base64md5 content ++ '.' : T.unpack ext'
        let content' =
                if ext' == "js"
                    then case minifym content of
                            Left _ -> content
                            Right y -> y
                    else content
        let statictmp = Settings.staticdir ++ "/tmp/"
        liftIO $ createDirectoryIfMissing True statictmp
        let fn' = statictmp ++ fn
        exists <- liftIO $ doesFileExist fn'
        unless exists $ liftIO $ L.writeFile fn' content'
        return $ Just $ Right (StaticR $ StaticRoute ["tmp", T.pack fn] [], [])

    clientSessionDuration _ = 60 * 24 * 7 * 2 -- 2 weeks

    maximumContentLength _ (Just UploadDitamapR) = 1000 * 1000 * 5
    maximumContentLength _ _ = 1000 * 1000 * 1

-- How to run database actions.
instance YesodPersist Wiki where
    type YesodDB Wiki = SqlPersist
    runDB db = liftIOHandler
             $ fmap connPool getYesod >>= Settings.runConnectionPool db

instance RenderMessage Wiki FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodAuth Wiki where
    type AuthId Wiki = UserId

    -- Where to send a user after successful login
    loginDest _ = SettingsR
    -- Where to send a user after logout
    logoutDest _ = RootR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (uid, _) -> return $ Just uid
            Nothing -> do
                handle <- getUniqueHandle (1 :: Int)
                fmap Just $ insert $ User (credsIdent creds) "Unnamed User" False handle Nothing Nothing Nothing
      where
        getUniqueHandle i = do
            let h = UserHandleT $ pack $ "anon" ++ show i
            x <- getBy $ UniqueHandle h
            case x of
                Nothing -> return h
                Just _ -> getUniqueHandle $ i + 1

    authPlugins = [authOpenId]

    loginHandler = do
        ident <- newIdent
        let name = "openid_identifier" :: Text
        defaultLayout $(widgetFile "login")

instance YesodBreadcrumbs Wiki where
    breadcrumb RootR = return (MsgHomepageTitle, Nothing)
    breadcrumb (PageR p) = do
        t <- runDB $ getBy404 (UniquePage p) >>= get404 . pageTopic . snd
        return (MsgTopicTitle $ topicTitle t, Just RootR)
    breadcrumb CreateTopicR = return (MsgCreateTopicTitle, Just SettingsR)
    breadcrumb (TopicR tid) = do
        t <- runDB $ get404 tid
        return (MsgTopicTitle $ topicTitle t, Just RootR)
    breadcrumb SettingsR = return (MsgSettingsTitle, Just RootR)
    breadcrumb CreateMapR = return (MsgCreateMapTitle, Just RootR)
    breadcrumb (EditMapR i) = do
        m <- runDB $ get404 i
        return (MsgEditMapTitle $ tMapTitle m, Just SettingsR)
    breadcrumb LabelsR = return (MsgLabelsTitle, Just SettingsR)
    breadcrumb BrowseR = return (MsgBrowseTitle, Just RootR)

    breadcrumb (ShowMapR tmid) = do
        tm <- runDB $ get404 tmid
        return (MsgShowMapTitle $ tMapTitle tm, Just RootR)
    breadcrumb (AuthR LoginR) = return (MsgLoginTitle, Just RootR)
    breadcrumb (BlogPostR year month slug) = do
        blog <- getBlogPost year month slug
        tm <- runDB $ get404 $ blogMap blog
        return (MsgBlogPostTitle $ tMapTitle tm, Just RootR)
    breadcrumb BookR = do
        book <- runDB getBook
        tm <- runDB $ get404 $ bookMap book
        return (MsgBookTitle $ tMapTitle tm, Just RootR)
    breadcrumb (BookChapterR mnslug mnslugs) = do
        (_, mn) <- runDB $ getMapNode mnslug mnslugs
        title <-
            case tMapNodeCtopic mn of
                Just tid -> runDB $ topicTitle <$> get404 tid
                Nothing -> return "" -- FIXME
        return (MsgBookChapterTitle title, Just BookR)
    breadcrumb SearchR = return (MsgSearchTitle, Just RootR)
    breadcrumb (WikiR ps) = do
        let title = if null ps then MsgWikiHomeTitle else MsgWikiTitle (last ps)
        let parent = if null ps then RootR else WikiR (init ps)
        return (title, Just parent)

    breadcrumb StaticR{} = return (MsgNotFound, Nothing)
    breadcrumb FaviconR{} = return (MsgNotFound, Nothing)
    breadcrumb RobotsR{} = return (MsgNotFound, Nothing)
    breadcrumb FeedR{} = return (MsgNotFound, Nothing)
    breadcrumb BlogFeedR{} = return (MsgNotFound, Nothing)
    breadcrumb FeedItemR{} = return (MsgNotFound, Nothing)
    breadcrumb EditPageR{} = return (MsgNotFound, Nothing)
    breadcrumb NewLabelR{} = return (MsgNotFound, Nothing)
    breadcrumb TopicLabelsR{} = return (MsgNotFound, Nothing)
    breadcrumb MapLabelsR{} = return (MsgNotFound, Nothing)
    breadcrumb AuthR{} = return (MsgNotFound, Nothing)
    breadcrumb AddBlogMapR{} = return (MsgNotFound, Nothing)
    breadcrumb SetBookR{} = return (MsgNotFound, Nothing)
    breadcrumb RebuildSearchR{} = return (MsgNotFound, Nothing)
    breadcrumb BlogR{} = return (MsgNotFound, Nothing)
    breadcrumb StaticContentR{} = return (MsgNotFound, Nothing)
    breadcrumb UploadDitamapR{} = return (MsgNotFound, Nothing)
    breadcrumb DownloadDitamapR{} = return (MsgNotFound, Nothing)
    breadcrumb UploadDitamapUrlR{} = return (MsgNotFound, Nothing)
    breadcrumb BlogPostNoDateR{} = return (MsgNotFound, Nothing)
    breadcrumb UploadBlogsR{} = return (MsgNotFound, Nothing)
    breadcrumb CommentCountR{} = return (MsgNotFound, Nothing)
    breadcrumb CommentsR{} = return (MsgNotFound, Nothing)
    breadcrumb TopicWorldWriteableR{} = return (MsgNotFound, Nothing)
    breadcrumb TopicNotWorldWriteableR{} = return (MsgNotFound, Nothing)
    breadcrumb DeleteTopicR{} = return (MsgNotFound, Nothing)
    breadcrumb DeleteMapR{} = return (MsgNotFound, Nothing)
    breadcrumb EditMapNameR{} = return (MsgNotFound, Nothing)

class YesodBreadcrumbs y where
    -- | Returns the title and the parent resource, if available. If you return
    -- a 'Nothing', then this is considered a top-level page.
    breadcrumb :: Route y -> GHandler sub y (WikiMessage, Maybe (Route y))

-- | Gets the title of the current page and the hierarchy of parent pages,
-- along with their respective titles.
breadcrumbs :: YesodBreadcrumbs y => GHandler sub y (WikiMessage, [(Route y, WikiMessage)])
breadcrumbs = do
    x' <- getCurrentRoute
    tm <- getRouteToMaster
    let x = fmap tm x'
    case x of
        Nothing -> return (MsgNotFound, [])
        Just y -> do
            (title, next) <- breadcrumb y
            z <- go [] next
            return (title, z)
  where
    go back Nothing = return back
    go back (Just this) = do
        (title, next) <- breadcrumb this
        go ((this, title) : back) next

addNewsItem :: Text -> WikiRoute -> Maybe Text -> Html -> SqlPersist (GGHandler s Wiki IO) ()
addNewsItem title url mhash content = do
    now <- liftIO getCurrentTime
    render <- lift getUrlRender
    let hash = maybe "" ("#" `T.append`) mhash
    _ <- insert $ NewsItem now title (render url `T.append` hash) content
    return ()

fromLabel :: WikiMessage -> FieldSettings WikiMessage
fromLabel x = FieldSettings x Nothing Nothing Nothing

getBlogPost :: Int -> Month -> BlogSlugT -> GHandler sub Wiki Blog
getBlogPost year month slug =
    runDB $ fmap snd $ getBy404 $ UniqueBlogSlug year month slug

getBook :: SqlPersist (GGHandler s Wiki IO) Book
getBook = do
    x <- selectList [] [LimitTo 1]
    case x of
        [] -> lift notFound
        (_, y):_ -> return y

getMapNode :: MapNodeSlug -> MapNodeSlugs -> SqlPersist (GGHandler s Wiki IO) (TMapNodeId, TMapNode)
getMapNode mnslug mnslugs = do
    book <- getBook
    (mnid, mn) <- go' (bookMap book) mnslug
    go mnid mn mnslugs
  where
    go' tmid slug = getBy404 $ UniqueMapNode tmid slug
    go mnid mn [] = return (mnid, mn)
    go _ mn (x:xs) =
        case tMapNodeCmap mn of
            Nothing -> lift notFound -- FIXME more debug info?
            Just tmid -> do
                (mnid', mn') <- go' tmid x
                go mnid' mn' xs
