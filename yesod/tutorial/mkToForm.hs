{-# LANGUAGE TypeFamilies, QuasiQuotes, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Yesod
import Yesod.Helpers.Crud
import Yesod.Form.Jquery
import Yesod.Form.Nic
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Time (Day)

share2 mkToForm mkPersist [$persist|
Entry
    title String
    day Day Desc toFormField=YesodJquery.jqueryDayField
    content Html toFormField=YesodNic.nicHtmlField
    deriving
|]

instance Item Entry where
    itemTitle = entryTitle

data Blog = Blog { pool :: ConnectionPool }

type EntryCrud = Crud Blog Entry

mkYesod "Blog" [$parseRoutes|
/ RootR GET
/entry/#EntryId EntryR GET
/admin AdminR EntryCrud defaultCrud
|]

instance Yesod Blog where
    approot _ = "http://localhost:3000"

instance YesodPersist Blog where
    type YesodDB Blog = SqlPersist
    runDB db = fmap pool getYesod >>= runSqlPool db

instance YesodJquery Blog
instance YesodNic Blog

getRootR = do
    entries <- runDB $ selectList [] [EntryDayDesc] 0 0
    defaultLayout $ do
        setTitle $ string "Yesod Blog Tutorial Homepage"
        addBody [$hamlet|
%h1 Archive
%ul
    $forall entries entry
        %li
            %a!href=@EntryR.fst.entry@ $entryTitle.snd.entry$
%p
    %a!href=@AdminR.CrudListR@ Admin
|]

getEntryR entryid = do
    entry <- runDB $ get404 entryid
    defaultLayout $ do
        setTitle $ string $ entryTitle entry
        addBody [$hamlet|
%h1 $entryTitle.entry$
%h2 $show.entryDay.entry$
$entryContent.entry$
|]

withBlog f = withSqlitePool "blog.db3" 8 $ \pool -> do
    flip runSqlPool pool $ runMigration $ do
        migrate (undefined :: Entry)
    f $ Blog pool

main = withBlog $ basicHandler 3000
