---
title: Blog, part 2 -- Screencasts -- Yesod
---
<object width="600" height="400"><param name="allowfullscreen" value="true" /><param name="allowscriptaccess" value="always" /><param name="movie" value="http://vimeo.com/moogaloop.swf?clip_id=13353654&amp;server=vimeo.com&amp;show_title=1&amp;show_byline=1&amp;show_portrait=0&amp;color=00ADEF&amp;fullscreen=1" /><embed src="http://vimeo.com/moogaloop.swf?clip_id=13353654&amp;server=vimeo.com&amp;show_title=1&amp;show_byline=1&amp;show_portrait=0&amp;color=00ADEF&amp;fullscreen=1" type="application/x-shockwave-flash" allowfullscreen="true" allowscriptaccess="always" width="600" height="400"></embed></object><p><a href="http://vimeo.com/13353654">Blog Tutorial - Part 2 - Yesod Web Framework 0.4.0</a> from <a href="http://vimeo.com/user1975429">Michael Snoyman</a> on <a href="http://vimeo.com">Vimeo</a>.</p>

Here's the source code:

    {-# LANGUAGE TypeFamilies, QuasiQuotes, GeneralizedNewtypeDeriving #-}
    import Yesod
    import Yesod.Helpers.Crud
    import Database.Persist.Sqlite
    import Data.Time (Day)
    
    share2 mkToForm mkPersist [$persist|
    Entry
        title String
        day JqueryDay Desc
        content NicHtml
        deriving
    |]
    
    instance Item Entry where
        itemTitle = entryTitle
    
    data Blog = Blog { pool :: Pool Connection }
    
    type EntryCrud = Crud Blog Entry
    
    mkYesod "Blog" [$parseRoutes|
    / RootR GET
    /entry/#EntryId EntryR GET
    /admin AdminR EntryCrud defaultCrud
    |]
    
    instance Yesod Blog where
        approot _ = "http://localhost:3000"
    
    instance YesodPersist Blog where
        type YesodDB Blog = SqliteReader
        runDB db = fmap pool getYesod>>= runSqlite db
    
    getRootR = do
        entries <- runDB $ select [] [EntryDayDesc]
        applyLayoutW $ do
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
        applyLayoutW $ do
            setTitle $ string $ entryTitle entry
            addBody [$hamlet|
    %h1 $entryTitle.entry$
    %h2 $show.unJqueryDay.entryDay.entry$
    $unNicHtml.entryContent.entry$
    |]
    
    withBlog f = withSqlite "blog.db3" 8 $ \pool -> do
        flip runSqlite pool $ do
            initialize (undefined :: Entry)
        f $ Blog pool
    
    main = withBlog $ basicHandler 3000
