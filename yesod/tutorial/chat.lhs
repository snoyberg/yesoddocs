---
title: Chat -- Tutorials -- Yesod
---
OK, maybe chat is a little over-reaching for this tutorial... but I like typing less, and chat is shorter than message board ;).

In this tutorial, we'll create an app that let's you log in and add messages. The server will display all messages to logged-in users.

> {-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell #-}
> import Yesod
> import Yesod.Helpers.Auth
> import Control.Concurrent.MVar
> import Data.Monoid (mempty)

> data Message = Message
>   { messageAuthor :: String
>   , messageContent :: String
>   }

Since we want to alter the list of messages, we're going to need a mutable variable. Note that this application- for simplicity- does not store any information on disk, so you'll lose your history on a server restart.

> data Chat = Chat
>   { chatMsgs :: MVar [Message]
>   , chatAuth :: Auth
>   }

Yesod comes baked in with three different authentication methods: OpenId, Rpxnow and e-mail based. For those not familiar with Rpxnow, it's a service that makes it easy to log in through multiple backends, such as Google, Twitter, Yahoo, etc.

The e-mail method allows users to register an e-mail address, get a verification key by e-mail, set password and log in. As you might imagine, this requires some setup in general: usually, you'll have to add some database tables and configure e-mail sending. For testing, Yesod includes inMemoryEmailSettings, which uses an in-memory database and simply outputs verification information to standard error.

> loadChat :: IO Chat
> loadChat = do
>   msgs <- newMVar []
>   aes <- inMemoryEmailSettings
>   return $ Chat msgs Auth
>       { authIsOpenIdEnabled = True
>       , authRpxnowApiKey = Just "c8043882f14387d7ad8dfc99a1a8dab2e028f690"
>       , authEmailSettings = Just aes
>       , authFacebook = Just ("134280699924829", "a7685e10c8977f5435e599aaf1d232eb")
>       , authFacebookPerms = ["email"]
>       }

There are three resource patterns: the homepage, the auth subsite, and the messages page. When you GET the messages page, it will give you the history. POSTing will allow you to add a message. The homepage will have login information.

> mkYesod "Chat" [$parseRoutes|
> /                  HomeR      GET
> /auth              AuthR      Auth chatAuth
> /messages          MessagesR  GET POST
> |]

And now let's hit the typeclasses; as usual, we need the Yesod typeclass. Now, we'll also add the YesodAuth typeclass. Like Yesod, it provides default values when possible. Since we need to provide a return URL for OpenID, we now need a valid value for approot instead of just an empty string.

> instance Yesod Chat where
>   approot _ = "http://localhost:3000"

> instance YesodAuth Chat where
>   defaultDest _ = MessagesR
>   defaultLoginRoute _ = HomeR

This basically says "send users to MessagesR on login, and to HomeR when they *need* to login."

Now we'll write the homepage; that funny iframe bit at the bottom comes straight from RPXnow.

> getHomeR :: Handler Chat RepHtml
> getHomeR = applyLayout "Chat Home" mempty [$hamlet|
> %h1 OpenID
> %form!action=@AuthR.OpenIdForward@
>   %input!type=text!name=openid
>   %input!type=submit!value=Login
> %h1 Email (well, sort of)
> %form!method=post!action=@AuthR.EmailRegisterR@
>   %input!type=email!name=email
>   %input!type=submit!value=Register
> %h1 Facebook
> %a!href=@AuthR.StartFacebookR@ Facebook Connect
> %h1 Rpxnow
> <iframe src="http://yesod-test.rpxnow.com/openid/embed?token_url=@AuthR.RpxnowR@" scrolling="no" frameBorder="no" allowtransparency="true" style="width:400px;height:240px"></iframe>
> |]

Next, we'll write the GET handler for messages. We use the "requireCreds" to get the user's credentials. If the user it not logged in, they are redirected to the homepage.

> getMessagesR :: Handler Chat RepHtml
> getMessagesR = do
>   creds <- requireCreds -- now we know we're logged in
>   msgs' <- chatMsgs `fmap` getYesod
>   msgs <- liftIO $ readMVar msgs'
>   hamletToRepHtml $ template msgs creds
>  where
>   template msgs creds = [$hamlet|
>       !!!
>       %html
>           %head
>               %title Silly Chat Server
>           %body
>               %p Logged in as $string.credsIdent.creds$
>               %p Your full creds: $string.show.creds$
>               %form!method=post!action=@MessagesR@
>                   Enter your message: 
>                   %input!type=text!name=message!width=400
>                   %input!type=submit
>               %h1 Messages
>               %dl
>                   $forall msgs msg
>                       %dt $string.messageAuthor.msg$
>                       %dd $string.messageContent.msg$
>   |]

Pretty straight-forward. Now we'll add the post handler.

> postMessagesR :: Handler Chat ()
> postMessagesR = do
>   creds <- requireCreds -- now we know we're logged in
>   message <- runFormPost $ notEmpty $ required $ input "message"
>   msgs <- chatMsgs `fmap` getYesod
>   let msg = Message (credsIdent creds) message
>   liftIO $ modifyMVar_ msgs $ return . (:) msg
>   redirect RedirectTemporary MessagesR

This includes a minor introduction to the Yesod.Form module. The second line in the do block essentially says to get the "message" POST parameter; there must be precisely one parameter and cannot be empty (ie, ""). The modifyMVar_ line simply tacks the new message onto the message MVar, and then we redirect to view the messages.

Finally, we'll do our standard main function.

> main :: IO ()
> main = do
>   chat <- loadChat
>   app <- toWaiApp chat
>   basicHandler 3000 app
