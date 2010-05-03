---
title: Chat -- Tutorials -- Yesod
---
OK, maybe chat is a little over-reaching for this tutorial... but I like typing less, and chat is shorter than message board ;).

In this tutorial, we'll create an app that let's you log in via OpenID and add messages. The server will display all messages to logged-in users.

> {-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell #-}
> import Yesod
> import Yesod.Helpers.Auth
> import Control.Concurrent.MVar

> data Message = Message
>   { messageAuthor :: String
>   , messageContent :: String
>   }

Since we want to alter the list of messages, we're going to need a mutable variable. Note that this application- for simplicity- does not store any information on disk, so you'll lose your history on a server restart.

> data Chat = Chat
>   { chatMsgs :: MVar [Message]
>   , chatAuth :: Auth
>   }

> loadChat :: IO Chat
> loadChat = do
>   msgs <- newMVar []
>   return $ Chat msgs Auth

That Auth constructor is supposed to be empty. The Auth subsite receives arguments through the YesodAuth typeclass, *not* through its argument datatype.

> mkYesod "Chat" [$parseRoutes|
> /                  HomeR      GET
> /auth              AuthR      Auth   siteAuth   chatAuth
> /messages          MessagesR  GET POST
> |]

And now let's hit the typeclasses; as usual, we need the Yesod typeclass. Now, we'll also add the YesodAuth typeclass. Like Yesod, it provides default values when possible. Since we need to provide a return URL for OpenID, we now need a valid value for approot instead of just an empty string.

> instance Yesod Chat where
>   approot _ = "http://localhost:3000"

> instance YesodAuth Chat where
>   defaultDest = const HomeR
>   liftAuthRoute = const AuthR

In a normal app, we'd probably put together a homepage that would explain our app. For now, we'll simply check if the user is logged in. If they are, we'll redirect to the messages page; otherwise, we'll redirect to the OpenId login page.

> getHomeR :: Handler Chat ()
> getHomeR = do
>   _ <- authIdentifier -- automatically redirects to login
>   redirect RedirectTemporary MessagesR

Next, we'll write the GET handler for messages.

> getMessagesR :: Handler Chat RepHtml
> getMessagesR = do
>   msgs' <- chatMsgs `fmap` getYesod
>   msgs <- liftIO $ readMVar msgs'
>   hamletToRepHtml $ template msgs
>  where
>   template = [$hamlet|
>       !!!
>       %html
>           %head
>               %title Silly Chat Server
>           %body
>               %form!method=post!action=@messagesR@
>                   Enter your message: 
>                   %input!type=text!name=message!width=400
>                   %input!type=submit
>               %h1 Messages
>               %dl
>                   $forall id msg
>                       %dt $msg.messageAuthor.cs$
>                       %dd $msg.messageContent.cs$
>   |]
>   messagesR _ = MessagesR

Pretty straight-forward. Now we'll add the post handler.

> postMessagesR :: Handler Chat ()
> postMessagesR = do
>   ident <- authIdentifier -- now we know we're logged in
>   message <- runFormPost $ notEmpty $ required $ input "message"
>   msgs <- chatMsgs `fmap` getYesod
>   let msg = Message ident message
>   liftIO $ modifyMVar_ msgs $ return . (:) msg
>   redirect RedirectTemporary MessagesR

This includes a minor introduction to the Yesod.Form module. The second line in the do block essentially says to get the "message" POST parameter; there must be precisely one parameter and cannot be empty (ie, ""). The modifyMVar_ line simply tacks the new message onto the message MVar, and then we redirect to view the messages.

Finally, we'll do our standard main function.

> main :: IO ()
> main = do
>   chat <- loadChat
>   app <- toWaiApp chat
>   basicHandler 3000 app
