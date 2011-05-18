{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Util
    ( renderContent
    , validateContent
    , userGravatar
    , prettyDate
    ) where

import Model (TopicFormat (..), User (userEmail))
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Text.Hamlet (Html, preEscapedText, toHtml)
import Text.HTML.SanitizeXSS (sanitizeBalance)
import Text.Hamlet.NonPoly (html)
import Data.Digest.Pure.MD5 (md5)
import Data.Char (isSpace, toLower)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as L

renderContent :: TopicFormat -> Text -> Html
renderContent TFHtml t = preEscapedText t
renderContent TFText t = toHtml t
renderContent tf _ = toHtml $ "renderContent: not yet written for " ++ show tf

validateContent :: TopicFormat -> Text -> Text
validateContent TFHtml t = pack $ sanitizeBalance $ unpack t
validateContent TFText t = t
validateContent tf _ = pack $ "validateContent: not yet written for " ++ show tf

userGravatar :: User -> Html
userGravatar u =
    [html|<img src="http://www.gravatar.com/avatar/#{hash}?d=identicon&s=100">|]
  where
    email = fromMaybe "" $ userEmail u
    hash = pack $ show $ md5 $ L.fromChunks $ return $ encodeUtf8 $ pack $ map toLower $ trim $ unpack email
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

prettyDate :: Show a => a -> String -- FIXME
prettyDate = show