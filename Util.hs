module Util
    ( renderContent
    , validateContent
    ) where

import Model (TopicFormat (..))
import Data.Text (Text, pack, unpack)
import Text.Hamlet (Html, preEscapedText, toHtml)
import Text.HTML.SanitizeXSS (sanitizeBalance)

renderContent :: TopicFormat -> Text -> Html
renderContent TFHtml t = preEscapedText t
renderContent TFText t = toHtml t
renderContent tf _ = toHtml $ "renderContent: not yet written for " ++ show tf

validateContent :: TopicFormat -> Text -> Text
validateContent TFHtml t = pack $ sanitizeBalance $ unpack t
validateContent TFText t = t
validateContent tf _ = pack $ "validateContent: not yet written for " ++ show tf
