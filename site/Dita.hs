{-# LANGUAGE OverloadedStrings #-}
module Dita (renderDita) where

import Data.XML.Types
import Text.Blaze
import Data.Text (unpack)
import Data.Monoid (mconcat, mempty)
import Text.Blaze.Html5 hiding (map, title, body)

renderDita :: Document -> (Html, String)
renderDita (Document _ (Element _ _ subs) _) =
    (gos body, title)
  where
    title' = subs >>= isElement >>= isNamed "title" >>= elementText
    title = unpack $ mconcat title'
    body = subs >>= isElement >>= isNamed "conbody" >>= nodeChildren . NodeElement

gos :: [Node] -> Html
gos = mconcat . map go

go :: Node -> Html
go (NodeContent (ContentText t)) = toHtml t
go (NodeElement (Element e _ children)) = getWrapper e $ gos children
go _ = mempty

getWrapper :: Name -> Html -> Html
getWrapper "p" = p
getWrapper x = error $ "Unknown DITA element: " ++ show x
