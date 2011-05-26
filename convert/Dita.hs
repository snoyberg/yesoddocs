{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Dita where

import Prelude hiding (FilePath)
import Data.Text (Text, pack, unpack)
import Data.XML.Types
import System.FilePath.CurrentOS
import System.Directory
import qualified Text.XML.Enumerator.Document as D
import Data.List (foldl1')

data DitaMap = DitaMap
    { mapTitle :: Text
    , mapTopics :: [Topic]
    }

data Topic = Topic
    { topicTitle :: Text
    , topicId :: Text
    , topicPath :: [Text]
    , topicContent :: [Node]
    , topicChildren :: [Topic]
    }

writeDitaMap :: FilePath -> DitaMap -> IO ()
writeDitaMap mapFile DitaMap {..} = do
    createDirectoryIfMissing True $ toString $ directory mapFile
    D.writePrettyFile (toString mapFile) $ Document
        (Prologue
            []
            (Just $ Doctype "map" $ Just $ PublicID "-//OASIS//DTD DITA Map//EN" "http://docs.oasis-open.org/dita/v1.1/OS/dtd/map.dtd")
            [])
        (Element "map" []
            $ NodeElement (Element "title" [] [NodeContent $ ContentText mapTitle])
            : map topicToRef mapTopics)
        []
    mapM_ (writeTopic $ directory mapFile) $ concatMap allTopics mapTopics

writeTopic :: FilePath -> Topic -> IO ()
writeTopic top t = do
    let fp = top </> topicRelPath t
    createDirectoryIfMissing True $ toString $ directory fp
    D.writeFile (toString fp) $ Document
        (Prologue
            []
            (Just $ Doctype "concept" $ Just $ PublicID "-//OASIS//DTD DITA Concept//EN" "http://docs.oasis-open.org/dita/v1.1/OS/dtd/concept.dtd")
            [])
        (Element "concept" [("id", [ContentText $ topicId t])]
            [ NodeElement $ Element "title" [] [NodeContent $ ContentText $ topicTitle t]
            , NodeElement $ Element "conbody" [] $ topicContent t
            ])
        []

topicToRef :: Topic -> Node
topicToRef t = NodeElement $ Element
    "topicref"
    [("href", [ContentText $ pack $ map fixSlash $ toString $ topicRelPath t])]
    $ map topicToRef $ topicChildren t
  where
    fixSlash '\\' = '/'
    fixSlash c = c

topicRelPath :: Topic -> FilePath
topicRelPath Topic { topicPath = [] } = error "Topic must have a path"
topicRelPath Topic { topicPath = x } = foldl1' append (map (fromString . unpack) x) `addExtension` "dita"

allTopics :: Topic -> [Topic]
allTopics t = t : concatMap allTopics (topicChildren t)
