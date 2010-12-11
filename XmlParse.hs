{-# LANGUAGE DeriveDataTypeable #-}
module XmlParse
    ( tag
    , tag'
    , tag''
    , force
    , content
    , content'
    , XmlException (..)
    , requireAttr
    , optionalAttr
    , requireAttrRaw
    , optionalAttrRaw
    , simplify
    , parseFile
    , parseFile_
    , choose
    , many
    , SEvent (..)
    , ignoreAttrs
    ) where

import Data.XML.Types
    ( Event (..), Content (..), Attribute (..), Name (..)
    )
import qualified Data.Enumerator as E
import Text.XML.Enumerator.Parse (detectUtf, parseBytes)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Exception (SomeException, Exception, throwIO)
import Data.Enumerator
    ( Iteratee, run, joinI, ($$), throwError
    , (>>==), Stream (..), Enumeratee
    )
import Data.Enumerator.IO (enumFile)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy (Text)
import Data.Char (isSpace)
import Data.Typeable (Typeable)
import Control.Monad (ap, liftM)
import Control.Applicative (Applicative (..))

type SAttr = (Name, Text)
data SEvent = SBeginElement Name [SAttr]
            | SEndElement
            | SContent Text
    deriving (Show, Eq)

content :: Monad m => Iteratee SEvent m (Maybe Text)
content = do
    x <- E.peek
    case x of
        Just (SContent t) -> E.drop 1 >> return (Just t)
        _ -> return Nothing

content' :: Monad m => Iteratee SEvent m Text
content' = do
    x <- content
    case x of
        Nothing -> return T.empty
        Just y -> return y

tag :: Monad m
    => (Name -> Maybe a)
    -> (a -> AttrParser b)
    -> (b -> Iteratee SEvent m c)
    -> Iteratee SEvent m (Maybe c)
tag checkName attrParser f = do
    x <- dropWS
    case x of
        Just (SBeginElement name as) ->
            case checkName name of
                Just y ->
                    case runAttrParser' (attrParser y) as of
                        Left e -> throwError e
                        Right z -> do
                            E.drop 1
                            z' <- f z
                            a <- dropWS
                            case a of
                                Just SEndElement -> E.drop 1 >> return (Just z')
                                _ -> throwError $ SXmlException ("Expected end tag for: " ++ show name) a
                Nothing -> return Nothing
        _ -> return Nothing
  where
    dropWS = do
        x <- E.peek
        case x of
            Just (SContent t)
                | T.all isSpace t -> E.drop 1 >> E.peek
            _ -> return x
    runAttrParser' p as =
        case runAttrParser p as of
            Left e -> Left e
            Right ([], x) -> Right x
            Right (attr, _) -> Left $ UnparsedAttributes attr

tag' :: Monad m
     => Name
     -> AttrParser a
     -> (a -> Iteratee SEvent m b)
     -> Iteratee SEvent m (Maybe b)
tag' name attrParser = tag
    (\x -> if x == name then Just () else Nothing)
    (const attrParser)

tag'' :: Monad m => Name -> Iteratee SEvent m a -> Iteratee SEvent m (Maybe a)
tag'' name f = tag' name (return ()) $ const f

choose :: Monad m
       => [Iteratee SEvent m (Maybe a)]
       -> Iteratee SEvent m (Maybe a)
choose [] = return Nothing
choose (i:is) = do
    x <- i
    case x of
        Nothing -> choose is
        Just a -> return $ Just a

force :: Monad m
      => String -- ^ Error message
      -> Iteratee SEvent m (Maybe a)
      -> Iteratee SEvent m a
force msg i = do
    x <- i
    case x of
        Nothing -> throwError $ XmlException msg Nothing
        Just a -> return a

-- FIXME toSEvent :: Monad m => (Text -> Maybe Text) -> Enumeratee Event ([Name], SEvent) m b
simplify :: Monad m => (Text -> Maybe Text) -> Enumeratee Event SEvent m b
simplify renderEntity =
    loop []
  where
    loop stack = E.checkDone $ go stack
    sattr (Attribute x y) = do
        y' <- flip mapM y $ \z ->
            case z of
                ContentText t -> return t
                ContentEntity t ->
                    case renderEntity t of
                        Just t' -> return t'
                        Nothing -> throwError $ InvalidEntity t
        return (x, T.concat y')
    go stack k = do
        x <- E.head
        case x of
            Nothing -> k EOF >>== return
            Just EventBeginDocument -> go stack k
            Just EventEndDocument ->
                k EOF >>== return
            Just EventInstruction{} -> go stack k
            Just EventDoctype{} -> go stack k
            Just (EventBeginElement n as) -> do
                as' <- mapM sattr as
                k (Chunks [SBeginElement n as']) >>== loop (n : stack)
            Just (EventEndElement n) ->
                case stack of
                    [] -> throwError $ InvalidEndElement n
                    n':rest
                        | n == n' -> k (Chunks [SEndElement]) >>== loop rest
                        | otherwise -> throwError $ InvalidEndElement n
            Just (EventContent c) -> do
                t <- contentToText c
                ts <- takeContents $ (:) t
                k (Chunks [SContent $ T.concat $ ts []]) >>== loop stack
            Just EventComment{} -> go stack k
      where
        contentToText (ContentEntity e) =
            case renderEntity e of
                Nothing -> throwError $ InvalidEntity e
                Just t -> return t
        contentToText (ContentText t) = return t
        takeContents front = do
            x <- E.peek
            case x of
                Nothing -> return front
                Just EventBeginElement{} -> return front
                Just EventEndElement{} -> return front
                Just (EventContent c) -> do
                    E.drop 1
                    t <- contentToText c
                    takeContents $ front . (:) t
                Just EventBeginDocument -> takeContents front
                Just EventEndDocument -> takeContents front
                Just EventInstruction{} -> takeContents front
                Just EventDoctype{} -> takeContents front
                Just EventComment{} -> takeContents front

parseFile_ :: String -> (Text -> Maybe Text) -> Iteratee SEvent IO a -> IO a
parseFile_ fn re p =
    parseFile fn re p >>= go
  where
    go (Left e) = liftIO $ throwIO e
    go (Right a) = return a

parseFile :: String -> (Text -> Maybe Text) -> Iteratee SEvent IO a -> IO (Either SomeException a)
parseFile fn re p =
    run $ enumFile fn     $$ joinI
        $ detectUtf       $$ joinI
        $ parseBytes      $$ joinI
        $ simplify re     $$ p

data XmlException = XmlException
    { xmlErrorMessage :: String
    , xmlBadInput :: Maybe Event
    }
                  | InvalidEndElement Name
                  | InvalidEntity Text
                  | SXmlException
    { xmlErrorMessage :: String
    , sxmlBadInput :: Maybe SEvent
    }
                  | UnparsedAttributes [SAttr]
    deriving (Show, Typeable)
instance Exception XmlException

newtype AttrParser a = AttrParser { runAttrParser :: [SAttr] -> Either XmlException ([SAttr], a) }

instance Monad AttrParser where
    return a = AttrParser $ \as -> Right (as, a)
    (AttrParser f) >>= g = AttrParser $ \as ->
        case f as of
            Left e -> Left e
            Right (as', f') -> runAttrParser (g f') as'
instance Functor AttrParser where
    fmap = liftM
instance Applicative AttrParser where
    pure = return
    (<*>) = ap

optionalAttrRaw :: (SAttr -> Maybe b) -> AttrParser (Maybe b)
optionalAttrRaw f =
    AttrParser $ go id
  where
    go front [] = Right (front [], Nothing)
    go front (a:as) =
        case f a of
            Nothing -> go (front . (:) a) as
            Just b -> Right (front as, Just b)

requireAttrRaw :: String -> (SAttr -> Maybe b) -> AttrParser b
requireAttrRaw msg f = do
    x <- optionalAttrRaw f
    case x of
        Just b -> return b
        Nothing -> AttrParser $ const $ Left $ XmlException msg Nothing

requireAttr :: Name -> AttrParser Text
requireAttr n = requireAttrRaw
    ("Missing attribute: " ++ show n)
    (\(x, y) -> if x == n then Just y else Nothing)

optionalAttr :: Name -> AttrParser (Maybe Text)
optionalAttr n = optionalAttrRaw
    (\(x, y) -> if x == n then Just y else Nothing)

ignoreAttrs :: AttrParser ()
ignoreAttrs = AttrParser $ \_ -> Right ([], ())

many :: Monad m => Iteratee SEvent m (Maybe a) -> Iteratee SEvent m [a]
many i =
    go id
  where
    go front = do
        x <- i
        case x of
            Nothing -> return $ front []
            Just y -> go $ front . (:) y
