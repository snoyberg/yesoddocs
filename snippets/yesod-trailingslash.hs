-- START
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell #-}
import Yesod
import Web.Routes (encodePathInfo)

data Slash = Slash

mkYesod "Slash" [$parseRoutes|
/ RootR GET
/foo FooR GET
|]

instance Yesod Slash where
    approot _ = ""

    joinPath _ ar pieces' qs =
        ar ++ '/' : encodePathInfo pieces qs
      where
        pieces = if null pieces' then [] else pieces' ++ [""]

    -- We want to keep canonical URLs. Therefore, if the URL is missing a
    -- trailing slash, redirect. But the empty set of pieces always stays the
    -- same.
    cleanPath _ [] = Right []
    cleanPath _ s
        | dropWhile (not . null) s == [""] = -- the only empty string is the last one
            Right $ init s
        -- Since joinPath will append the missing trailing slash, we simply
        -- remove empty pieces.
        | otherwise = Left $ filter (not . null) s

getRootR = defaultLayout [$hamlet|
<p
    <a href=@{RootR}>RootR
<p
    <a href=@{FooR}>FooR
|]

getFooR = getRootR

main = warpDebug 3000 Slash
