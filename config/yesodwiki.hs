{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
#if PRODUCTION
import Controller (withWiki)
import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs)
import Data.Text (pack)

main :: IO ()
main = do
    args <- getArgs
    let usage = "Usage: yesodwiki <port> <approot>"
    (port, approot) <-
        case args of
            [x, y] ->
                case reads x of
                    (i, _):_ -> return (i, y)
                    _ -> error usage
            _ -> error usage
    withWiki (pack approot) $ run port
#else
import Controller (withWiki)
import System.IO (hPutStrLn, stderr)
--import Network.Wai.Middleware.Debug (debug)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
    let port = 3000
    hPutStrLn stderr $ "Application launched, listening on port " ++ show port
    withWiki "http://10.0.0.3:3000" $ run port -- . debug
#endif
