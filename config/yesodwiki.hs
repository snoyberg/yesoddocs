{-# LANGUAGE CPP #-}
#if PRODUCTION
import Controller (withWiki)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = withWiki $ run 3000
#else
import Controller (withWiki)
import System.IO (hPutStrLn, stderr)
import Network.Wai.Middleware.Debug (debug)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
    let port = 3000
    hPutStrLn stderr $ "Application launched, listening on port " ++ show port
    withWiki $ run port . debug
#endif
