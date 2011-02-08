import YesodDocs
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = putStrLn "Running..." >> withYesodDocs (run 3001)
