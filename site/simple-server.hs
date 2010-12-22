import YesodDocs
import Network.Wai.Handler.SimpleServer (run)

main :: IO ()
main = putStrLn "Running..." >> withYesodDocs (run 3000)
