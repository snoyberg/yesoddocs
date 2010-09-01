import YesodDocs
import Network.Wai.Handler.SimpleServer (run)

main = putStrLn "Running..." >> withYesodDocs (run 3000)
