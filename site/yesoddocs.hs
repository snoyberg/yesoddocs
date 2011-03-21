import Controller
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = putStrLn "Running..." >> withYesodDocs (run 9000)
