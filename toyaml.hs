import Comments
import Data.Object.Yaml
import Data.Object
import Yesod
import qualified Data.Text.Lazy as T

main = do
    c <- loadCommentsDat
    encodeFile "comments.yaml" $ commentsToSO c
