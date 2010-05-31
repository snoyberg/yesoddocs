import Text.Hakyll (hakyll)
import Text.Hakyll.Render (css, static)
import Text.Hakyll.File (directory)
import Text.Hakyll.Render (renderChain)
import Text.Hakyll.CreateContext (createPage)

main = hakyll "http://www.yesodweb.com" $ do
    directory css "css"
    directory static "static"

    static "favicon.ico"

    renderChain [] $ createPage "index.html"
    render "overview.markdown"

    render "yesod/index.markdown"
    render "yesod/helloworld.lhs"
    render "yesod/terminology.markdown"
    render "yesod/tutorial/blog.lhs"
    render "yesod/tutorial/ajax.lhs"
    render "yesod/tutorial/chat.lhs"
    render "yesod/tutorial/pretty-yaml.lhs"
    render "yesod/tutorial/i18n.lhs"
    render "yesod/deploying.markdown"

    render "hamlet/index.markdown"
    render "hamlet/synopsis.lhs"
    render "hamlet/syntax.markdown"
    render "hamlet/indentation.markdown"
    render "hamlet/doctypes.markdown"
    render "hamlet/lineparsing.markdown"
    render "hamlet/tagparsing.markdown"
    render "hamlet/contentparsing.markdown"
    render "hamlet/conditionals.markdown"
    render "hamlet/references.markdown"
    render "hamlet/loops.markdown"
    render "hamlet/maybe.markdown"
    render "hamlet/monad.markdown"

    render "web-routes-quasi/index.markdown"
    render "web-routes-quasi/synopsis.markdown"
    render "web-routes-quasi/syntax.markdown"
    render "web-routes-quasi/usage.markdown"

    where render = renderChain ["template.html"]
                 . createPage
