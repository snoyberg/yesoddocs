import Text.Hakyll (hakyll)
import Text.Hakyll.Render (css, static)
import Text.Hakyll.File (directory)
import Text.Hakyll.Render (renderChain)
import Text.Hakyll.CreateContext (createPage)

main = hakyll "http://www.yesodweb.com" $ do
    directory css "css"
    directory static "static"

    static "favicon.ico"

    renderPlain "index.html"
    renderPlain "screencasts/index.html"
    renderPlain "examples/index.html"
    renderPlain "book/index.html"
    renderPlain "articles.markdown"

    renderPlain "five-minutes.markdown"

    renderPlain "yesod/tutorial/blog.lhs"
    renderPlain "yesod/tutorial/ajax.lhs"
    renderPlain "yesod/tutorial/pretty-yaml.lhs"
    renderPlain "yesod/tutorial/i18n.lhs"
    renderPlain "yesod/tutorial/widgets.lhs"
    renderPlain "yesod/tutorial/form.lhs"

    renderPlain "hamlet/synopsis.lhs"
    renderPlain "web-routes-quasi/synopsis.markdown"
    renderPlain "persistent/synopsis.lhs"

    renderBook "book/introduction.markdown"
    renderBook "book/basics.markdown"
    renderBook "book/templates.markdown"
    renderBook "book/handler.markdown"
    renderBook "book/tools.markdown"
    renderBook "book/wai.markdown"
    renderBook "book/hamlet.markdown"
    renderBook "book/forms.markdown"
    renderBook "book/deploying.markdown"
    renderBook "book/persistent.markdown"
    renderBook "book/web-routes-quasi.markdown"
    renderBook "book/widgets.markdown"

    where renderPlain = renderChain [] . createPage
          renderBook = renderChain ["book-template.html"] . createPage
