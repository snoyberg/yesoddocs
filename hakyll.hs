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

    renderPlain "yesod/tutorial/blog.lhs"
    renderPlain "yesod/tutorial/ajax.lhs"
    renderPlain "yesod/tutorial/pretty-yaml.lhs"
    renderPlain "yesod/tutorial/i18n.lhs"
    renderPlain "yesod/tutorial/widgets.lhs"

    render "overview.markdown"

    render "yesod/index.markdown"
    render "yesod/tutorial/index.markdown"
    render "yesod/changelog.markdown"
    render "yesod/helloworld.lhs"
    render "yesod/terminology.markdown"
    render "yesod/articles.markdown"
    render "yesod/screencast/hello-world.markdown"
    render "yesod/screencast/blog-part1.markdown"
    render "yesod/screencast/blog-part2.markdown"
    render "yesod/deploying.markdown"

    render "hamlet/index.markdown"
    render "hamlet/changelog.markdown"
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

    render "web-routes-quasi/index.markdown"
    render "web-routes-quasi/changelog.markdown"
    render "web-routes-quasi/synopsis.markdown"
    render "web-routes-quasi/syntax.markdown"
    render "web-routes-quasi/usage.markdown"

    render "persistent/index.markdown"
    render "persistent/changelog.markdown"
    render "persistent/synopsis.lhs"
    render "persistent/overview.markdown"
    render "persistent/defining-entities.markdown"
    render "persistent/type-classes.markdown"
    render "persistent/backends.markdown"
    render "persistent/relations.markdown"

    renderBook "book/introduction.markdown"
    renderBook "book/tools.markdown"
    renderBook "book/wai.markdown"
    renderBook "book/hamlet.markdown"
    renderBook "book/forms.markdown"
    renderBook "book/deploying.markdown"

    where render = renderChain ["template.html"]
                 . createPage
          renderPlain = renderChain [] . createPage
          renderBook = renderChain ["book-template.html"] . createPage
