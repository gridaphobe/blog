{-# LANGUAGE OverloadedStrings #-}

{-|

This is where all the routes and handlers are defined for your site. The
'app' function is the initializer that combines everything together and
is exported by this module.

-}

module Site
  ( app
  ) where

import           Control.Monad.State
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as B
import           Data.List
import qualified Data.Map                    as M
import           Data.Monoid
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time.Format
import qualified Language.CSS                as C
import           Snap.Core
import           Snap.Snaplet
import           Snap.Util.FileServe
import           System.Directory
import           System.FilePath
import           System.Locale
import           Text.Blaze                  (Html, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Renderer.Text
import           Text.Pandoc.Highlighting


import           Application                 hiding (posts)
import           Post                        (Post, loadPosts)
import qualified Post                        as P
import qualified Feed                        as F


writeHtml :: Html -> AppHandler ()
writeHtml h = do
  modifyResponse $ addHeader "Content-Type" "text/html; charset=utf-8"
  writeLazyText $ renderHtml h

------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
indexH :: AppHandler ()
indexH = do
    posts <- gets _posts
    writeHtml $ layout "Eric Seidel" $ do
        H.img ! A.class_ "pull-right" ! A.title "Eric Seidel"
              ! A.src "/img/eric.jpg"
        H.h1 "Eric Seidel"
        H.p $ mconcat
            [ "I'm Eric Seidel. I am finishing up my BS in Computer Science, "
            , "and will be starting my PhD in the fall. Here you'll find a "
            , "collection of thoughts, a list of current and past projects, "
            , "and a list of publications."
            ]
        H.p $ do
            "If you want to get in touch, you can reach me at "
            H.a ! A.href "mailto:gridaphobe@gmail.com" $ "gridaphobe@gmail.com"
            " or "
            H.a ! A.href "http://twitter.com/gridaphobe" $ "@gridaphobe"
            " on Twitter."
        H.h3 "Recent Posts"
        renderPostLinks $ take 5 $ reverse $ sort $ M.elems posts

feedH :: AppHandler ()
feedH = do
    posts <- gets _posts
    modifyResponse $ addHeader "Content-Type" "application/atom+xml; charset=utf-8"
    writeLazyText "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
    writeLazyText $ renderHtml $ F.feed $ reverse $ sort $ M.elems posts

archiveH :: AppHandler ()
archiveH = do
    posts <- gets _posts
    writeHtml $ layout "Eric Seidel" $ do
        H.h1 "All Posts"
        renderPostLinks $ reverse $ sort $ M.elems posts

renderPost :: Post -> Html
renderPost post = do
    H.h1 $ do
        H.toHtml $ P.title post
        H.small ! A.class_ "pull-right" $ H.toHtml $
            formatTime defaultTimeLocale "Posted on %B %d, %Y" $ P.date post
    H.article $ P.content post


renderPostLinks :: [Post] -> Html
renderPostLinks posts = do
    H.ul $ mconcat $ map postLink posts
  where
    postLink post = do
        H.li $ do
            H.a ! A.href (H.toValue $ "/posts/" `mappend` P.slug post) $
                H.toHtml $ P.title post
            H.preEscapedText " &ndash; "
            H.toHtml $ formatTime defaultTimeLocale "%B %d, %Y" $ P.date post

------------------------------------------------------------------------------
-- | Renders a single post
postH :: AppHandler ()
postH = do
    Just slug <- liftM (fmap B.unpack) (getParam "slug")
    posts <- gets _posts
    if takeExtension slug == ".lhs"
        then do let f = "resources/posts/" ++ slug
                b <- liftIO $ doesFileExist f
                if b then serveFile f else pass
        else case M.lookup slug posts of
                    Just post -> writeHtml $ layout (P.title post)
                                                    (renderPost post)
                    Nothing -> pass

publicationsH :: AppHandler ()
publicationsH =
    writeHtml $ layout "Eric Seidel" $ do
        H.h1 "Publications"
        H.h3 "Papers"
        H.ul $ do
            H.li $ do
                "E. L. Seidel. 2012. "
                H.a ! A.href "/pub/fluidinfo-bwupep11.pdf" $ H.em
                    "Metadata Management in Scientific Computing"
                ". Journal of Computational Science Education. Accepted for publication."
            H.li $ do
                "G. Allen, T. Goodale, F. Löffler, D. Rideout, E. Schnetter, "
                "and E. L. Seidel. 2010. "
                H.a ! A.href "/pub/ccl-cbhpc10.pdf" $ H.em
                    "Component Specification in the Cactus Framework: The Cactus Configuration Language"
                ". Grid Computing (GRID), 2010 11th IEEE/ACM International Conference on. DOI="
                H.a ! A.href "http://dx.doi.org/10.1109/GRID.2010.5698008" $
                    "10.1109/GRID.2010.5698008"
            H.li $ do
                "E. L. Seidel, G. Allen, S. Brandt, F. Löffler, and E. Schnetter. 2010. "
                H.a ! A.href "/pub/crl-tg10.pdf" $ H.em
                    "Simplifying Complex Software Assembly: The Component Retrieval Language and Implementation"
                ". In Proceedings of the 2010 Teragrid Conference "
                "(Pittsburgh, Pennsylvania, August 02 – 05, 2010). TG '10. ACM, "
                "New York, NY, 1-8. DOI="
                H.a ! A.href "http://doi.acm.org/10.1145/1838574.1838592" $
                    "10.1145/1838574.1838592"
        H.h3 "Posters"
        H.ul $ do
            H.li $ do
                "E. L. Seidel and G. Allen. 2011. "
                H.a ! A.href "/pub/fluidinfo-poster.pdf" $ H.em
                    "Metadata Management in Scientific Computing"
                ". Presented at TeraGrid '11 in Salt Lake City, UT."
            H.li $ do
                "E. L. Seidel, G. Allen, S. Brandt, F. Löffler, and E. Schnetter. 2010. "
                H.a ! A.href "/pub/crl-poster.pdf" $ H.em
                    "Simplifying Complex Software Assembly: The Component Retrieval Language and Implementation"
                ". Presented at TeraGrid '10 in Pittsburgh, PA."
        H.h3 "Presentations"
        H.ul $ do
            H.li $ do
                "E. L. Seidel, G. Allen, S. Brandt, F. Löffler, and E. Schnetter. 2010. "
                H.a ! A.href "/pub/crl-presentation.pdf" $ H.em
                    "Simplifying Complex Software Assembly: The Component Retrieval Language and Implementation"
                ". Presented at TeraGrid '10 in Pittsburgh, PA."



notFoundH :: AppHandler ()
notFoundH = do
    modifyResponse $ setResponseCode 404
    writeHtml $ layout "Eric Seidel" $ do
        H.h1 "Not Found"
        H.p "Sorry, couldn't find that... Try again?"

------------------------------------------------------------------------------
-- | Default layout
layout :: Text -> Html -> Html
layout title body = H.docTypeHtml $ do
    H.head $ do
        H.meta ! A.httpEquiv "content-type" ! A.content "text/html;charset=utf-8"
        H.meta ! A.httpEquiv "content-language" ! A.content "en"
        H.meta ! A.name "author" ! A.content "Eric Seidel"
        H.meta ! A.name "keywords" !
            A.content "eric seidel,eric,seidel,eseidel,gridaphobe"
        H.meta ! A.name "icon" ! A.href "/favicon.ico" ! A.type_ "image/x-icon"
        H.title $ H.toHtml title
        -- H.link ! A.href "http://feeds.feedburner.com/gridaphobe"
        --     ! A.type_ "application/rss+xml" ! A.rel "alternate"
        --     ! A.title "Eric Seidel"
        H.link ! A.href "/feed" ! A.type_ "application/atom+xml"
            ! A.rel "alternate" ! A.title "Eric Seidel"
        css "http://fonts.googleapis.com/css?family=Ubuntu|Vollkorn|Inconsolata"
        css "/css/bootstrap.min.css"
        css "/css/style.css"
        css "/css/code.css"
    H.body $ do
        H.div ! A.class_ "container" $ do
            H.nav $ do
                H.ul ! A.class_ "breadcrumb" $ do
                    H.li $ do
                        H.a ! A.href "/" $ "Home"
                        H.span ! A.class_ "divider" $ "|"
                    H.li $ do
                        H.a ! A.href "/posts" $ "Archive"
                        H.span ! A.class_ "divider" $ "|"
                    H.li $ do
                        H.a ! A.href "/projects" $ "Projects"
                        H.span ! A.class_ "divider" $ "|"
                    H.li $ do
                        H.a ! A.href "/publications" $ "Publications"
                        H.span ! A.class_ "divider" $ "|"
                    H.li $ do
                        H.a ! A.href "/cv" $ "Resumé"
            H.section body
            H.footer ! A.class_ "footer" $ do
                H.p $ do
                    H.preEscapedText "Copyright &copy; Eric Seidel, 2012"
  where
    css href = H.link ! A.href href ! A.type_ "text/css" ! A.rel "stylesheet"

------------------------------------------------------------------------------
-- | Extra styling
styleH :: AppHandler ()
styleH = do
    modifyResponse $ addHeader "Content-Type" "text/css; charset=utf-8"
    writeLazyText $ C.renderCSS $ C.runCSS $ do
        C.rule ".container" $ do
            C.width "750px"
            C.paddingTop "20px"
        C.rule "a code.url" $ do
            C.color "#08C"
        C.rule "h1, h2, h3, h4, h5, h6" $ do
            C.fontFamily "Ubuntu, sans-serif"
        C.rule "pre, code" $ do
            C.fontFamily "Inconsolata, monospace"
            C.color "black"
            C.borderStyle "none"
        C.rule "nav ul li a" $ do
            C.fontFamily "Ubuntu, sans-serif"
            C.color "black"
        C.rule "footer" $ do
            C.marginTop "5px"
            C.borderTop "1px solid #E5E5E5"
            C.rule "p" $ do
                C.textAlign "center"
                C.fontSize "80%"
        C.rule "section h1" $ do
            C.marginBottom "15px"
            C.borderBottom "1px solid #E5E5E5"

codeH :: AppHandler ()
codeH = do
    modifyResponse $ addHeader "Content-Type" "text/css; charset=utf-8"
    writeText $ T.pack $ styleToCss pygments

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, AppHandler ())]
routes = exactly [ ("",              indexH)
                 , ("atom.xml",      feedH)
                 , ("feed",          feedH)
                 , ("css/style.css", styleH)
                 , ("css/code.css",  codeH)
                 , ("posts/:slug",   postH)
                 , ("posts",         archiveH)
                 , ("projects",      redirect "http://github.com/gridaphobe")
                 , ("publications",  publicationsH)
                 , ("resume",        redirect "http://fluidcv.com/gridaphobe")
                 , ("cv",            redirect "http://fluidcv.com/gridaphobe")
                 ] `mappend`
                 [ ("", serveDirectory "resources/static")
                 , ("", notFoundH)
                 ]
  where
    exactly = map (\(p, h) -> (p, ifTop h))

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "A simple blog using Pandoc." Nothing $ do
    posts <- liftIO $ loadPosts "resources/posts"
    addRoutes routes
    return $ App posts
