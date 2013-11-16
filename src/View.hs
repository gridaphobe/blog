{-# LANGUAGE OverloadedStrings #-}

module View  where

import           Data.Monoid
import           Data.Text                   (Text)
import qualified Data.Text.Lazy              as L
import           Data.Time.Format
import qualified Language.CSS                as C
import           System.Locale
import           Text.Blaze.Internal         (preEscapedText)
import           Text.Blaze.Html5            (Html, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Text
import           Text.Highlighting.Kate.Format.HTML
import           Text.Highlighting.Kate.Styles


import           Post                        (Post)
import qualified Post                        as P
import qualified Feed                        as F


index :: [Post] -> L.Text
index ps =
    layout "Eric Seidel" $ do
        H.img ! A.class_ "pull-right" ! A.title "Eric Seidel"
              ! A.src "/img/eric.jpg"
        H.h1 "Eric Seidel"
        H.p $ do
            "I'm Eric Seidel. I am a second-year PhD student in Computer Science at "
            H.a ! A.href "http://cs.ucsd.edu" $ "UC San Diego"
            ". Here you'll find a "
            "collection of thoughts, a list of current and past projects, "
            "and a list of publications."
        H.p $ do
            "If you want to get in touch, you can reach me at "
            H.a ! A.href "mailto:gridaphobe@gmail.com" $ "gridaphobe@gmail.com"
            " or "
            H.a ! A.href "http://twitter.com/gridaphobe" $ "@gridaphobe"
            " on Twitter."
        H.h3 "Recent Posts"
        renderPostLinks ps

feed :: [Post] -> L.Text
feed ps =
    mconcat ["<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            , renderHtml $ F.feed ps
            ]

archive :: [Post] -> L.Text
archive ps = layout "Eric Seidel" $ do
    H.h1 "All Posts"
    renderPostLinks ps

renderPost :: Post -> Html
renderPost p = do
    H.h1 $ do
        H.toHtml $ P.title p
        H.small ! A.class_ "pull-right" $ H.toHtml $
            formatTime defaultTimeLocale "Posted on %B %d, %Y" $ P.date p
    H.article $ P.content p


renderPostLinks :: [Post] -> Html
renderPostLinks ps = do
    H.ul $ mconcat $ map postLink ps
  where
    postLink p = do
        H.li $ do
            H.a ! A.href (H.toValue $ "/posts/" `mappend` P.slug p) $
                H.toHtml $ P.title p
            preEscapedText " &ndash; "
            H.toHtml $ formatTime defaultTimeLocale "%B %d, %Y" $ P.date p

------------------------------------------------------------------------------
-- | Renders a single post
post :: Post -> L.Text
post p = layout (P.title p) (renderPost p)


publications :: L.Text
publications = layout "Eric Seidel" $ do
    H.h1 "Publications"
    H.h3 "Papers"
    H.ul $ do
        H.li $ do
            "E. L. Seidel. "
            H.a ! A.href "/pub/fluidinfo-bwupep11.pdf" $
                "Metadata Management in Scientific Computing."
            H.em " Journal of Computational Science Education 3 (2). "
            "2012."
        H.li $ do
            "W. L. Khoo, E. L. Seidel, and Z. Zhu. "
            H.a ! A.href "/pub/virtualenv-icchp12.pdf" $
                "Designing a Virtual Environment to Evaluate Multimodal Sensors for Assisting the Visually Impaired."
            H.em " In Proceedings of the 13th Iternational Conference on Computers Helping People with Special Needs (2). "
            "2012."
        H.li $ do
            "G. Allen, T. Goodale, F. Löffler, D. Rideout, E. Schnetter, "
            "and E. L. Seidel. "
            H.a ! A.href "/pub/ccl-cbhpc10.pdf" $
                "Component Specification in the Cactus Framework: The Cactus Configuration Language."
            H.em " In Proceedings of the 11th IEEE/ACM International Conference on Grid Computing. "
            "2010."
        H.li $ do
            "E. L. Seidel, G. Allen, S. Brandt, F. Löffler, and E. Schnetter. 2010. "
            H.a ! A.href "/pub/crl-tg10.pdf" $
                "Simplifying Complex Software Assembly: The Component Retrieval Language and Implementation."
            H.em " In Proceedings of the 2010 Teragrid Conference. "
            "2010."

    H.h3 "Posters"
    H.ul $ do
        H.li $ do
            "E. L. Seidel and G. Allen. "
            H.a ! A.href "/pub/fluidinfo-poster.pdf" $
                "Metadata Management in Scientific Computing"
            ". Presented at TeraGrid '11 in Salt Lake City, UT. 2011."
        H.li $ do
            "E. L. Seidel, G. Allen, S. Brandt, F. Löffler, and E. Schnetter. "
            H.a ! A.href "/pub/crl-poster.pdf" $
                "Simplifying Complex Software Assembly: The Component Retrieval Language and Implementation"
            ". Presented at TeraGrid '10 in Pittsburgh, PA. 2010."

    H.h3 "Presentations"
    H.ul $ do
        H.li $ do
            "E. L. Seidel, G. Allen, S. Brandt, F. Löffler, and E. Schnetter. "
            H.a ! A.href "/pub/crl-presentation.pdf" $
                "Simplifying Complex Software Assembly: The Component Retrieval Language and Implementation"
            ". Presented at TeraGrid '10 in Pittsburgh, PA. 2010. "



notFound :: L.Text
notFound = layout "Eric Seidel" $ do
    H.h1 "Not Found"
    H.p "Sorry, couldn't find that... Try again?"

------------------------------------------------------------------------------
-- | Default layout
layout :: Text -> Html -> L.Text
layout title body = renderHtml $ H.docTypeHtml $ do
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
        css "http://fonts.googleapis.com/css?family=Ubuntu|PT+Serif:400,700,400italic|Inconsolata"
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
                    preEscapedText "Copyright &copy; Eric Seidel, 2012"
  where
    css href = H.link ! A.href href ! A.type_ "text/css" ! A.rel "stylesheet"

------------------------------------------------------------------------------
-- | Extra styling
style :: L.Text
style = C.renderCSS $ C.runCSS $ do
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
    C.rule "nav" $ do
        C.marginLeft "auto"
        C.marginRight "auto"
        C.textAlign "center"
        C.width "auto"
        C.rule "ul li a" $ do
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
    C.rule ".pull-right" $
        C.paddingLeft "20px"

code :: L.Text
code = L.pack $ styleToCss pygments
