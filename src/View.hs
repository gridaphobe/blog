{-# LANGUAGE OverloadedStrings #-}

module View where

import           Data.List
import           Data.Text                   (Text)
import qualified Data.Text.Lazy              as L
import           Data.Time.Format
import qualified Clay                        as C
import           Text.Blaze.Internal         (preEscapedText)
import           Text.Blaze.Html5            (Html, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Text
import           Skylighting.Format.HTML
import           Skylighting.Styles


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
            "I work on Domain-Specific Languages for Economic Modeling at Bridgewater Associates. "
            "Before that, I worked on Distributed Systems at Bloomberg LP. "
            "I have a PhD in Programming Languages from UC San Diego, with a focus on diagnosability of formal methods (e.g. type systems). "
            "Here you'll find a collection of thoughts, a list of current and past projects, and a list of publications."
        H.p $ do
            "If you want to get in touch, you can reach me at "
            H.a ! A.href "mailto:blog@eric.seidel.io" $ "eric@seidel.io"
            "."
        H.h3 "Recent Posts"
        renderPostLinks ps

feed :: [Post] -> L.Text
feed ps =
    mconcat [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
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

mkPublication :: Html -> H.AttributeValue -> Html -> [Html] -> Html
mkPublication title pdf venue authors = H.li $ do
  H.a ! A.href pdf $ title
  preEscapedText " &mdash; "
  venue
  H.br
  foldr1 (>>) $ intersperse ", " authors

me = H.b "Eric L. Seidel"

publications :: L.Text
publications = layout "Eric Seidel" $ do
    H.h1 "Publications"
    H.ul ! A.class_ "pubs" $ do
        mkPublication "Dynamic Witnesses for Static Type Errors (journal extension)"
                      "/pub/nanomaly-jfp.pdf"
                      "JFP, vol. 28, 2018"
                      [ me, "Ranjit Jhala", "Westley Weimer" ]
        mkPublication "Data-Driven Techniques for Type Error Diagnosis"
                      "/pub/seidel-dissertation.pdf"
                      "PhD Dissertation 2017"
                      [ me ]
        mkPublication "Learning to Blame"
                      "/pub/nate-oopsla17.pdf"
                      "OOPSLA 2017"
                      [ me, "Huma Sibghat", "Kamalika Chaudhuri", "Westley Weimer", "Ranjit Jhala" ]
        mkPublication "Dynamic Witnesses for Static Type Errors"
                      "/pub/nanomaly-icfp16.pdf"
                      "ICFP 2016"
                      [ me, "Ranjit Jhala", "Westley Weimer" ]
        mkPublication "Guilt Free Ivory"
                      "/pub/ivory-haskell15.pdf"
                      "Haskell 2015"
                      [ "Trevor Elliott", "Simon Winwood", "James Bielman"
                      , "Lee Pike", "Pat Hickey", "Jamey Sharp", me
                      , "John Launchbury" ]
        mkPublication "Type Targeted Testing"
                      "/pub/target-esop15.pdf"
                      "ESOP 2015"
                      [ me, "Niki Vazou", "Ranjit Jhala" ]
        mkPublication "LiquidHaskell: Experience with Refinement Types in the Real World"
                      "/pub/realworldliquid-haskell14.pdf"
                      "Haskell 2014"
                      [ "Niki Vazou", me, "Ranjit Jhala" ]
        mkPublication "Refinement Types for Haskell"
                      "/pub/haskellrefinements-icfp14.pdf"
                      "ICFP 2014 (Awarded Most Influential Paper at ICFP 2024)"
                      [ "Niki Vazou", me, "Ranjit Jhala"
                      , "Dimitrios Vytiniotis", "Simon Peyton-Jones" ]
        mkPublication "Metadata Management in Scientific Computing"
                      "/pub/fluidinfo-bwupep11.pdf"
                      "JOCSE 2012"
                      [ me ]
        mkPublication "Designing a Virtual Environment to Evaluate Multimodal Sensors for Assisting the Visually Impaired"
                      "/pub/virtualenv-icchp12.pdf"
                      "ICCHP 2012"
                      [ "Wai L. Khoo", me, "Zhigang Zhu" ]
        mkPublication "Component Specification in the Cactus Framework: The Cactus Configuration Language"
                      "/pub/ccl-cbhpc10.pdf"
                      "GRID 2010"
                      [ "Gabrielle Allen", "Tom Goodale", "Frank Löffler"
                      , "David Rideout", "Erik Schnetter", me ]
        mkPublication "Simplifying Complex Software Assembly: The Component Retrieval Language and Implementation"
                      "/pub/crl-tg10.pdf"
                      "TeraGrid 2010"
                      [ me, "Gabrielle Allen", "Steven Brandt"
                      , "Frank Löffler", "Erik Schnetter" ]


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
        H.link ! A.href "/feed.xml" ! A.type_ "application/atom+xml"
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
                        H.a ! A.href "//gridaphobe.github.io/" $ "Projects"
                        H.span ! A.class_ "divider" $ "|"
                    H.li $ do
                        H.a ! A.href "/publications" $ "Publications"
                        H.span ! A.class_ "divider" $ "|"
                    H.li $ do
                        H.a ! A.href "/cv.pdf" $ "Resumé"
            H.section body
            H.footer ! A.class_ "footer" $ do
                H.p $ do
                    preEscapedText "Copyright &copy; Eric Seidel, 2012"
  where
    css href = H.link ! A.href href ! A.type_ "text/css" ! A.rel "stylesheet"

------------------------------------------------------------------------------
-- | Extra styling
style :: L.Text
style = C.render $ do
    ".container" C.? do
        C.width (C.px 750)
        C.paddingTop (C.px 20)
    "a code.url" C.? do
        C.color "#08C"
    "h1, h2, h3, h4, h5, h6" C.? do
        C.fontFamily ["Ubuntu"] [C.sansSerif]
    "pre, code" C.? do
        C.fontFamily ["Inconsolata"] [C.monospace]
        C.color C.black
        C.borderStyle C.none
    "nav" C.? do
        C.marginLeft C.auto
        C.marginRight C.auto
        C.textAlign (C.alignSide C.sideCenter)
        C.width C.auto
        "ul li a" C.? do
            C.fontFamily ["Ubuntu"] [C.sansSerif]
            C.color C.black
    "footer" C.? do
        C.marginTop (C.px 5)
        C.borderTop (C.px 1) C.solid "#E5E5E5"
        "p" C.? do
            C.textAlign (C.alignSide C.sideCenter)
            C.fontSize (C.pct 80)
    "section h1" C.? do
        C.marginBottom (C.px 15)
        C.borderBottom (C.px 1) C.solid "#E5E5E5"
    ".pull-right" C.?
        C.paddingLeft (C.px 20)
    "ul.pubs" C.? C.listStyleType C.none
    "ul.pubs > li" C.?
        C.marginBottom (C.em 0.5)

code :: L.Text
code = L.pack $ styleToCss pygments
