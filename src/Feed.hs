{-# LANGUAGE OverloadedStrings #-}
module Feed (feed) where

import           Data.Monoid
import           Data.Time.Format
import           Text.Blaze
import           Text.Blaze.Html5 (Html, toHtml)
import           Text.Blaze.Html5.Attributes (rel, href)
import           Text.Blaze.Internal
import           Text.Blaze.Html.Renderer.Text

import           Post (Post)
import qualified Post as P

feed :: [Post] -> Html
feed ps = feed_ ! customAttribute "xmlns" "http://www.w3.org/2005/Atom" $ do
    title_ "Eric Seidel"
    link_ ! rel "self" ! href "http://eseidel.org/feed"
    link_ ! href "http://eseidel.org/"
    updated_ $ toHtml $ formatTime defaultTimeLocale "%FT%XZ" $ P.date $ head ps
    id_ "tag:gridaphobe.blog,2012:"
    author_ $ do
        name_ "Eric Seidel"
        email_ "gridaphobe@gmail.com"
    mapM_ entry ps


entry :: Post -> Html
entry p = entry_ $ do
    title_ $ toHtml $ P.title p
    link_ ! href ("http://eseidel.org/posts/" `mappend` toValue (P.slug p))
    published_ $ toHtml $ formatTime defaultTimeLocale "%FT%XZ" $ P.date p
    updated_ $ toHtml $ formatTime defaultTimeLocale "%FT%XZ" $ P.date p
    id_ $ toHtml $ "tag:gridaphobe.blog,2012:" `mappend` P.slug p
    content_ ! customAttribute "type" "html" $ toHtml $ renderHtml $ P.content p

feed_, entry_, title_, updated_, id_, content_, author_, name_, email_, published_ :: Html -> Html
feed_ = Parent "feed" "<feed" "</feed>"
entry_ = Parent "entry" "<entry" "</entry>"
title_ = Parent "title" "<title" "</title>"
updated_ = Parent "updated" "<updated" "</updated>"
published_ = Parent "published" "<published" "</published>"
id_ = Parent "id" "<id" "</id>"
content_ = Parent "content" "<content" "</content>"
author_ = Parent "author" "<author" "</author>"
name_ = Parent "name" "<name" "</name>"
email_ = Parent "email" "<email" "</email>"

link_ :: Html
link_ = Leaf "link" "<link" "/>" ()
