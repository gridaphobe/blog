{-# LANGUAGE OverloadedStrings #-}
module Feed (feed) where

import Data.Monoid
import Data.Time.Format
import System.Locale
import Text.Blaze
import Text.Blaze.Html5.Attributes (rel, href)
import Text.Blaze.Internal
import Text.Blaze.Renderer.String

import Post

feed :: [Post] -> Html
feed ps = feed_ ! customAttribute "xmlns" "http://www.w3.org/2005/Atom" $ do
    title_ "Eric Seidel"
    link_ ! rel "self" ! href "http://eseidel.org/atom.xml"
    link_ ! href "http://eseidel.org/"
    updated_ $ toHtml $ formatTime defaultTimeLocale "%FT%XZ" $ date $ head ps
    id_ "tag:gridaphobe.blog,2012:"
    author_ $ do
        name_ "Eric Seidel"
        email_ "gridaphobe@gmail.com"
    mapM_ entry ps


entry :: Post -> Html
entry p = entry_ $ do
    title_ $ toHtml $ title p
    link_ ! href ("http://eseidel.org/posts/" `mappend` toValue (slug p))
    published_ $ toHtml $ formatTime defaultTimeLocale "%FT%XZ" $ date p
    updated_ $ toHtml $ formatTime defaultTimeLocale "%FT%XZ" $ date p
    id_ $ toHtml $ "tag:gridaphobe.blog,2012:" `mappend` slug p
    content_ ! customAttribute "type" "html" $ toHtml $ renderHtml $ content p

feed_, entry_, title_, updated_, id_, content_, author_, name_, email_, published_ :: HtmlM a -> HtmlM b
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

link_ :: HtmlM a
link_ = Leaf "link" "<link" "/>"
