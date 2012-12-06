{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Map                    as M
import qualified Data.Text.Lazy              as T
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           System.Directory
import           System.FilePath

import           Web.Scotty

import           Post
import qualified View                        as V


main :: IO ()
main = do
    posts <- loadPosts "resources/posts"
    scotty 8000 $ do
        middleware logStdoutDev
        middleware $ staticPolicy (noDots >-> addBase "resources/static")

        get "/" $
            html $ V.index $ mostRecent 5 posts

        get "/feed" $
            atom $ V.feed $ sorted posts

        get "/css/style.css" $
            css $ V.style

        get "/css/code.css" $
            css $ V.code

        get "/posts" $
            html $ V.archive $ sorted posts

        get "/posts/:slug" $ do
            s <- param "slug"
            if takeExtension s == ".lhs"
                then do let f = "resources/posts/" ++ s
                        whenM (liftIO $ doesFileExist f) (file f)
                        next
                else case M.lookup s posts of
                    Just p -> html $ V.post p
                    Nothing -> next

        get "/projects" $ redirect "http://gridaphobe.github.com"

        get "/publications" $
            html $ V.publications

        get "/resume" $ redirect "http://fluidcv.com/gridaphobe"
        get "/cv"     $ redirect "http://fluidcv.com/gridaphobe"

        notFound $ html V.notFound


atom :: T.Text -> ActionM ()
atom t = do
    text t
    header "Content-Type" "application/atom+xml; charset=utf-8"

css :: T.Text -> ActionM ()
css t = do
    text t
    header "Content-Type" "text/css; charset=utf-8"


whenM :: (Monad m) => m Bool -> m () -> m ()
whenM b m = do
    b' <- b
    when b' m
