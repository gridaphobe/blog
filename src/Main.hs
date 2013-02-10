{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.IO.Class
import qualified Data.Map                    as M
import qualified Data.Text.Lazy              as T
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           System.Directory
import           System.FilePath
import           System.Posix.Env
import           Web.Scotty

import           Post
import qualified View                        as V


main :: IO ()
main = do
    port <- getEnv "PORT"
    posts <- loadPosts "resources/posts"
    scotty (maybe 8000 read port) $ do
        middleware logStdoutDev
        middleware $ staticPolicy (noDots >-> addBase "resources/static")

        get "/" $
            html $ V.index $ mostRecent 5 posts

        get "/feed" $
            atom $ V.feed $ sorted posts

        get "/css/style.css" $
            css V.style

        get "/css/code.css" $
            css V.code

        get "/posts" $
            html $ V.archive $ sorted posts

        get "/posts/:slug" $ \s -> msum
            [ let f = "resources/posts/" ++ s
              in guardM (liftIO $ doesFileExist f) >> file f
            , html . V.post =<< lookupM s posts
            ]

        get "/projects" $
            redirect "http://gridaphobe.github.com"

        get "/publications" $
            html V.publications

        get "/resume" $ redirect "http://fluidcv.com/gridaphobe"
        get "/cv"     $ redirect "http://fluidcv.com/gridaphobe"

        notFound $
            html V.notFound


--------------------------------------------------------------------------------
-- | Miscellaneous utilities for working with Scotty
--------------------------------------------------------------------------------

atom :: T.Text -> ActionM ()
atom t = do
    text t
    header "Content-Type" "application/atom+xml; charset=utf-8"

css :: T.Text -> ActionM ()
css t = do
    text t
    header "Content-Type" "text/css; charset=utf-8"

guardM :: (MonadPlus m) => m Bool -> m ()
guardM b = b >>= guard

lookupM :: (MonadPlus m, Ord k) => k -> M.Map k a -> m a
lookupM k m = case M.lookup k m of
                Nothing -> mzero
                Just v  -> return v

instance MonadPlus ActionM where
    mzero = next
    a `mplus` b = a `catchError` \_ -> b
