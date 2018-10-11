{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error
import qualified Data.Map                    as M
import qualified Data.Text                   as ST
import qualified Data.Text.Lazy              as T
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           System.Directory
import           System.FilePath
import           System.Posix.Env
import           Web.Scotty hiding (next)
import           Web.Scotty.Trans (ActionT, next, ScottyError)

import           Hakyll

import           Post
import qualified View                        as V


dropDirectory :: FilePath -> FilePath
dropDirectory = joinPath . drop 1 . splitPath

main :: IO ()
main = do
  posts <- loadPosts "resources/posts"
  hakyllWith (defaultConfiguration {destinationDirectory="docs"}) $ do
    -- Static files
    match ("resources/static/*" .||. "resources/static/css/*" .||.
           "resources/static/img/*" .||. "resources/static/js/*" .||.
           "resources/static/pub/*") $ do
      route $ customRoute $ dropDirectory . dropDirectory . toFilePath
      compile copyFileCompiler

    create ["css/style.css"] $ do
      route idRoute
      compile $ do
        makeItem (T.unpack V.style)
        >>= relativizeUrls

    create ["css/code.css"] $ do
      route idRoute
      compile $ do
        makeItem (T.unpack V.code)
        >>= relativizeUrls

    create ["index.html"] $ do
      route idRoute
      compile $ do
        makeItem (T.unpack (V.index (mostRecent 5 posts)))
        >>= relativizeUrls

    create ["feed.xml"] $ do
      route idRoute
      compile $ do
        makeItem (T.unpack (V.feed (sorted posts)))
        >>= relativizeUrls

    create ["posts/index.html"] $ do
      route idRoute
      compile $ do
        makeItem (T.unpack (V.archive (sorted posts)))
        >>= relativizeUrls

    create ["publications/index.html"] $ do
      route idRoute
      compile $ do
        makeItem (T.unpack V.publications)
          >>= relativizeUrls

    forM_ posts $ \post -> do
      create [fromFilePath $ "posts" </> ST.unpack (slug post) </> "index.html"] $ do
        route idRoute
        compile $ do
          makeItem (T.unpack (V.post post))
          >>= relativizeUrls

    create ["cv.pdf", "resume.pdf"] $ do
      route idRoute
      compile $ makeItem $ CopyFile "resources/static/Eric_Seidel_Resume.pdf"

    create ["404.html"] $ do
      route idRoute
      compile $ do
        makeItem (T.unpack V.notFound)
          >>= relativizeUrls

    create ["CNAME"] $ do
      route idRoute
      compile $ do
        makeItem ("eric.seidel.io" :: String)

-- main :: IO ()
-- main = do
--     port <- getEnv "PORT"
--     posts <- loadPosts "resources/posts"
--     scotty (maybe 8000 read port) $ do
--         middleware logStdoutDev
--         middleware $ staticPolicy (noDots >-> addBase "resources/static")

--         get "/" $
--             html $ V.index $ mostRecent 5 posts

--         get "/feed" $
--             atom $ V.feed $ sorted posts

--         get "/css/style.css" $
--             css V.style

--         get "/css/code.css" $
--             css V.code

--         get "/posts" $
--             html $ V.archive $ sorted posts

--         get "/posts/:slug" $ do
--           s <- param "slug"
--           msum [ let f = "resources/posts/" ++ s
--                  in guardM (liftIO $ doesFileExist f) >> file f
--                , html . V.post =<< lookupM s posts
--                ]

--         get "/projects" $
--             redirect "http://gridaphobe.github.com"

--         get "/publications" $
--             html V.publications

--         get "/resume" $ file "resources/static/Eric_Seidel_Resume.pdf"
--         get "/cv"     $ file "resources/static/Eric_Seidel_Resume.pdf"
--         -- get "/resume" $ redirect "http://fluidcv.com/gridaphobe"
--         -- get "/cv"     $ redirect "http://fluidcv.com/gridaphobe"

--         notFound $
--             html V.notFound


-- --------------------------------------------------------------------------------
-- -- | Miscellaneous utilities for working with Scotty
-- --------------------------------------------------------------------------------

-- atom :: T.Text -> ActionM ()
-- atom t = do
--     text t
--     setHeader "Content-Type" "application/atom+xml; charset=utf-8"

-- css :: T.Text -> ActionM ()
-- css t = do
--     text t
--     setHeader "Content-Type" "text/css; charset=utf-8"

-- guardM :: (MonadPlus m) => m Bool -> m ()
-- guardM b = b >>= guard

-- lookupM :: (MonadPlus m, Ord k) => k -> M.Map k a -> m a
-- lookupM k m = case M.lookup k m of
--                 Nothing -> mzero
--                 Just v  -> return v

-- instance (ScottyError e, Alternative m, Monad m) => Alternative (ActionT e m) where
--     empty = next
--     a <|> b = a `catchError` \_ -> b

-- instance (ScottyError e, MonadPlus m) => MonadPlus (ActionT e m) where
