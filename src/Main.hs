{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import qualified Data.Text                   as ST
import qualified Data.Text.Lazy              as T
import           System.FilePath

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
