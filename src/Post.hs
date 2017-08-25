{-# LANGUAGE BangPatterns #-}

module Post
  ( Post(..)
  , Format(..)
  , loadPosts
  , mostRecent
  , sorted
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Map                     (Map)
import qualified Data.Map                     as M
import qualified Data.Set                     as S
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Time.Clock              (UTCTime)
import           Data.Time.Format
import           System.Directory
import           System.FilePath
import           Text.Blaze.Html5             (Html)
import           Skylighting.Styles
import           Text.Pandoc hiding (Format)
import           Text.Pandoc.Shared

data Format = MD | LHS deriving (Eq)

data Post = Post
    { title   :: !Text
    , slug    :: !Text
    , content :: !Html
    , authors :: ![Text]
    , date    :: !UTCTime
    , format  :: !Format
    }

instance Eq Post where
    a == b = slug a == slug b

instance Ord Post where
    compare a b = compare (date a) (date b)

mostRecent :: Int -> Map String Post -> [Post]
mostRecent n = take n . sortBy (flip compare) . M.elems

sorted :: Map String Post -> [Post]
sorted = sortBy (flip compare) . M.elems

loadPosts :: FilePath -> IO (Map String Post)
loadPosts dir = do
    posts <- liftM (filter (\p -> takeExtension p `elem` [".md", ".lhs"]))
                   (getDirectoryContents dir)
             >>= mapM (loadPost . combine dir)
    return $! M.fromList [(T.unpack $ slug p, p) | p <- posts]

loadPost :: FilePath -> IO Post
loadPost path = do
    Right p@(Pandoc m _) <- readMarkdown readerOptions <$> readFile path
    return Post { title   = T.pack $ stringify $ docTitle m
                , slug    = T.pack $ takeBaseName path
                , content = writeHtml writerOptions p
                , authors = map (T.pack . stringify) $ docAuthors m
                , date    = readTime defaultTimeLocale fmt $ stringify $ docDate m
                , format  = f
                }
  where
    fmt = "%a, %d %b %Y %T %Z"
    f = case takeExtension path of
        ".md" -> MD
        ".lhs" -> LHS
        x -> error $ "Unrecognized format: " ++ x

    exts | f == LHS  = S.insert Ext_literate_haskell pandocExtensions
         | otherwise = pandocExtensions

    readerOptions :: ReaderOptions
    readerOptions = def { readerExtensions = exts }

    writerOptions :: WriterOptions
    writerOptions = def { writerHtml5          = True
                        , writerHtmlQTags      = True
                        , writerHighlight      = True
                        , writerHighlightStyle = pygments
                        , writerExtensions     = exts
                        }
