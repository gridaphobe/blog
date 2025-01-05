{-# LANGUAGE BangPatterns #-}

module Post
  ( Post(..)
  , Format(..)
  , loadPosts
  , mostRecent
  , sorted
  ) where

import           Control.Monad
import           Data.List
import           Data.Map                     (Map)
import qualified Data.Map                     as M
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
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
loadPost path = runIOorExplode $ do
    mkd <- readFileStrict path
    p@(Pandoc m _) <- readMarkdown readerOptions (T.decodeUtf8 mkd)
    content <- writeHtml5 writerOptions p
    return Post { title   = stringify $ docTitle m
                , slug    = T.pack $ takeBaseName path
                , content = content
                , authors = map stringify $ docAuthors m
                , date    = parseTimeOrError True defaultTimeLocale fmt . T.unpack . stringify $ docDate m
                , format  = f
                }
  where
    fmt = "%a, %d %b %Y %T %Z"
    f = case takeExtension path of
        ".md" -> MD
        ".lhs" -> LHS
        x -> error $ "Unrecognized format: " ++ x

    exts | f == LHS  = enableExtension Ext_literate_haskell pandocExtensions
         | otherwise = pandocExtensions

    readerOptions :: ReaderOptions
    readerOptions = def { readerExtensions = exts }

    writerOptions :: WriterOptions
    writerOptions = def { writerHtmlQTags      = True
                        , writerHighlightStyle = Just pygments
                        , writerExtensions     = exts
                        }
