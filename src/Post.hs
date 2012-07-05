{-# LANGUAGE BangPatterns #-}

module Post
  ( Post(..)
  , Format(..)
  , loadPosts
  ) where

import           Control.Monad
import           Data.Map                     (Map)
import qualified Data.Map                     as M
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Time.Clock              (UTCTime)
import           Data.Time.Format
import           System.Directory
import           System.FilePath
import           System.Locale
import           Text.Blaze.Html5             (Html)
import           Text.Pandoc.Definition       (Pandoc (..), Meta (..))
import           Text.Pandoc.Parsing
import           Text.Pandoc.Readers.Markdown
import           Text.Pandoc.Shared
import           Text.Pandoc.Writers.HTML

data Format = MD | LHS deriving (Eq)

data Post = Post
    { title   :: Text
    , slug    :: Text
    , content :: Html
    , authors :: [Text]
    , date    :: UTCTime
    , format  :: Format
    }

instance Eq Post where
    a == b = slug a == slug b

instance Ord Post where
    compare a b = compare (date a) (date b)

loadPosts :: FilePath -> IO (Map String Post)
loadPosts dir = do
    posts <- liftM (filter (\p -> takeExtension p `elem` [".md", ".lhs"]))
                   (getDirectoryContents dir)
             >>= mapM (loadPost . combine dir)
    return $ M.fromList [(T.unpack $ slug p, p) | p <- posts]

loadPost :: FilePath -> IO Post
loadPost path = do
    !p@(Pandoc (Meta t as d) _) <- liftM (readMarkdown parserState)
                                         (readFile path)
    return Post { title   = T.pack $ stringify t
                , slug    = T.pack $ takeBaseName path
                , content = writeHtml writerOptions p
                , authors = map (T.pack . stringify) as
                , date    = readTime defaultTimeLocale fmt $ stringify d
                , format  = f
                }
  where
    fmt = "%a, %d %b %Y %T %Z"
    f = case takeExtension path of
        ".md" -> MD
        ".lhs" -> LHS
        x -> error $ "Unrecognized format: " ++ x

    parserState :: ParserState
    parserState = defaultParserState { stateSmart = False
                                     , stateLiterateHaskell = f == LHS
                                     }

    writerOptions :: WriterOptions
    writerOptions = defaultWriterOptions { writerHtml5     = True
                                         , writerHighlight = True
                                         , writerLiterateHaskell = f == LHS
                                         }
