--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Data.Char (isAscii, isSpace)
import qualified Text.Feed.Import as FI
import qualified Text.Feed.Query as FQ
import qualified Text.Feed.Types as FT
import           Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Maybe (fromMaybe)
import Control.Monad (join)
import Data.Time.Format
import System.Locale
import Data.Time.Clock


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "templates/*" $ compile templateCompiler

    match pagesPattern $ do
        route $ setExtension "html"
        compile $ do
            pages <- loadAll (pagesPattern .&&. hasVersion "menu")
            f <- unsafeCompiler getFeed
            let indexCtx =
                    listField "pages" defaultContext (return pages) `mappend`
                    feedListCtx f `mappend`
                    defaultContext

            pandocCompiler
                >>= stripNbspCompiler
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match pagesPattern $ version "menu" $ do
        route $ setExtension "html"
        compile getResourceBody


-- Fetch and parse the RSS feed.
getFeed :: IO FT.Feed
getFeed = do
    k <- simpleHttp "https://frted.wordpress.com/feed/"
    case FI.parseFeedString $ BLC.unpack k of
         Nothing -> fail "Failed to parse downloaded feed"
         Just f -> return f

-- Context for a Feed (a list of posts). Takes the Feed object.
feedListCtx :: FT.Feed -> Context String
feedListCtx f = listField "posts" feedPostCtx posts
  where posts = return $ map mkItem $ FQ.feedItems f

-- Context of an individual post
feedPostCtx :: Context FT.Item
feedPostCtx = feedField "title" FQ.getItemTitle `mappend`
              feedField "url" FQ.getItemLink `mappend`
              feedDateField

-- Helper to make context fields from feed items
--feedField :: String -> FQ.ItemGetter String -> Context FT.Item
feedField :: String -> (a -> Maybe String) -> Context a
feedField name accessor = field name (return . handleError . accessor . itemBody)
  where handleError = fromMaybe (error ("[ERROR] feed item didn't contain " ++ name))

feedDateField :: Context FT.Item
feedDateField = field "date" (return . (formatTime defaultTimeLocale "%D" :: UTCTime -> String) . handleError . join . FQ.getItemPublishDate . itemBody)
  where handleError = fromMaybe (error "[ERROR] feed item didn't contain parseable date")
-- feedField "date" (fmap (formatTime defaultTimeLocale "%D" :: UTCTime -> String) . join . FQ.getItemPublishDate)

-- Used to make Items, which are needed for listField
-- Items have an Identifier along with their content.
-- Here, we use the link, interpreted as a file path
-- Sketchy?
mkItem :: FT.Item -> Item FT.Item
mkItem i = Item (fromFilePath . fromMaybe (error "[ERROR] feed item didn't contain url") $ FQ.getItemLink i) i

stripNbspCompiler :: Item String -> Compiler (Item String)
stripNbspCompiler = return . fmap stripNbsp

-- remove funny unicode spaces
stripNbsp = map go
  where go x | not (isAscii x) && isSpace x = ' '
        go x | otherwise = x

pagesPattern = "*.html" .||. "*.markdown"
--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
