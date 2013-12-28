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
import           Data.Maybe (fromJust)
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
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match pagesPattern $ version "menu" $ do
        route $ setExtension "html"
        compile getResourceBody




getFeed :: IO FT.Feed
getFeed = do
    k <- simpleHttp "https://frted.wordpress.com/feed/"
    let Just f = FI.parseFeedString $ BLC.unpack k
    return f

feedListCtx :: FT.Feed -> Context String
feedListCtx f = listField "posts" feedPostCtx posts
  where posts = return $ map mkItem $ FQ.feedItems f

feedPostCtx :: Context FT.Item
feedPostCtx = feedField "title" FQ.getItemTitle `mappend`
              feedField "url" FQ.getItemLink `mappend`
              -- feedField "date" (fmap (formatTime defaultTimeLocale "%D" :: UTCTime -> String) . join . FQ.getItemPublishDate)
              field "date" (return . (formatTime defaultTimeLocale "%D" :: UTCTime -> String) . fromJust . join . FQ.getItemPublishDate . itemBody)

feedField name accessor = field name (return . fromJust . accessor . itemBody)

mkItem :: FT.Item -> Item FT.Item
mkItem i = Item (fromFilePath . fromJust $ FQ.getItemLink i) i

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
