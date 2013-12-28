--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Data.Char (isAscii, isSpace)


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
            let indexCtx =
                    listField "pages" defaultContext (return pages) `mappend`
                    defaultContext

            pandocCompiler
                >>= stripNbspCompiler
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match pagesPattern $ version "menu" $ do
        route $ setExtension "html"
        compile getResourceBody

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
