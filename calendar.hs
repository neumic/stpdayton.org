{-# LANGUAGE OverloadedStrings #-}

import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Data.Time.Format
import Data.List (nub)
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Pretty
import qualified Data.ByteString.Lazy as B
import System.Locale
import System.Environment
import Control.Monad (when)


weeksOfMonth :: Integer -> Int -> [[Day]]
weeksOfMonth y m = weeks
  where
    days = map (fromGregorian y m) [1..final]
    final = gregorianMonthLength y m
    weekIds = nub $ map (fst . sundayStartWeek) days
    expandWeek w = map (fromSundayStartWeek y w) [0..6]
    weeks = map expandWeek weekIds


-- takes a month value to hide outside days
renderDay :: Int -> Day -> Markup
renderDay m d | month d /= m = td ! outmonth $ "-"
renderDay m d | otherwise    = td ! classValue $ content
  where classValue = if isWedFri d then fast
                                   else nofast
        content = do
            toHtml $ day d
            a ! href (readingLink d) $ "Readings"

-- need to figure out what all params are
-- year?
readingLink d = toValue $ concat
    [ "http://oca.org/Reading.asp?SID=25&id=&m="
    , show (month d)
    , "&D="
    , show (day d)
    ]

fast = class_ "fast"
nofast = class_ "nofast"
outmonth = class_ "outmonth"

month :: Day -> Int
month d = m
  where (_, m, _) = toGregorian d

day :: Day -> Int
day d = x
  where (_, _, x) = toGregorian d

renderWeek :: Int -> [Day] -> Markup
renderWeek m w = tr . toMarkup $ map (renderDay m) w


renderMonthTable :: Int -> [[Day]] -> Markup
renderMonthTable mId m = table $ do
    calHeader
    toMarkup $ map (renderWeek mId) m

renderMonth :: Integer -> Int -> Markup
renderMonth y m = do
    h3 $ toMarkup $ formatTime defaultTimeLocale "%B %Y" (fromGregorian y m 1)
    renderMonthTable m (weeksOfMonth y m)


isWedFri :: Day -> Bool
isWedFri d = x == 3 || x == 5
  where x = snd $ sundayStartWeek d


calHeader = tr $ mapM_ (th . toMarkup) weekdays

weekdays = map fst $ wDays defaultTimeLocale


-- USAGE: ./calendar <year> <month>
-- outputs at calendar.html
main = do
    args <- getArgs
    case args of
        [ys, ms] -> doIt ys ms
        _ -> showUsage

doIt ys ms = do
    let y = read ys
        m = read ms
        cal = renderMonth y m
    when (m < 1 || m > 12) (error "month not in range 1-12")
    writeFile "calendar.html" (renderHtml cal)

showUsage = putStrLn "Usage: ./calendar <year> <month>"
