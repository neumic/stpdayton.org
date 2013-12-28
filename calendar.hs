{-# LANGUAGE OverloadedStrings #-}

import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Data.List (nub)
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import qualified Data.ByteString.Lazy as B


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


renderMonth :: Int -> [[Day]] -> Markup
renderMonth mId m = table . toMarkup $ map (renderWeek mId) m


isWedFri :: Day -> Bool
isWedFri d = x == 3 || x == 5
  where x = snd $ sundayStartWeek d

testMonth = weeksOfMonth 2013 12
testMarkup = do
    Text.Blaze.Html5.head $ link ! rel "stylesheet" ! href "calendar.css"
    renderMonth 12 testMonth
testRendered = renderMarkup testMarkup

writeTest = B.writeFile "/tmp/foo.html" testRendered
