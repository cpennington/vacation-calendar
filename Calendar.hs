{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (head, id, div, span, writeFile)
import Text.Blaze.Html4.Strict hiding (map)
import Text.Blaze.Html4.Strict.Attributes hiding (title, span)
import Text.Blaze.Renderer.Text
import Data.Time.Calendar
import Data.List hiding (head, span)
import qualified Data.List as L
import Data.Function hiding (id)
import Data.Time.Calendar.WeekDate
import Data.List.Split
import Data.Time.Format
import System.Locale
import Data.Text.Lazy.IO

year = (\(y, _, _) -> y).toGregorian
month = (\(_, m, _) -> m).toGregorian
day = (\(_, _, d) -> d).toGregorian
dayOfWeek = (flip mod 7).(\(_, _, d) -> d).toWeekDate

daysOfYear y = takeWhile ((y ==).year) $ iterate (addDays 1) $ fromGregorian y 1 1
byMonth = groupBy ((==) `on` month)
padMonth m = (replicate (dayOfWeek firstDay) Nothing) ++ (map Just m) ++ (replicate (6 - dayOfWeek lastDay) Nothing)
    where firstDay = L.head m
          lastDay = last m
splitWeeks = splitEvery 7

calendarTitle = (printf "%B - %Y") . (L.head)
filePath = (printf "%Y-%m-%B.html") . (L.head)

fullDate = printf "%b %e, %Y"

printf = formatTime defaultTimeLocale

dayHtml dataF (Just d) = td ! class_ "day" $ do
    div ! class_ "header" $ string $ printf "%e" d
    div ! class_ "data" $ dataF d
dayHtml _     Nothing  = td ! class_ "day" $ ""

weekHtml dataF w = tr $ mapM_ (dayHtml dataF) w

monthHtml :: (Day -> Html) -> [Day] -> Html
monthHtml dataF m = html $ do
    head $ do
        title $ string $ calendarTitle m
        link ! rel "stylesheet" ! type_ "text/css" ! href "calendar.css"
    body $ do
        div ! class_ "calendar" $ do
            h1 ! class_ "title" $ string $ calendarTitle m
            table $ do
                tr $ do
                    mapM_ (th.string) ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"]
                mapM_ (weekHtml dataF) $ splitWeeks $ padMonth m

formatMonth dataF = renderHtml.(monthHtml dataF)
writeMonth dataF m = writeFile (filePath m) (formatMonth dataF m)
writeYear dataF y = mapM_ (writeMonth dataF) $ byMonth $ daysOfYear y

vacation = (span.string.fullDate.(addDays 45))
