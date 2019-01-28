{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveAnyClass, DeriveGeneric, OverloadedStrings, TemplateHaskell #-}
module Common.Dto where

import qualified Data.Attoparsec.Text as AP
import           Data.Attoparsec.Text (decimal, char)
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.Aeson.Types
import Control.Lens (makeLenses)
import Data.Monoid ((<>))
import Data.Text (Text, pack, breakOn, intercalate, unpack)
import Data.Time (formatTime)
import Data.Time.Calendar (Day(..))
import Data.Time.Calendar.OrdinalDate (sundayStartWeek)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime, LocalTime(..), TimeOfDay(..))
import Data.UUID (UUID)
import System.Directory (listDirectory)

import Common.Helper (attoToAeson)
import GHC.Generics (Generic)

newtype TimeRange = TimeRange (TimeOfDay, TimeOfDay)
  deriving (Generic)

instance ToJSON TimeRange where
  toJSON = String . printTimeRangeMilitary

instance Show TimeRange where
  show a = (unpack . printTimeRange) a

instance FromJSON TimeRange where
  parseJSON = attoToAeson parseTimeRange

printTimeRange :: TimeRange -> Text 
printTimeRange (TimeRange (start, end)) = printTimeOfDay start <> "-" <> printTimeOfDay end 

printTimeRangeMilitary :: TimeRange -> Text 
printTimeRangeMilitary (TimeRange (start, end)) = 
  printTimeOfDayMilitary start <> "-" <> printTimeOfDayMilitary end 

printTimeOfDayNoMod :: TimeOfDay -> Text
printTimeOfDayNoMod = pack . formatTime defaultTimeLocale "%-l:%M"

printTimeOfDay :: TimeOfDay -> Text
printTimeOfDay = pack . formatTime defaultTimeLocale "%-l:%M%P"

printTimeOfDayMilitary :: TimeOfDay -> Text
printTimeOfDayMilitary = pack . formatTime defaultTimeLocale "%H:%M"

loadHHs :: IO (Either String [HappyHour])
loadHHs = do
  let baseDir = "resources/data/"
  filenames <- listDirectory baseDir
  bytestrings <- traverse (B.readFile . (<>) baseDir) filenames
  return $ traverse eitherDecode bytestrings

parseTimeRange :: AP.Parser TimeRange
parseTimeRange = do
  startH <- decimal
  _ <- char ':'
  startM <- decimal
  _ <- char '-'
  endH <- decimal
  _ <- char ':'
  endM <- decimal
  return $ TimeRange (TimeOfDay startH startM 0, TimeOfDay endH endM 0)

defaultHH :: HappyHour
defaultHH = HappyHour 
  { _id = Nothing
  , _city = ""
  , _restaurant = ""
  , _schedule = []
  , _link = ""
  }

data HappyHour = HappyHour
  { _id :: Maybe UUID
  , _city :: Text
  , _restaurant :: Text
  , _schedule :: [Schedule]
  , _link :: Text
  } deriving (Generic, Show)

instance ToJSON HappyHour where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON HappyHour where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

data Schedule = Schedule
  { _days :: [DayOfWeek]
  , _time :: TimeRange
  , _scheduleDescription :: Text
  } deriving (Generic, Show)
  
defaultSchedule :: Schedule
defaultSchedule = Schedule
  { _days = []
  , _time = TimeRange (TimeOfDay 16 0 0, TimeOfDay 18 0 0)
  , _scheduleDescription = ""
  }

data DayOfWeek =
    Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday 
  deriving (Generic, Show, Read, Eq, Enum, Ord, ToJSON, FromJSON)

instance ToJSON Schedule where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

printDays :: [DayOfWeek] -> Text
printDays []  = ""
printDays [x]  = printDay x
printDays xs = printDay (head xs) <> "-" <> printDay (last xs)

printDay :: DayOfWeek -> Text 
printDay day = case day of
  Monday -> "Mon"
  Tuesday -> "Tue"
  Wednesday -> "Wed"
  Thursday -> "Thu"
  Friday -> "Fri"
  Saturday -> "Sat"
  Sunday -> "Sun"

getCurrentTimeAndDay :: IO (DayOfWeek, TimeOfDay)
getCurrentTimeAndDay = do
  currentTime <- getCurrentTime
  timezone <- getCurrentTimeZone
  let (LocalTime julianDay timeOfDay) = utcToLocalTime timezone currentTime
      (_weekNum, dayNum) = sundayStartWeek julianDay
  return (resolveDayNum dayNum, timeOfDay)

roundTimeUp :: TimeOfDay -> TimeOfDay
roundTimeUp (TimeOfDay h m s)
  | m < 30 = TimeOfDay h 30 0
  | h == 23 = TimeOfDay 0 0 0
  | otherwise = TimeOfDay (h + 1) 0 0

-- time 1.9.2 has more useful data types so we won't need to do this
resolveDayNum :: Int -> DayOfWeek
resolveDayNum int = case int of 
  0 -> Sunday
  1 -> Monday
  2 -> Tuesday
  3 -> Wednesday
  4 -> Thursday
  5 -> Friday
  6 -> Saturday

printDayShort :: DayOfWeek -> Text 
printDayShort day = case day of
  Monday -> "M"
  Tuesday -> "T"
  Wednesday -> "W"
  Thursday -> "T"
  Friday -> "F"
  Saturday -> "S"
  Sunday -> "S"

instance FromJSON Schedule where
  parseJSON = withObject "schedule" $ \o -> do
    _days                 <- map read <$> o .: "days"
    _time                 <- o .: "time" >>= parseJSON
    _scheduleDescription  <- o .: "scheduleDescription"
    return Schedule{..}

-- Make all the lenses
makeLenses ''HappyHour
makeLenses ''Schedule