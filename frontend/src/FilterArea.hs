{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FilterArea where

import Prelude hiding (id)
import qualified Data.Text as T

import Data.List (intersect, sortBy)
import Data.Map (fromList, Map)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Time.LocalTime (TimeOfDay(..))
import Data.UUID (UUID)
import Reflex.Dom.Core

import Common.Dto
import CreateModal
import FrontendCommon

----- DOM elements for filter area -----

filterSection ::  MonadWidget t m => m (Dynamic t SearchFilter, Event t ())
filterSection = do 
  elClass "div" "columns" $ do 
    restaurantVal <- elClass "div" "column is-narrow" $ filterBubbleInput "Restaurant"
    cityVal <- elClass "div" "column is-narrow" $ filterBubbleCity "Detroit" ["San Francisco", "Detroit"]
    dayVal <- elClass "div" "column is-narrow" $ filterBubbleDay Monday
    timeVal <- elClass "div" "column is-narrow" $ timeSelect (TimeOfDay 17 0 0)
    descriptionVal <- elClass "div" "column" $ filterBubbleInput "Description filter"
    eCreateClicked <- elClass "div" "column" $ createButton "Create New"
    let dSearchFilter = SearchFilter
          <$> cityVal 
          <*> restaurantVal 
          <*> descriptionVal 
          <*> fmap pure dayVal 
          <*> (Just <$> timeVal)
    return $ (dSearchFilter, eCreateClicked)

filterBubbleInput :: MonadWidget t m => T.Text -> m (Dynamic t T.Text)
filterBubbleInput initial = do
  ti <- textInput $ def 
    { _textInputConfig_attributes = constDyn ("class" =: "input is-rounded is-primary" <> "placeholder" =: initial)
    }
  return $ _textInput_value ti

filterBubbleCity :: (MonadWidget t m) 
  => T.Text
  -> [T.Text]
  -> m (Dynamic t T.Text)
filterBubbleCity initial options = elClass "div" "select is-primary is-rounded" $ do
  dd <- dropdown initial (constDyn $ fromList (zip options options)) def
  return (_dropdown_value dd)

filterBubbleDay :: (MonadWidget t m) 
  => DayOfWeek
  -> m (Dynamic t DayOfWeek)
filterBubbleDay initial = elClass "div" "field has-addons" $ do 
  elClass "div" "control" $ elClass "a" "button is-rounded is-primary is-outlined" $ iconNoButton "check"
  elClass "div" "control" $ elClass "div" "select is-rounded is-primary" $ do
    dd <- dropdown initial (constDyn $ toShowMap [Sunday .. Saturday]) def
    return (_dropdown_value dd)

toShowMap :: (Ord a, Show a) => [a] -> Map a T.Text
toShowMap xs = fromList $ zip xs (T.pack . show <$> xs)

createButton :: MonadWidget t m => T.Text -> m (Event t ())
createButton s = do
  (e, _) <- elClass' "button" "button is-primary is-pulled-right is-inverted" $ text s
  return $ domEvent Click e

----- Data and pure functions for filter area -----

data SearchFilter = SearchFilter 
  { _sCity :: T.Text
  , _sRestaurant :: T.Text
  , _sScheduleDescription :: T.Text
  , _sDay :: [DayOfWeek]
  , _sTime :: Maybe TimeOfDay
  }

data TableUpdate = 
    QueryResults [HappyHour]
  | SingleDeleted UUID
  | SingleEdited HappyHour
  | SingleCreated HappyHour

reduceTableUpdate :: TableUpdate -> [HappyHour] -> [HappyHour]
reduceTableUpdate update xs = case update of 
  QueryResults newXs -> 
    newXs
  SingleCreated x -> 
    sortBy (comparing _restaurant) (x : xs)
  SingleEdited newVal ->
    let (beforeEdit, withAndAfterEdit) = break (\hh -> _id hh == _id newVal) xs
    in  beforeEdit ++ (newVal : tail withAndAfterEdit)
  SingleDeleted deletedUUID -> 
    let (beforeDelete, withAndAfterDelete) = break (\hh -> _id hh == Just deletedUUID) xs
    in  beforeDelete ++ (tail withAndAfterDelete)

filterHappyHours :: [HappyHour] -> SearchFilter -> [HappyHour]
filterHappyHours xs searchFilter =
  let
    filterSingle :: HappyHour -> Maybe HappyHour
    filterSingle x = 
          cityMatches x (_sCity searchFilter)
      >>= restaurantMatches (_sRestaurant searchFilter)
      >>= dayMatches (_sDay searchFilter)
      >>= timeMatches (_sTime searchFilter)      
      >>= schedulesThatContain (_sScheduleDescription searchFilter)
  in
    mapMaybe filterSingle xs

cityMatches :: HappyHour -> T.Text -> Maybe HappyHour
cityMatches a cityFilter = boolToMaybe (T.isInfixOf cityFilter (_city a)) a

restaurantMatches :: T.Text -> HappyHour -> Maybe HappyHour
restaurantMatches restaurantFilter a = 
  let filterR = T.toLower restaurantFilter
      targetR = T.toLower (_restaurant a)
  in  boolToMaybe (T.isInfixOf filterR targetR) a

dayMatches :: [DayOfWeek] -> HappyHour -> Maybe HappyHour
dayMatches daysFilter a = 
  let dayMatch :: [DayOfWeek] -> Bool
      dayMatch ds = null daysFilter || (not . null . intersect daysFilter) ds
      matchingSchedules :: [Schedule]
      matchingSchedules = filter (dayMatch . _days) (_schedule a)
  in  if (length matchingSchedules == length (_schedule a)) 
        then Just a 
        else Just $ a { _schedule = matchingSchedules }

timeMatches :: Maybe TimeOfDay -> HappyHour -> Maybe HappyHour
timeMatches Nothing a = Just a 
timeMatches (Just timeFilter) a = 
  let matchingSchedules :: [Schedule]
      matchingSchedules = filter (isTimeBetween timeFilter . _time) (_schedule a)
  in  if (length matchingSchedules == length (_schedule a)) 
        then Just a 
        else Just $ a { _schedule = matchingSchedules }

isTimeBetween :: TimeOfDay -> TimeRange -> Bool
isTimeBetween tod (TimeRange (start, end)) = tod >= start && tod <= end

anyScheduleContains :: HappyHour -> T.Text -> Bool
anyScheduleContains x scheduleFilter = any (T.isInfixOf scheduleFilter . _scheduleDescription) (_schedule x)

schedulesThatContain :: T.Text -> HappyHour -> Maybe HappyHour
schedulesThatContain scheduleFilter a =
  let scheduleMatch :: Schedule -> Bool
      scheduleMatch = T.isInfixOf (T.toLower scheduleFilter) . T.toLower . _scheduleDescription
      matchingSchedules :: [Schedule]
      matchingSchedules = filter scheduleMatch (_schedule a)
  in  if (length matchingSchedules == length (_schedule a)) 
        then Just a 
        else Just $ a { _schedule = matchingSchedules } 