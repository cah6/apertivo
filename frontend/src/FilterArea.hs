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

import Data.List (intersect, nub, sortBy)
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

data FilterSectionConfig t = FilterSectionConfig
  { _currentDay :: DayOfWeek
  , _currentTime :: TimeOfDay
  , _eCurrentCity :: Event t T.Text
  , _eAvailableCities :: Event t [T.Text]
  }

filterSection ::  MonadWidget t m 
  => FilterSectionConfig t
  -> m (Dynamic t SearchFilter, Event t ())
filterSection config = do 
  elClass "div" "columns" $ do 
    restaurantVal <- elClass "div" "column is-narrow" $ filterBubbleInput "Restaurant"
    cityVal <- elClass "div" "column is-narrow" $ filterBubbleCity (_eCurrentCity config) (_eAvailableCities config)
    dayVal <- elClass "div" "column is-narrow" $ filterBubbleDay (_currentDay config)
    timeVal <- elClass "div" "column is-narrow" $ timeSelect (_currentTime config) "clock"
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
  => Event t T.Text
  -> Event t [T.Text]
  -> m (Dynamic t T.Text)
filterBubbleCity eCurrentCity eDropdownValues = elClass "div" "select is-primary is-rounded" $ do
  -- dynOptions <- holdDyn mempty $ toSameMap <$> eDropdownValues
  dynOptions <- nubMerge eCurrentCity eDropdownValues
  -- let dynOptions = (constDyn (toSameMap ["San Francisco", "Detroit"]))
  eSetCity <- delay 0.1 eCurrentCity
  dd <- dropdown "" (toSameMap <$> dynOptions) $ def { _dropdownConfig_setValue = eSetCity }
  return (_dropdown_value dd)

nubMerge :: MonadWidget t m
  => Event t T.Text
  -> Event t [T.Text]
  -> m (Dynamic t [T.Text])
nubMerge e1 e2 = do
  dynSingle <- holdDyn "" e1 
  dynList <- holdDyn [] e2
  return $ zipDynWith (:) dynSingle dynList

reduceCityInput :: Map T.Text T.Text -> Map T.Text T.Text -> Map T.Text T.Text
reduceCityInput citiesInQuery initial = citiesInQuery <> initial

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

toSameMap :: [T.Text] -> Map T.Text T.Text
toSameMap xs = fromList $ zip xs xs

createButton :: MonadWidget t m => T.Text -> m (Event t ())
createButton s = do
  (e, _) <- elClass' "button" "button is-primary is-pulled-right is-inverted" $ text s
  return $ domEvent Click e

----- Data and pure functions for computing filter results -----

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

-- Exclusive NOR between (start < end) and isBetween
isTimeBetween :: TimeOfDay -> TimeRange -> Bool
isTimeBetween tod (TimeRange (start, end)) 
  | start < end = isBetween
  | otherwise   = not isBetween
  where isBetween = tod >= start && tod <= end

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