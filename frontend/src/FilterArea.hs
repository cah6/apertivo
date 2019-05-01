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
import Data.Maybe (mapMaybe, maybeToList)
import Data.Ord (comparing)
import Data.Time.LocalTime (TimeOfDay(..))
import Data.UUID.Types (UUID)
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
    maybeDayVal <- elClass "div" "column is-narrow" $ filterBubbleDay (_currentDay config)
    maybeTimeVal <- elClass "div" "column is-narrow" $ filterTimeSelect (_currentTime config)
    descriptionVal <- elClass "div" "column" $ filterBubbleInput "Description filter"
    cityVal <- elClass "div" "column is-narrow" $ filterBubbleCity (_eCurrentCity config) (_eAvailableCities config)
    eCreateClicked <- elClass "div" "column" $ createButton "Create New"
    let dSearchFilter = SearchFilter
          <$> cityVal 
          <*> restaurantVal 
          <*> descriptionVal 
          <*> fmap maybeToList maybeDayVal 
          <*> maybeTimeVal
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
  dynOptions <- dynMappend (pure <$> eCurrentCity) eDropdownValues
  -- let dynOptions = (constDyn (toSameMap ["San Francisco", "Detroit"]))
  eSetCity <- delay 0.1 eCurrentCity
  dd <- dropdown "" (toSameMap <$> dynOptions) $ def { _dropdownConfig_setValue = eSetCity }
  return (_dropdown_value dd)

-- Converts both events to dynamics, then mappends those dynamics together.
-- Needs to be in larger m monad as holdDyn requires this. 
dynMappend :: (MonadWidget t m, Monoid a)
  => Event t a
  -> Event t a
  -> m (Dynamic t a)
dynMappend e1 e2 = do 
  de1 <- holdDyn mempty e1
  de2 <- holdDyn mempty e2
  return $ de1 <> de2

reduceCityInput :: Map T.Text T.Text -> Map T.Text T.Text -> Map T.Text T.Text
reduceCityInput citiesInQuery initial = citiesInQuery <> initial

filterBubbleDay :: (MonadWidget t m) 
  => DayOfWeek
  -> m (Dynamic t (Maybe DayOfWeek))
filterBubbleDay initial = divAddons $ do 
  isActive <- divControl toggleButton
  divControl $ elDynAttr "div" (dropdownContainerRoundAttr <$> isActive) $ do
    dd <- dropdown initial (constDyn $ toShowMap [Sunday .. Saturday]) $ def { _dropdownConfig_attributes = dropdownAttrs <$> isActive}
    return $ zipDynWith toMaybe (_dropdown_value dd) isActive

filterTimeSelect :: MonadWidget t m 
  => TimeOfDay
  -> m (Dynamic t (Maybe TimeOfDay))
filterTimeSelect initial = divAddons $ do
  isActive <- divControl toggleButton
  dynTimeOfDay <- timeOfDaySelectNoIcon (normalizeTo12Hour initial) isActive
  dynAmPm <- toggleAmPmSelect (amOrPm initial) isActive
  return $ zipDynWith toMaybe (zipDynWith adjustTime dynAmPm dynTimeOfDay) isActive

timeOfDaySelectNoIcon :: MonadWidget t m 
  => TimeOfDay
  -> Dynamic t Bool
  -> m (Dynamic t TimeOfDay)
timeOfDaySelectNoIcon initial isActive = divControl $ elDynAttr "div" (dropdownContainerSquareAttr <$> isActive) $ do
  selected <- dropdown initial (constDyn timeOptions) $ def { _dropdownConfig_attributes = dropdownAttrs <$> isActive}
  return (_dropdown_value selected)

toggleAmPmSelect :: MonadWidget t m 
  => AmPm
  -> Dynamic t Bool
  -> m (Dynamic t AmPm)
toggleAmPmSelect initial isActive =
  divControl $
    elDynAttr "span" (dropdownContainerRoundAttr <$> isActive) $ do
      selected <- dropdown initial (constDyn amPmMap) $ def { _dropdownConfig_attributes = dropdownAttrs <$> isActive}
      return (_dropdown_value selected)

divAddons :: MonadWidget t m => m a -> m a
divAddons = elClass "div" "field has-addons"

divControl :: MonadWidget t m => m a -> m a
divControl = elClass "div" "control"

divControl' :: MonadWidget t m => m a -> m (El t, a)
divControl' = elClass' "div" "control"

dropdownContainerRoundAttr :: Bool -> Map T.Text T.Text
dropdownContainerRoundAttr isActive = if isActive 
  then
    "class" =: "select is-rounded is-primary"
  else
    "class" =: "select is-rounded"

dropdownContainerSquareAttr :: Bool -> Map T.Text T.Text
dropdownContainerSquareAttr isActive = if isActive 
  then
    "class" =: "select is-primary"
  else
    "class" =: "select"

dropdownAttrs :: Bool -> Map T.Text T.Text
dropdownAttrs isActive = if isActive 
  then
    "class" =: "select is-rounded is-primary"
  else
    "class" =: "select is-rounded has-background-white-ter is-outlined"

buttonAttrs :: Bool -> Map T.Text T.Text
buttonAttrs isActive = if isActive
  then
    "class" =: "button is-rounded is-outlined is-primary"
  else
    "class" =: "button is-rounded is-outlined has-background-white-ter"

toMaybe :: a -> Bool -> Maybe a
toMaybe a True = Just a
toMaybe _ False = Nothing 

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
  in  if (length matchingSchedules == 0) 
        then Nothing
        else Just $ a { _schedule = matchingSchedules }

timeMatches :: Maybe TimeOfDay -> HappyHour -> Maybe HappyHour
timeMatches Nothing a = Just a 
timeMatches (Just timeFilter) a = 
  let matchingSchedules :: [Schedule]
      matchingSchedules = filter (isTimeBetween timeFilter . _time) (_schedule a)
  in  if (length matchingSchedules == 0) 
        then Nothing
        else Just $ a { _schedule = matchingSchedules }

isTimeBetween :: TimeOfDay -> TimeRange -> Bool
isTimeBetween tod (TimeRange (start, end)) 
  | start < end = tod >= start && tod <= end
  | otherwise   = tod >= start || tod <= end

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

toggleButton :: MonadWidget t m => m (Dynamic t Bool)
toggleButton = mdo 
  (btn, _) <- elDynAttr' "a" (buttonAttrs <$> isActive) $ toggleIcon isActive
  isActive <- toggle True (domEvent Click btn)
  return isActive