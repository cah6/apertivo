{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CreateModal(createModal, dayOfWeekBtns, timeSelect, enabledDays) where

import qualified Data.Map.Lazy as M
import qualified Data.Text as T

import Data.Coerce (coerce)
import Data.Maybe (isJust)
import Data.Time
import Reflex.Dom

import Common.Dto
import FrontendCommon

createModal :: MonadWidget t m => HappyHour -> m (Event t HappyHour, Event t ())
createModal initial = do
  elClass "div" "modal-background" blank
  elClass "div" "modal-card" $ do
    eClose <- elClass "header" "modal-card-head" $ do
      let titleText = if (isJust $ _id initial) then "Edit" else "Create"
      elClass "p" "modal-card-title" $ text titleText
      b_delete
    dynHappyHour <- elClass "section" "modal-card-body" (createFields initial)
    (eSubmit, eCancel) <- elClass "footer" "modal-card-foot" $ do
      eSubmit <- b_button "Submit"
      eCancel <- b_button "Cancel"
      return (eSubmit, eCancel)
    return (tagPromptlyDyn dynHappyHour eSubmit, leftmost [eClose, eCancel, eSubmit])

createFields :: MonadWidget t m
  => HappyHour
  -> m (Dynamic t HappyHour)
createFields initial = elClass "div" "box" $ do
  dynRestaurant <- _textInput_value <$> horizontalInputWithInit (_restaurant initial) "Restaurant name:"
  dynCity <- _textInput_value <$> horizontalInputWithInit (_city initial) "City name:"
  dynLink <- _textInput_value <$> horizontalInputWithInit (_link initial) "Link to description:"
  dynSchedules <- scheduleInput (_schedule initial)
  return $ HappyHour <$> pure (_id initial) <*> dynCity <*> dynRestaurant <*> dynSchedules <*> dynLink
 
scheduleInput :: MonadWidget t m => [Schedule] -> m (Dynamic t [Schedule])
scheduleInput initial =
  elClass "div" "tile is-ancestor is-10" $
    elClass "div" "tile is-vertical is-parent" $ mdo
      let eAdd = AddAnother <$ domEvent Click btnAddAnother
          eScheduleChanged = leftmost $ getScheduleCardEvent dynMapCardResult : [eAdd]
      newBools <- foldDyn reduceScheduleCardEvent initialMap eScheduleChanged
      dynMapCardResult <- listWithKey2 newBools singleScheduleCard
      (btnAddAnother, _)  <- elClass' "button" "button" $ text "Add another"
      let dynSchedules = dynMapCardResult >>= getScheduleCardSchedules
      return dynSchedules
    where
  initialMap = M.fromList (zip [(0 :: Int) ..] initial)
  -- initialMaybeMap = Just <$> initialMap

listWithKey2 :: forall t k v m a. (Ord k, MonadWidget t m) => Dynamic t (M.Map k v) -> (k -> v -> m a) -> m (Dynamic t (M.Map k a))
listWithKey2 vals mkChild = do
  postBuild <- getPostBuild
  rec sentVals :: Dynamic t (M.Map k v) <- foldDyn applyMap M.empty changeVals
      let changeVals :: Event t (M.Map k (Maybe v))
          changeVals = attachWith diffMapNoEq (current sentVals) $ leftmost
                         [ updated vals
                         , tag (current vals) postBuild
                         ]
  listWithKeyShallowDiff M.empty changeVals $ \k v0 _ -> do
    mkChild k v0

singleScheduleCard :: MonadWidget t m => Int -> Schedule -> m (Event t ScheduleCardEvent, Dynamic t Schedule)
singleScheduleCard num initialSchedule = elClass "div" "tile is-child message" $ do
  clicked <- elClass "div" "message-header" $ do
    text $ "Schedule " <> (T.pack . show) (num + 1)
    (btn, _) <- elClass' "button" "delete" $ blank
    return $ domEvent Click btn
  dynSchedule <- elClass "div" "message-body" $ elClass "div" "columns is-multiline" $ do
    dynDays <- elClass "div" "column is-full" $ elClass "div" "buttons has-addons is-centered" $ dayOfWeekBtns (_days initialSchedule)
    timeRange <- elClass "div" "field has-addons" $ do
      let (initStartTime, initEndTime) = coerce (_time initialSchedule)
      startTime <- elClass "div" "column is-narrow" $ timeSelect initStartTime
      elClass "div" "column" $ text "to"
      endTime <- elClass "div" "column is-narrow" $ timeSelect initEndTime
      return $ TimeRange <$> zipDyn startTime endTime
    description <- elClass "div" "column is-full" $ textInput (descriptionOptions initialSchedule)
    return $ Schedule <$> dynDays <*> timeRange <*> _textInput_value description
  return $ (DeleteOne num <$ clicked, dynSchedule)


descriptionOptions :: Reflex t => Schedule -> TextInputConfig t
descriptionOptions initialSchedule = def
    { _textInputConfig_attributes = constDyn descriptionAttributes
    , _textInputConfig_initialValue = _scheduleDescription initialSchedule
    , _textInputConfig_inputType = "text"
    }

descriptionAttributes :: M.Map T.Text T.Text
descriptionAttributes = 
        "class" =: "input" 
    <>  "type" =: "text" 
    <>  "placeholder" =: "Short description of deals"

data ScheduleCardEvent = AddAnother | DeleteOne Int deriving Show

getScheduleCardEvent :: Reflex t
  => Dynamic t (M.Map k (Event t ScheduleCardEvent, a))
  -> Event t ScheduleCardEvent
getScheduleCardEvent input =
  let dynMapEvent = (fmap . fmap) fst input
      dynListEvent = fmap (fmap snd . M.toList) dynMapEvent
      dynEvent = leftmost <$> dynListEvent
  in  switchDyn dynEvent

getScheduleCardSchedules :: Reflex t => M.Map k (a, Dynamic t Schedule) -> Dynamic t [Schedule]
getScheduleCardSchedules input = sequence $ (snd . snd) <$> M.toList input

reduceScheduleCardEvent :: ScheduleCardEvent -> M.Map Int Schedule -> M.Map Int Schedule
reduceScheduleCardEvent e xs = case e of
  AddAnother ->
    let foldF key _ = max key
        maxKey = M.foldrWithKey foldF (negate (1 :: Int)) xs
        newKey = maxKey + 1
    in  xs <> (newKey =: defaultSchedule)
  DeleteOne i ->
    M.delete i xs

dayOfWeekBtns :: MonadWidget t m 
  => [DayOfWeek]
  -> m (Dynamic t [DayOfWeek])
dayOfWeekBtns initial = do
  dynDays <- mapM singleDayBtn (enabledDays initial)
  return (mapBtnState <$> sequence dynDays)

-- Get list of all days where inputs are True and rest are False
enabledDays :: [DayOfWeek] -> [(DayOfWeek, Bool)]
enabledDays xs = 
  let defaults = M.fromList $ zip [Sunday .. Saturday] (repeat False) 
      active = M.fromList $ zip xs (repeat True)
  in  M.toList $ M.union active defaults

singleDayBtn :: (MonadWidget t m)
  => (DayOfWeek, Bool)
  -> m (Dynamic t (DayOfWeek, Bool))
singleDayBtn (dow, initial) = mdo
  (btn, _) <- elDynAttr' "span" (singleDayBtnAttrs <$> isActive) $ text (printDay dow)
  isActive <- toggle initial (domEvent Click btn)
  return $ (\bool -> (dow, bool)) <$> isActive

mapBtnState :: [(DayOfWeek, Bool)] -> [DayOfWeek]
mapBtnState = map fst . filter (\tuple -> snd tuple == True)

singleDayBtnAttrs :: Bool -> M.Map T.Text T.Text
singleDayBtnAttrs isSelected = if isSelected
  then
    "class" =: "button is-rounded is-selected is-active"
  else
    "class" =: "button is-rounded"

-- Time selector functions

timeSelect :: MonadWidget t m 
  => TimeOfDay 
  -> m (Dynamic t TimeOfDay)
timeSelect initial =
  elClass "div" "field has-addons" $ do
    dynTimeOfDay <- timeOfDaySelect (normalizeTo12Hour initial)
    dynAmPm <- amPmSelect (amOrPm initial)
    return $ zipDynWith adjustTime dynAmPm dynTimeOfDay

adjustTime :: AmPm -> TimeOfDay -> TimeOfDay
adjustTime AM tod = tod
adjustTime PM (TimeOfDay h m s) = TimeOfDay (h + 12) m s

normalizeTo12Hour :: TimeOfDay -> TimeOfDay
normalizeTo12Hour (TimeOfDay h m s) = TimeOfDay (mod h 12) m s

amOrPm :: TimeOfDay -> AmPm
amOrPm (TimeOfDay h _ _) = if h < 12 then AM else PM

timeOfDaySelect :: MonadWidget t m 
  => TimeOfDay
  -> m (Dynamic t TimeOfDay)
timeOfDaySelect initial =
  elClass "div" "control" $
    elClass "span" "select is-rounded is-primary" $ do
      selected <- dropdown initial (constDyn timeOptions) def
      return (_dropdown_value selected)

amPmSelect :: MonadWidget t m 
  => AmPm
  -> m (Dynamic t AmPm)
amPmSelect initial =
  elClass "p" "control" $
    elClass "span" "select is-rounded is-primary" $ do
      selected <- dropdown initial (constDyn amPmMap) def
      return (_dropdown_value selected)

data AmPm = AM | PM
  deriving (Eq, Ord, Read)

timeOptions :: M.Map TimeOfDay T.Text
timeOptions =
  let
    startUtc = UTCTime (ModifiedJulianDay 0) 0
    transform =
      -- pair TimeOfDay with its shown form
        (\timeOfDay -> (timeOfDay, printTimeOfDayNoMod timeOfDay))
      -- convert from UTCTime to TimeOfDay
      . (\utcTime -> localTimeOfDay (utcToLocalTime utc utcTime))
      -- convert to an offset UTCTime
      . (\seconds -> addUTCTime (fromInteger seconds) startUtc)
      -- convert to seconds
      . (\minutes -> minutes * 60)
  in
    M.fromList $ transform <$> [0, 30 .. 30*23]

amPmMap :: M.Map AmPm T.Text
amPmMap = M.fromList [(AM, "am"), (PM, "pm")]