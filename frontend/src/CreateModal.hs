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
module CreateModal(
    createModal
  , dayOfWeekBtns
  , timeSelect
  , enabledDays
  , timeOfDaySelect
  , amPmSelect
  , amOrPm
  , adjustTime
  , normalizeTo12Hour
  , timeOptions
  , AmPm
  , amPmMap
  ) where

import qualified Data.Map.Lazy as M
import qualified Data.Text as T

import Control.Lens
import Data.Coerce (coerce)
import Data.Maybe (isJust)
import Data.Time
import Reflex.Dom hiding (link)

import Autocomplete
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
createFields initial = elClass "div" "columns is-multiline" $ do
  let dynRestaurant = constDyn "Detroit"
  -- dynRestaurant <- elClass "div" "column is-half" $ bubbleInput (initial ^. restaurant) "Restaurant name"
  (e, _) <- elClass "div" "column is-full" $ elDynAttr' "input" (constDyn ("type" =: "text" <> "class" =: "input is-rounded is-primary")) blank
  eIdLlCity <- eAutocompleteBox (_element_raw e)
  dynLink <- elClass "div" "column is-full" $ bubbleInput (initial ^. link) "Link to description"
  dynId <- holdDyn (initial ^. placeId) (get1st <$> eIdLlCity) 
  dynLatLng <- holdDyn (initial ^. latLng) (get2nd <$> eIdLlCity) 
  dynCity <- holdDyn (initial ^. city) (get3rd <$> eIdLlCity) 
  _ <- elClass "div" "column is-half" $ staticTextBubble "City: use autocomplete" dynCity
  _ <- elClass "div" "column is-half" $ staticTextBubble "Id: use autocomplete" dynId
  dynSchedules <- elClass "div" "column is-full" $ scheduleInput (_schedule initial)
  return $ HappyHour <$> pure (_id initial) <*> dynCity <*> dynRestaurant <*> dynSchedules <*> dynLink <*> dynLatLng <*> dynId

get1st (x,_,_) = x
get2nd (_,x,_) = x
get3rd (_,_,x) = x

staticTextBubble :: MonadWidget t m => T.Text -> Dynamic t T.Text -> m ()
staticTextBubble initial placeholder = do
  let pMap = placeholderAttr initial placeholder
      dMap = pure $ "disabled" =: "true"
      cMap = pure $ "class" =: "input is-rounded"
  elDynAttr "input" (mconcat [pMap, dMap, cMap]) blank
  return ()

placeholderAttr :: Reflex t => T.Text -> Dynamic t T.Text -> Dynamic t (M.Map T.Text T.Text)
placeholderAttr fallback dynT = f <$> dynT
    where
  f t = if t == "" then embed fallback else embed t
  embed v = "placeholder" =: v

bubbleInput :: MonadWidget t m => T.Text -> T.Text -> m (Dynamic t T.Text)
bubbleInput initial placeholder = do
  ti <- textInput $ def 
    { _textInputConfig_attributes = constDyn ("class" =: "input is-rounded is-primary" <> "placeholder" =: placeholder)
    , _textInputConfig_initialValue = initial
    }
  return $ _textInput_value ti

scheduleInput :: MonadWidget t m => [Schedule] -> m (Dynamic t [Schedule])
scheduleInput initial =
  elClass "div" "tile is-ancestor is-full" $
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
singleScheduleCard num initialSchedule = elClass "div" "tile is-child message is-full" $ do
  clicked <- elClass "div" "message-header" $ do
    text $ "Schedule " <> (T.pack . show) (num + 1)
    (btn, _) <- elClass' "button" "delete" $ blank
    return $ domEvent Click btn
  dynSchedule <- elClass "div" "message-body" $ elClass "div" "columns is-multiline is-centered" $ do
    dynDays <- elClass "div" "column is-full" $ elClass "div" "buttons has-addons is-centered" $ dayOfWeekBtns (_days initialSchedule)
    timeRange <- do
      let (initStartTime, initEndTime) = coerce (_time initialSchedule)
      -- elClass "div" "column is-hidden-mobile" blank
      startTime <- elClass "div" "column" $ timeSelect initStartTime "hourglass-start"
      endTime <- elClass "div" "column has-text-centered" $ timeSelect initEndTime "hourglass-end"
      -- elClass "div" "column is-hidden-mobile" blank
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
  -> T.Text
  -> m (Dynamic t TimeOfDay)
timeSelect initial iconName = do
  let isCentered = if iconName == "clock" then "" else " has-addons-centered"
  elClass "div" ("field has-addons has-icons-left" <> isCentered) $ do
    -- elClass "div" "control" $ elClass "a" "button is-rounded is-primary is-outlined is-static" $ iconNoButton "hourglass-end"
    -- iconNoButton "hourglass-end"
    dynTimeOfDay <- timeOfDaySelect (normalizeTo12Hour initial) iconName
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
  -> T.Text
  -> m (Dynamic t TimeOfDay)
timeOfDaySelect initial iconName =
  elClass "div" "control has-icons-left" $ do
    elClass "span" "icon has-text-primary" $ elClass "i" ("fas fa-" <> iconName) blank
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
    showPair =
      -- pair TimeOfDay with its shown form
        (\timeOfDay -> (timeOfDay, printTimeOfDayNoMod timeOfDay))
      -- convert from UTCTime to TimeOfDay
      . (\utcTime -> localTimeOfDay (utcToLocalTime utc utcTime))
      -- convert to an offset UTCTime
      . (\seconds -> addUTCTime (fromInteger seconds) startUtc)
      -- convert to seconds
      . (\minutes -> minutes * 60)
  in
    M.fromList $ showPair <$> [0, 30 .. 30*23]

amPmMap :: M.Map AmPm T.Text
amPmMap = M.fromList [(AM, "am"), (PM, "pm")]