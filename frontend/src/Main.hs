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

module Main where

import qualified Data.Text as T
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.List (intersect, sortBy)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Map (fromList, toList, Map)
import Data.Monoid ((<>))
import Data.Time.LocalTime (TimeOfDay(..))
import Data.UUID (toText, UUID)
import Language.Javascript.JSaddle.Warp
import Reflex.Dom.Core

import Autocomplete
import Common.Dto
import CreateModal
import FrontendCommon
import ServantReflexClient

main :: IO ()
main = run 3003 $ mainWidgetWithHead frontendHead (prerender (text "Loading...") body)

-- main :: IO ()
-- main = run 3003 $ mainWidgetWithHead frontendHead (prerender (text "Loading...") autocompleteBoxMain)

frontendHead :: forall t m. MonadWidget t m => m ()
frontendHead = do
  el "title" $ text "Apertivo"
  elAttr "link" ("href" =: "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.1/css/bulma.min.css"
              <> "rel" =: "stylesheet"
              <> "type" =: "text/css"
              ) blank
  elAttr "link" ("href" =: "https://use.fontawesome.com/releases/v5.5.0/css/all.css"
              <> "rel" =: "stylesheet"
              <> "integrity" =: "sha384-B4dIYHKNBt8Bc12p+WXckhzcICo0wtJAoU8YZTY5qE0Id1GSseTk6S+L3BlXeVIU"
              <> "crossorigin" =: "anonymous"
              ) blank
  elAttr "script" ("src" =: "https://maps.googleapis.com/maps/api/js?key=AIzaSyBZiVkgP8la1GHQw_ZJXNQl0N8dGCOW62c&libraries=places"
              <> "type" =: "text/javascript"
              ) blank
  return ()

body :: forall t m. MonadWidget t m => m ()
body = do
  _ <- makeHero
  started <- getPostBuild
  eQueryResults <- queryHH started
  _ <- searchTab (sortBy sortByRestaurant <$> eQueryResults)
  return ()

makeHero :: MonadWidget t m => m ()
makeHero = 
  elClass "section" "hero is-primary is-bold" $ 
    elClass "div" "hero-body" $ void $ do
      elClass "h1" "title" $ text "Apertivo"
      elClass "h2" "subtitle" $ text "The Happy Hour Finder"

searchTab :: MonadWidget t m => Event t [HappyHour] -> m ()
searchTab eInitQueryResults = elClass "div" "box" $ mdo
  (dynSearchFilter, eCreateClicked) <- filterSection
  dynMaybeHH <- removingModal ((\_ -> defaultHH) <$> eCreateClicked) createModal
  let eCreateSubmitted = switchDyn $ flattenMaybe <$> dynMaybeHH
      eQueryResults = QueryResults <$> eInitQueryResults
  eNewUUID <- createHH eCreateSubmitted
  -- let eCreatedWithUUID = (flattenMaybe . sequence) $ attachPromptlyDynWith zipServerResponse dynMaybeHH eNewUUID
  eCreatedWithUUID <- traceEvent "latest created" <$> alignLatest zipServerResponse (defaultHH {_schedule = []}) eCreateSubmitted eNewUUID
  elClass "table" "table is-fullwidth" $ mdo
    el "thead" $
      el "tr" $
        mapM_ (elAttr "th" ("scope" =: "col" ) . text) cols
    eEditOrDelete <- mkTableBody (filterHappyHours <$> newlyUpdatedRows <*> dynSearchFilter)
    let eTableAction = leftmost [eQueryResults, SingleCreated <$> eCreatedWithUUID, eEditOrDelete]
    newlyUpdatedRows <- foldDyn reduceTableUpdate ([defaultHH {_schedule = []}]) eTableAction
    return ()
  return ()

-- "Zips" in time: [the event for create response from modal, the response from server with UUID]
zipServerResponse :: HappyHour -> UUID -> HappyHour
zipServerResponse hh newUUID = hh { _id = Just newUUID }

alignLatest :: MonadWidget t m
  => (a -> b -> c) 
  -> a 
  -> Event t a 
  -> Event t b 
  -> m (Event t c)
alignLatest f defA eA eB = do 
  dynLeft <- holdDyn defA eA
  return $ attachPromptlyDynWith f dynLeft eB

reduceTableUpdate :: TableUpdate -> [HappyHour] -> [HappyHour]
reduceTableUpdate update xs = case update of 
  QueryResults newXs -> 
    newXs
  SingleCreated x -> 
    sortBy sortByRestaurant (x : xs)
  SingleEdited newVal ->
    let (beforeEdit, withAndAfterEdit) = break (\hh -> _id hh == _id newVal) xs
    in  beforeEdit ++ (newVal : tail withAndAfterEdit)
  SingleDeleted deletedUUID -> 
    let (beforeDelete, withAndAfterDelete) = break (\hh -> _id hh == Just deletedUUID) xs
    in  beforeDelete ++ (tail withAndAfterDelete)

-- mkGoogleMapsFrame :: MonadWidget t m => m ()
-- mkGoogleMapsFrame = elAttr "iframe" (
--         "width" =: "600"
--     <> "height" =: "450"
--     <> "frameborder" =: "0"
--     <> "style" =: "border:0"
--     <> "src" =: "https://www.google.com/maps/embed/v1/place?key=AIzaSyDxM3_sjDAP1kDHzbRMkZ6Ky7BYouXfVOs&q=place_id:ChIJMSuIlbnSJIgRbUFj__-VGdA&q=ChIJMUfEOWEtO4gRddDKWmgPMpI"
--     ) blank

data TableUpdate = 
    QueryResults [HappyHour]
  | SingleDeleted UUID
  | SingleEdited HappyHour
  | SingleCreated HappyHour

filterSection ::  MonadWidget t m => m (Dynamic t SearchFilter, Event t ())
filterSection = do 
  elClass "div" "columns" $ do 
    restaurantVal <- elClass "div" "column is-narrow" $ filterBubbleInput "Restaurant"
    cityVal <- elClass "div" "column is-narrow" $ filterBubbleCity "Detroit" ["San Francisco", "Detroit"]
    days <- elClass "div" "column is-narrow" $ filterBubbleDay Monday
      -- elClass "div" "buttons has-addons" $ dayOfWeekBtns []
    descriptionVal <- elClass "div" "column" $ filterBubbleInput "Description filter"
    eCreateClicked <- elClass "div" "column" $ createButton "Create New"
    return $ (SearchFilter <$> cityVal <*> restaurantVal <*> descriptionVal <*> fmap (\d -> [d]) days <*> pure Nothing, eCreateClicked)

createButton :: MonadWidget t m => T.Text -> m (Event t ())
createButton s = do
  (e, _) <- elClass' "button" "button is-primary is-pulled-right is-inverted" $ text s
  return $ domEvent Click e

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

-- filterBubbleDay :: (MonadWidget t m) 
--   => DayOfWeek
--   -> m (Dynamic t DayOfWeek)
-- filterBubbleDay initial = elClass "div" "select is-rounded is-primary" $ el "select" $ do 
--   mapM (\day -> el "option" $ text "day") [Sunday .. Saturday]
--   return $ constDyn Monday

filterBubbleDay :: (MonadWidget t m) 
  => DayOfWeek
  -> m (Dynamic t DayOfWeek)
filterBubbleDay initial = elClass "div" "select is-rounded is-primary" $ do
  dd <- dropdown initial (constDyn $ toShowMap [Sunday .. Saturday]) def
  return (_dropdown_value dd)

toShowMap :: (Ord a, Show a) => [a] -> Map a T.Text
toShowMap xs = fromList $ zip xs (T.pack . show <$> xs)

data SearchFilter = SearchFilter 
  { _sCity :: T.Text
  , _sRestaurant :: T.Text
  , _sScheduleDescription :: T.Text
  , _sDay :: [DayOfWeek]
  , _sTime :: Maybe TimeOfDay
  }

flattenMaybe :: Reflex t => Maybe (Event t a) -> Event t a
flattenMaybe Nothing  = never
flattenMaybe (Just a) = a

sortByRestaurant :: HappyHour -> HappyHour -> Ordering
sortByRestaurant a1 a2 = compare (_restaurant a1) (_restaurant a2)

filterHappyHours :: [HappyHour] -> SearchFilter -> [HappyHour]
filterHappyHours xs searchFilter =
  let
    filterSingle :: HappyHour -> Maybe HappyHour
    filterSingle x = 
          cityMatches x (_sCity searchFilter)
      >>= restaurantMatches (_sRestaurant searchFilter)
      >>= dayMatches (_sDay searchFilter)
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
      dayMatch days = null daysFilter || (not . null . intersect daysFilter) days
      matchingSchedules :: [Schedule]
      matchingSchedules = filter (dayMatch . _days) (_schedule a)
  in  if (length matchingSchedules == length (_schedule a)) 
        then Just a 
        else Just $ a { _schedule = matchingSchedules }

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

boolToMaybe :: Bool -> a -> Maybe a
boolToMaybe True a = Just a
boolToMaybe False _ = Nothing

listToMaybeDef :: [a] -> b -> Maybe b
listToMaybeDef xs b = boolToMaybe (null xs) b

mkTableBody :: MonadWidget t m => Dynamic t [HappyHour] -> m (Event t TableUpdate)
mkTableBody xs = do 
  let rows = simpleList xs makeTableSection
  (eDelete, eEdit) <- flattenDynList <$> el "tbody" rows
  _ <- deleteHH (coerce <$> eDelete)
  dynMaybeHH <- removingModal (openModalEvent xs eEdit) createModal
  let eDeleteHappened = fmap (\(DeleteClicked uuid) -> SingleDeleted uuid) eDelete
      eEditForServant = switchDyn $ flattenMaybe <$> dynMaybeHH
      eEditHappened = SingleEdited <$> eEditForServant
  _ <- updateHH eEditForServant
  return $ leftmost [eDeleteHappened, eEditHappened]

openModalEvent :: (Reflex t) 
  => Dynamic t [HappyHour] 
  -> Event t EditClicked
  -> Event t HappyHour
openModalEvent dA eEdit = 
  let extract :: [HappyHour] -> UUID -> Maybe HappyHour
      extract as uuid = listToMaybe $ filter (isId uuid) as
  in  attachPromptlyDynWithMaybe extract dA (coerce <$> eEdit)

isId :: UUID -> HappyHour -> Bool
isId uuid a = case (_id a) of 
  Nothing -> False
  Just b -> uuid == b

cols :: [T.Text]
cols = ["Restaurant", "City", "Time", "Description", "Action"]

type RowAction t = (Event t DeleteClicked, Event t EditClicked)

newtype DeleteClicked = DeleteClicked UUID
newtype EditClicked = EditClicked UUID

makeTableSection :: MonadWidget t m
      => Dynamic t HappyHour
      -> m (RowAction t)
makeTableSection dA = do
  let dS = _schedule <$> dA
      dScheduleWithKey = fmap (fromList . zip [(0 :: Int)..]) dS
  dEvents <- listWithKey dScheduleWithKey (createRows dA)
  return (coerceMap dEvents)

coerceMap :: Reflex t 
  => Dynamic t (Map k (Event t a, Event t b))
  -> (Event t a, Event t b)
coerceMap dMap = flattenDynList $ fmap (map snd . toList) dMap

createRows :: MonadWidget t m 
  => Dynamic t HappyHour
  -> Int
  -> Dynamic t Schedule
  -> m (RowAction t)
createRows dHH 0 dS = createHeadRow dHH dS
createRows _   _ dS = createTailRow dS

createHeadRow :: MonadWidget t m 
  => Dynamic t HappyHour
  -> Dynamic t Schedule
  -> m (RowAction t)
createHeadRow dA dS = el "tr" $ do
  let mkLinkAttrs hh = ("href" =: _link hh)
      mkRowspan hh = ("rowspan" =: (T.pack $ show $ length $ _schedule hh)) <> ("style" =: "vertical-align:middle")
  _c1 <- elDynAttr "td" (mkRowspan <$> dA) $ elDynAttr "a" (mkLinkAttrs <$> dA) (dynText (_restaurant <$> dA))
  _c2 <- elDynAttr "td" (mkRowspan <$> dA) $ dynText $ _city <$> dA
  _c3 <- el "td" $ dynText $ times <$> dS
  _c4 <- el "td" $ dynText $ _scheduleDescription <$> dS
  c5 <- elDynAttr "td" (mkRowspan <$> dA) $ do
    eEdit <- icon "edit"
    eDelete <- icon "trash-alt"
    return (DeleteClicked <$> tagA dA eDelete, EditClicked <$> tagA dA eEdit)
  return c5

-- These rows don't need as many columns, since those are already "filled" by
-- the rowspan in head row.
createTailRow :: MonadWidget t m 
  => Dynamic t Schedule
  -> m (RowAction t)
createTailRow dS = el "tr" $ do
  _c3 <- el "td" $ dynText $ times <$> dS
  _c4 <- el "td" $ dynText $ _scheduleDescription <$> dS
  return (never, never)

flattenDynList :: Reflex t => Dynamic t [(Event t a, Event t b)] -> (Event t a, Event t b)
flattenDynList dxs = 
  let dLeft = leftmost <$> (fmap . fmap) fst dxs
      dRight = leftmost <$> (fmap . fmap) snd dxs
  in  (switchDyn dLeft, switchDyn dRight)

tagA :: Reflex t => Dynamic t HappyHour -> Event t () -> Event t UUID
tagA dA e = fmapMaybe _id $ tag (current dA) e

uuidText :: HappyHour -> T.Text
uuidText a = case _id a of
  Nothing -> "Nothing"
  Just x -> toText x

times :: Schedule -> T.Text
times Schedule{ _days, _time } =
  let
    days = printDays _days
    time = printTimeRange _time
  in
    days <> ", " <> time

flattenHH :: HappyHour -> [HappyHour]
flattenHH HappyHour{_schedule, ..} = map (\s -> HappyHour {_schedule = [s], .. } ) _schedule

row :: MonadWidget t m
  => [m ()]
  -> m a
  -> m a
row xs lastCol = el "tr" $ do
  mapM_ (el "td") xs
  el "td" lastCol

-- "Removing" modal taken from reflex-dom-contribs
removingModal
  :: MonadWidget t m
  => Event t a
  -- ^ Event to open the model
  -> (a -> m (b, Event t ()))
  -- ^ Widget rendering the body of the modal.  Returns an event with a
  -- success value and an event triggering the close of the modal.
  -> m (Dynamic t (Maybe b))
removingModal showm modalBody = do
    rec let visE = leftmost [Just <$> showm, Nothing <$ closem]
        res <- widgetHold (removeFromDOMWrapper Nothing) (removeFromDOMWrapper <$> visE)
        let resE = fst <$> res
            closem = switchDyn $ snd <$> res
    return resE
  where
    removeFromDOMWrapper Nothing = return (Nothing, never)
    removeFromDOMWrapper (Just a) =
      elClass "div" "modal is-active" $
        first Just <$> modalBody a

-- Utils

putDebugLnE :: MonadWidget t m => Event t a -> (a -> String) -> m ()
putDebugLnE e mkStr = performEvent_ (liftIO . putStrLn . mkStr <$> e)

icon :: MonadWidget t m => T.Text -> m (Event t ())
icon name = do
  (e, _) <- elClass' "span" "icon" $ elClass "i" ("fas fa-" <> name) blank
  return $ domEvent Click e