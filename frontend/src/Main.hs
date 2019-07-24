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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Prelude hiding (id)
import qualified Data.Text as T

import Control.Lens
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Lens
import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.FileEmbed
import Data.Functor
import Data.List (nub, sortBy, length)
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.Map (fromList, toList, Map)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.JSString (unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.UUID.Types (UUID)
import Language.Javascript.JSaddle (liftJSM, JSVal, JSM, fromJSVal, showJSValue, valToJSON, MonadJSM)
import Language.Javascript.JSaddle.Warp
import Reflex.Dom.Core
import qualified GoogleMapsReflex as G

import Autocomplete
import Common.Dto
import CreateModal
import GeocodingReflexClient
import FilterArea
import FrontendCommon
import GetLocation
import ServantReflexClient

main :: IO ()
main = run 3003 $ mainWidgetWithHead frontendHead (prerender (text "Loading...") body)
-- 
-- main :: IO ()
-- main = run 3003 $ mainWidgetWithHead frontendHead (prerender (text "Loading...") locationMain)

frontendHead :: forall t m. MonadWidget t m => m ()
frontendHead = do
  -- liftJSM attachToWindow
  el "title" $ text "Apertivo"
  elAttr "meta" ("name" =: "viewport" <> "content" =: "initial-scale=1.0, width=device-width") blank
  -- todo: don't hardcode this! somehow?
  el "style" $ text $ decodeUtf8 $(embedFile "/Users/christian.henry/coding/haskell/apertivo/css/mystyles.css")
  -- el "style" $ text $ decodeUtf8 $(embedFile "/home/cah6/coding/haskell/apertivo/css/mystyles.css")
  elAttr "script" ("src" =: "https://maps.googleapis.com/maps/api/js?key=AIzaSyDZh_dyyl7PJCe-haE_hGOOP7NJCnqdy4k&libraries=places"
      <> "async" =: "true" <> "defer" =: "true") blank
  eFilledGlobalVal
  return ()

body :: forall t m. MonadWidget t m => m ()
body = mdo
  _ <- makeHero
  started <- getPostBuild
  eQueryResults <- queryHH started
  --elClass "footer" "footer" $ elAttr "div" ("class" =: "content" <> "flex-direction" =: "column") $ text "Hello"
  let eInitQueryResults = (sortBy (comparing _restaurant) <$> eQueryResults) 
  (dynSearchFilter, eCreateClicked) <- filterHeaders eInitQueryResults
  exampleMapsWidget activeRows
  activeRows <- searchTab eInitQueryResults dynSearchFilter eCreateClicked
  return ()

makeHero :: MonadWidget t m => m ()
makeHero = 
  elClass "section" "hero is-primary is-bold" $ 
    elClass "div" "hero-body" $ void $ do
      elClass "h1" "title" $ text "Apertivo"
      elClass "h2" "subtitle" $ text "The Happy Hour Finder"

searchTab :: MonadWidget t m 
  => Event t [HappyHour] 
  -> Dynamic t SearchFilter
  -> Event t ()
  -> m (Dynamic t [HappyHour])
searchTab eInitQueryResults dynSearchFilter eCreateClicked = elAttr "footer" ("class" =: "footer" <> "style" =: "position: absolute; bottom: 0; width: 100%; height: 20%; background-color: transparent;") $ mdo
  dynMaybeHH <- removingModal ((\_ -> defaultHH { _schedule = [defaultSchedule]}) <$> eCreateClicked) createModal
  let eCreateSubmitted = switchDyn $ flattenMaybe <$> dynMaybeHH
      eQueryResults = QueryResults <$> eInitQueryResults
  eNewUUID <- createHH eCreateSubmitted
  eCreatedWithUUID <- alignLatest (flip $ set id . Just) defaultHH eCreateSubmitted eNewUUID
  activeRows <- elClass "div" "box" $ elClass "div" "table-container" $ elClass "table" "table is-fullwidth" $ mdo
    el "thead" $
      el "tr" $ do 
        elAttr "th" ("scope" =: "col") $ text "Restaurant"
        elAttr "th" ("scope" =: "col" <> "class" =: "is-hidden-mobile") $ text "City"
        elAttr "th" ("scope" =: "col" <> "class" =: "is-hidden-mobile") $ text "Days"
        elAttr "th" ("scope" =: "col") $ text "Time"
        elAttr "th" ("scope" =: "col") $ text "Description"
        elAttr "th" ("scope" =: "col" <> "class" =: "is-hidden-mobile") $ text "Action"
        return ()
        -- mapM_ (elAttr "th" ("scope" =: "col" ) . text) cols
    activeRows <- return $ filterHappyHours <$> newlyUpdatedRows <*> dynSearchFilter
    eEditOrDelete <- mkTableBody newlyUpdatedRows activeRows
    let eTableAction = leftmost [eQueryResults, SingleCreated <$> eCreatedWithUUID, eEditOrDelete]
    newlyUpdatedRows <- foldDyn reduceTableUpdate [defaultHH] eTableAction
    return activeRows
  return activeRows

filterHeaders :: MonadWidget t m
  => Event t [HappyHour]
  -> m (Dynamic t SearchFilter, Event t ())
filterHeaders eInitQueryResults = elAttr "div" ("class" =: "box" <> "style" =: "position: absolute; z-index: 90; width: 100%;") $ do 
  (dow, tod) <- liftIO getCurrentTimeAndDay
  coords <- traceEventWith printLocation <$> eGetLocation
  city <- getCity coords
  let filterSectionConfig = FilterSectionConfig dow (roundTimeUp tod) city (toCities <$> eInitQueryResults)
  filterSection filterSectionConfig

exampleMapsWidget :: MonadWidget t m 
  => Dynamic t [HappyHour]
  -> m ()
exampleMapsWidget dxs = do
  let configDyn = hhsToConfig <$> dxs

  (Element _ mapEl, _) <- elAttr' "div" ("style" =: "width: 100%; height: 87%; position: absolute;") blank

  maps <- G.googleMaps mapEl (G.ApiKey "AIzaSyDZh_dyyl7PJCe-haE_hGOOP7NJCnqdy4k") configDyn

  eJSVal :: Event t JSVal <- G.mapEvent G.Click maps 
  eJson <- performEvent (valToString <$> eJSVal)
  d <- holdDyn "" (const "Clicked" <$> traceEventWith (\z -> "Clicked with result: " <> show z) eJson)
  el "div" $ dynText d
  return ()

valToString :: MonadJSM m
  => JSVal
  -> m String
valToString eVal = do 
  json <- liftJSM $ valToJSON eVal
  return $ unpack json

hhsToConfig :: [HappyHour] -> G.Config T.Text
hhsToConfig xs = def 
  { G._config_markers = fromList $ map (\hh -> (hh ^. placeId, hhToMarker hh)) $ availablePlaces
  , G._config_infoWindows = fromList $ map (\hh -> (hh ^. placeId, hhToInfoWindowState hh)) $ availablePlaces
  , G._config_mapOptions = def 
    { G._mapOptions_center = G.LatLng 42.363368799999996 (negate 83.0643899)
    , G._mapOptions_zoom = 13
    , G._mapOptions_disableDefaultUI = True
    , G._mapOptions_scrollwheel = False
    }
  }
    where
  availablePlaces = filter (\hh -> hh ^. placeId /= "") xs

hhToMarker :: HappyHour -> G.MarkerOptions
hhToMarker hh = def
  { G._markerOptions_position = G.LatLng (hh ^. latLng . Common.Dto.latitude) (hh ^. latLng . Common.Dto.longitude)
  , G._markerOptions_title = "" --hh ^. restaurant
  , G._markerOptions_animation = Just G.Drop
  , G._markerOptions_cursor = Nothing
  , G._markerOptions_clickable = True
  }

hhToInfoWindowState :: HappyHour -> G.InfoWindowState
hhToInfoWindowState hh = G.InfoWindowState 
  { G._infoWindowState_options = G.InfoWindowOptions 
    { G._infoWindowOptions_content = G.ContentText $ hh ^. restaurant
    , G._infoWindowOptions_disableAutoPan = True
    , G._infoWindowOptions_maxWidth = 200
    , G._infoWindowOptions_pixelOffset = G.Size 
      { _size_width = 0
      , _size_height = -40
      , _size_widthUnit = Nothing
      , _size_heightUnit = Nothing
      }
    , G._infoWindowOptions_position = G.LatLng (hh ^. latLng . Common.Dto.latitude) (hh ^. latLng . Common.Dto.longitude)
    , G._infoWindowOptions_zIndex = 0
    }
  , G._infoWindowState_open = True
  }

printLocation :: Coordinates -> String
printLocation latLng = "Found user at location: " <> show latLng

toCities :: [HappyHour] -> [T.Text]
toCities as = nub $ _city <$> as

-- "Zips" two events in time. 
-- I.e. returns an event that fires when both events have come in. 
alignLatest :: MonadWidget t m
  => (a -> b -> c) 
  -> a 
  -> Event t a 
  -> Event t b 
  -> m (Event t c)
alignLatest f defA eA eB = do 
  dynLeft <- holdDyn defA eA
  return $ attachPromptlyDynWith f dynLeft eB

mkTableBody :: MonadWidget t m 
  => Dynamic t [HappyHour]
  -> Dynamic t [HappyHour]
  -> m (Event t TableUpdate)
mkTableBody allRows displayRows = do 
  let rows = simpleList displayRows makeTableSection
  (eDelete, eEdit) <- flattenDynList <$> el "tbody" rows
  _ <- deleteHH (coerce <$> eDelete)
  dynMaybeHH <- removingModal (openModalEvent allRows eEdit) createModal
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
      extract as uuid = listToMaybe $ filter ((==) (Just uuid) . view id) as
  in  attachPromptlyDynWithMaybe extract dA (coerce <$> eEdit)

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
      mkRestRow hh = "rowspan" =: (T.pack $ show $ length $ _schedule hh) 
        <> "style" =: "vertical-align:middle"
      mkActionRow hh = "rowspan" =: (T.pack $ show $ length $ _schedule hh)
        <> "style" =: "vertical-align:middle;white-space:nowrap"
        <> "class" =: "is-hidden-mobile"
      mkCityRow hh = "rowspan" =: (T.pack $ show $ length $ _schedule hh)
        <> "style" =: "vertical-align:middle" 
        <> "class" =: "is-hidden-mobile"
  _c1 <- elDynAttr "td" (mkRestRow <$> dA) $ elDynAttr "a" (mkLinkAttrs <$> dA) (dynText (_restaurant <$> dA))
  _c2 <- elDynAttr "td" (mkCityRow <$> dA) $ dynText $ _city <$> dA
  _common <- createCommonRow dS
  c6 <- elDynAttr "td" (mkActionRow <$> dA) $ do
    eEdit <- icon "edit"
    eDelete <- icon "trash-alt"
    dynShouldDelete <- removingModal (tag (current dA) eDelete) confirmDeleteModal
    let eShouldDelete = switchDyn $ flattenMaybe <$> dynShouldDelete
    -- gate (flattenBool <$> current dynShouldDelete) (() <$ updated dynShouldDelete)
    return (DeleteClicked <$> tagA dA eShouldDelete, EditClicked <$> tagA dA eEdit)
  return c6

flattenBool :: Maybe Bool -> Bool
flattenBool Nothing = False
flattenBool (Just b) = b

confirmDeleteModal :: MonadWidget t m
  => HappyHour
  -> m (Event t (), Event t ())
confirmDeleteModal hh = do
  elClass "div" "modal-background" blank
  elClass "div" "modal-card" $ do
    _ <- elClass "section" "modal-card-body" $ text $ "Are you sure you want to delete ALL schedules for " <> hh ^. restaurant <> "?"
    (eSubmit, eCancel) <- elClass "footer" "modal-card-foot" $ do
      eSubmit <- b_button "Yes"
      eCancel <- b_button "No"
      return (eSubmit, eCancel)
    return (eSubmit, leftmost [eCancel, eSubmit])

-- These rows don't need as many columns, since those are already "filled" by
-- the rowspan in head row.
createTailRow :: MonadWidget t m 
  => Dynamic t Schedule
  -> m (RowAction t)
createTailRow dS = el "tr" $ do
  _common <- createCommonRow dS
  return (never, never)

createCommonRow :: MonadWidget t m
  => Dynamic t Schedule
  -> m ()
createCommonRow dS = void $ do
  _c3 <- compactDayOfWeekBtns $ _days <$> dS
  let timeText = dynText $ (printTimeRange . _time) <$> dS
  -- what will show up on computer
  _c4 <- elAttr "td" ("style" =: "vertical-align:middle;white-space:nowrap" <> "class" =: "is-hidden-mobile") timeText
  -- what will show up on mobile
  _c4' <- elAttr "td" ("style" =: "vertical-align:middle" <> "class" =: "is-hidden-tablet") timeText
  elAttr "td" ("style" =: "vertical-align:middle") $ dynText $ _scheduleDescription <$> dS

compactDayOfWeekBtns :: MonadWidget t m 
  => Dynamic t [DayOfWeek]
  -> m ()
compactDayOfWeekBtns initial = elAttr "td" ("style" =: "min-width:260px;vertical-align:middle" <> "class" =: "is-hidden-mobile") $ void $
  elClass "div" "buttons has-addons is-pulled-left" $ simpleList (enabledDays <$> initial) staticSingleDayButton 

staticSingleDayButton :: (MonadWidget t m)
  => Dynamic t (DayOfWeek, Bool)
  -> m ()
staticSingleDayButton dynTuple = void $
  elDynAttr' "span" (staticBtnAttr . snd <$> dynTuple) $ dynText (printDayShort . fst <$> dynTuple)

staticBtnAttr :: Bool -> Map T.Text T.Text
staticBtnAttr True =
  "class" =: "button is-small is-rounded is-static is-selected is-active is-primary"
staticBtnAttr False =
  "class" =:  "button is-small is-rounded is-static"

-- Given a dynamic list of event tuples, 
flattenDynList :: Reflex t => Dynamic t [(Event t a, Event t b)] -> (Event t a, Event t b)
flattenDynList dxs = 
  let dLeft = leftmost <$> (fmap . fmap) fst dxs
      dRight = leftmost <$> (fmap . fmap) snd dxs
  in  (switchDyn dLeft, switchDyn dRight)

tagA :: Reflex t => Dynamic t HappyHour -> Event t () -> Event t UUID
tagA dA e = fmapMaybe _id $ tag (current dA) e

row :: MonadWidget t m
  => [m ()]
  -> m a
  -> m a
row xs lastCol = el "tr" $ do
  mapM_ (el "td") xs
  el "td" lastCol

-- "Removing" modal taken from reflex-dom-contribs
removingModal :: MonadWidget t m
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
      elAttr "div" ("class" =: "modal is-active" <> "style" =: "z-index: 90") $
        first Just <$> modalBody a

-- mkGoogleMapsFrame :: MonadWidget t m => m ()
-- mkGoogleMapsFrame = elAttr "iframe" (
--         "width" =: "600"
--     <> "height" =: "450"
--     <> "frameborder" =: "0"
--     <> "style" =: "border:0"
--     <> "src" =: "https://www.google.com/maps/embed/v1/place?key=AIzaSyDxM3_sjDAP1kDHzbRMkZ6Ky7BYouXfVOs&q=place_id:ChIJMSuIlbnSJIgRbUFj__-VGdA&q=ChIJMUfEOWEtO4gRddDKWmgPMpI"
--     ) blank

switchMobile :: MonadWidget t m
  => T.Text -- element type
  -> Map T.Text T.Text -- attr map
  -> m a -- mobile format
  -> m a -- desktop format
  -> m ()
switchMobile elType attrs mobileWidget desktopWidget = void $ do 
  _ <- elAttr elType (setMobileClass attrs) mobileWidget
  elAttr elType (setDesktopClass attrs) desktopWidget

-- needs to merge with previous class
setMobileClass :: Map T.Text T.Text -> Map T.Text T.Text
setMobileClass = mappend $ "class" =: "is-hidden-tablet"

setDesktopClass :: Map T.Text T.Text -> Map T.Text T.Text
setDesktopClass = mappend $ "class" =: "is-hidden-mobile"
-- ix "class" %~ (\old -> old ++ " is-hidden-tablet")