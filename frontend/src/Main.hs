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
import qualified Network.Wreq as N

import Control.Lens
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Lens
import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.FileEmbed
import Data.List (nub, sortBy)
import Data.Maybe (listToMaybe)
import Data.Map (fromList, toList, Map)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Text.Encoding (decodeUtf8)
import Data.UUID (UUID)
import Generics.OneLiner
import Language.Javascript.JSaddle (liftJSM)
import Language.Javascript.JSaddle.Warp
import Reflex.Dom.Core

import Common.Dto
import CreateModal
import GeocodingReflexClient
import FilterArea
import FrontendCommon
import GetLocation
import ServantReflexClient

main :: IO ()
main = run 3003 $ mainWidgetWithHead frontendHead (prerender (text "Loading...") body)
-- import Autocomplete
-- main :: IO ()
-- main = run 3003 $ mainWidgetWithHead frontendHead (prerender (text "Loading...") locationMain)

frontendHead :: forall t m. MonadWidget t m => m ()
frontendHead = do
  -- liftJSM attachToWindow
  el "title" $ text "Apertivo"
  elAttr "meta" ("name" =: "viewport" <> "content" =: "initial-scale=1.0, width=device-width") blank
  -- todo: don't hardcode this!
  el "style" $ text $ decodeUtf8 $(embedFile "/Users/christian.henry/coding/haskell/apertivo/css/mystyles.css")
  -- elAttr "script" ("src" =: "https://maps.googleapis.com/maps/api/js?key=AIzaSyBZiVkgP8la1GHQw_ZJXNQl0N8dGCOW62c&libraries=places"
  --     <> "async" =: "true" <> "defer" =: "true") blank
  return ()

body :: forall t m. MonadWidget t m => m ()
body = do
  _ <- makeHero
  started <- getPostBuild
  eQueryResults <- queryHH started
  _ <- searchTab started (sortBy (comparing _restaurant) <$> eQueryResults)
  return ()

makeHero :: MonadWidget t m => m ()
makeHero = 
  elClass "section" "hero is-primary is-bold" $ 
    elClass "div" "hero-body" $ void $ do
      elClass "h1" "title" $ text "Apertivo"
      elClass "h2" "subtitle" $ text "The Happy Hour Finder"

searchTab :: MonadWidget t m 
  => Event t ()
  -> Event t [HappyHour] 
  -> m ()
searchTab ePostBuild eInitQueryResults = elClass "div" "box" $ mdo
  (dow, tod) <- liftIO getCurrentTimeAndDay
  coords <- eGetLocation
  city <- getCity coords
  let filterSectionConfig = FilterSectionConfig dow (roundTimeUp tod) city (toCities <$> eInitQueryResults)
  (dynSearchFilter, eCreateClicked) <- filterSection filterSectionConfig
  dynMaybeHH <- removingModal ((\_ -> defaultHH { _schedule = [defaultSchedule]}) <$> eCreateClicked) createModal
  let eCreateSubmitted = switchDyn $ flattenMaybe <$> dynMaybeHH
      eQueryResults = QueryResults <$> eInitQueryResults
  eNewUUID <- createHH eCreateSubmitted
  eCreatedWithUUID <- alignLatest (flip $ set id . Just) defaultHH eCreateSubmitted eNewUUID
  elClass "div" "box" $ elClass "div" "table-container" $ elClass "table" "table is-fullwidth" $ mdo
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
    eEditOrDelete <- mkTableBody (filterHappyHours <$> newlyUpdatedRows <*> dynSearchFilter)
    let eTableAction = leftmost [eQueryResults, SingleCreated <$> eCreatedWithUUID, eEditOrDelete]
    newlyUpdatedRows <- foldDyn reduceTableUpdate [defaultHH] eTableAction
    return ()
  return ()

toCities :: [HappyHour] -> [T.Text]
toCities as = nub $ _city <$> as

getCurrentCityFromIp :: IO T.Text
getCurrentCityFromIp = do
  r <- N.get "https://geoip-db.com/json"
  return $ r ^. N.responseBody . key "city" . _String

getCurrentCityFromIp' :: IO T.Text
getCurrentCityFromIp' = return "Portland"

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
    return (DeleteClicked <$> tagA dA eDelete, EditClicked <$> tagA dA eEdit)
  return c6

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

-- class    (s ~ [UnList s], s' ~ Int) => C s s'
-- instance (s ~ [UnList s], s' ~ Int) => C s s'
-- type family UnList a :: * where UnList [a] = a

-- mapPair :: ([a], [b]) -> (Int, Int)
-- mapPair = gmap @C length

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
      elClass "div" "modal is-active" $
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