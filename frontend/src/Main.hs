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

-- import Control.Monad (join, void)
import Control.Monad.Trans (liftIO)
import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.List (intersect, sortBy)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.Time.LocalTime (TimeOfDay(..))
import Data.UUID (toText, UUID)
import Language.Javascript.JSaddle.Warp
import Reflex.Dom.Core

import Common.Dto
import CreateModal
import FrontendCommon
import ServantReflexClient

main :: IO ()
main = run 3003 $ mainWidgetWithHead frontendHead (prerender (text "Loading...") body)

frontendHead :: forall t m. MonadWidget t m => m ()
frontendHead = do
  el "title" $ text "Happy Hours"
  elAttr "link" ("href" =: "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.1/css/bulma.min.css"
              <> "rel" =: "stylesheet"
              <> "type" =: "text/css"
              ) blank
  elAttr "link" ("href" =: "https://use.fontawesome.com/releases/v5.5.0/css/all.css"
              <> "rel" =: "stylesheet"
              <> "integrity" =: "sha384-B4dIYHKNBt8Bc12p+WXckhzcICo0wtJAoU8YZTY5qE0Id1GSseTk6S+L3BlXeVIU"
              <> "crossorigin" =: "anonymous"
              ) blank
  return ()

body :: forall t m. MonadWidget t m => m ()
body = mdo
  started <- getPostBuild
  eQueryResult <- queryHH started
  dHHs <- holdDyn [defaultHH] eQueryResult
  eHappyHourCreated <- searchTab (sortBy sortByRestaurant <$> dHHs)
  _ <- createHH eHappyHourCreated
  return ()

searchTab :: MonadWidget t m => Dynamic t [HappyHour] -> m (Event t HappyHour)
searchTab xs = elClass "div" "box" $ do
  eCreate <- b_button "Create New"
  -- filterVal <- _textInput_value <$> horizontalInput "Search Filters:"
  dynSearchFilter <- filterSection
  dynMaybeHH <- removingModal ((\_ -> defaultHH) <$> eCreate) createModal
  elClass "table" "table is-bordered" $ do
    el "thead" $
      el "tr" $
        mapM_ (elAttr "th" ("scope" =: "col") . text) cols
    _ <- mkTableBody (filterHappyHours <$> xs <*> dynSearchFilter)
    return ()
  elAttr "iframe" (
        "width" =: "600"
    <> "height" =: "450"
    <> "frameborder" =: "0"
    <> "style" =: "border:0"
    <> "src" =: "https://www.google.com/maps/embed/v1/place?key=AIzaSyDxM3_sjDAP1kDHzbRMkZ6Ky7BYouXfVOs&q=place_id:ChIJMSuIlbnSJIgRbUFj__-VGdA&q=ChIJMUfEOWEtO4gRddDKWmgPMpI"
    ) blank
  return (switchDyn $ flattenMaybe <$> dynMaybeHH)

filterSection ::  MonadWidget t m => m (Dynamic t SearchFilter)
filterSection = do 
  elClass "label" "label is-large" $ text "Filter Results"
  cityVal <- _textInput_value <$> horizontalInputWithInit "" "City"
  restaurantVal <- _textInput_value <$> horizontalInputWithInit "" "Restaurant Name"
  descriptionVal <- _textInput_value <$> horizontalInputWithInit "" "Schedule Description"
  days <- do 
    elClass "label" "label" $ text "On Day"
    elClass "div" "buttons has-addons" $ dayOfWeekBtns []
  -- timeSelect
  return $ SearchFilter <$> cityVal <*> restaurantVal <*> descriptionVal <*> days <*> pure Nothing

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
restaurantMatches restaurantFilter a = boolToMaybe (T.isInfixOf restaurantFilter (_restaurant a)) a

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
      scheduleMatch = T.isInfixOf scheduleFilter . _scheduleDescription
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

mkTableBody :: MonadWidget t m => Dynamic t [HappyHour] -> m ()
mkTableBody xs = do 
  let rows = simpleList xs mkRow
  (eDelete, eEdit) <- flattenDynList <$> el "tbody" rows
  _ <- deleteHH (coerce <$> eDelete)
  dynMaybeHH <- removingModal (openModalEvent xs eEdit) createModal
  _ <- updateHH (switchDyn $ flattenMaybe <$> dynMaybeHH)
  return ()

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

-- handleRowAction :: MonadWidget t m => Dynamic t [HappyHour] -> RowAction t -> m (Event t ())
-- handleRowAction dxs (eDelete, eEdit) = do
  
--   removingModal eEdit createModal
--   return eDeleted

-- -- fanEither eRowAction
--   let
--     go :: MonadWidget t m => RowAction -> m (Event t ())
--     go (DeleteClicked duuid, EditClicked euuid) = do 
--       eDeleted <- deleteHH $ fmap (\ra -> duuid) eRowAction
--       return eDeleted
--   in
--     return (go <$> eRowAction)

-- mkBuiltInTable :: MonadWidget t m => Dynamic t [HappyHour] -> m ()
-- mkBuiltInTable dynHHs =
--   let
--     dRowsList = zip [(1 :: Integer)..] <$> dynHHs
--     dRows = M.fromList <$> dRowsList
--     dCols = M.fromList $ zip cols mkRow
--   in 
--     tableDynAttr "td" dCols dRows undefined >> return ()

cols :: [T.Text]
cols = ["Restaurant", "City", "Time", "Description", "Action"]

-- data RowAction =
--     DeleteClicked UUID
--   | EditClicked UUID

type RowAction t = (Event t DeleteClicked, Event t EditClicked)

newtype DeleteClicked = DeleteClicked UUID
newtype EditClicked = EditClicked UUID

mkRow :: MonadWidget t m
      => Dynamic t HappyHour
      -> m (RowAction t)
mkRow dA = flattenDynList <$> simpleList (_schedule <$> dA) (\schedule ->
  let
    mkLinkAttrs hh = ("href" =: _link hh)
    c1 = elDynAttr "a" (mkLinkAttrs <$> dA) (dynText (_restaurant <$> dA))
    c2 = dynText $ _city <$> dA
    c3 = dynText $ times <$> schedule
    c4 = dynText $ _scheduleDescription <$> schedule
    c5 = do
      eEdit <- icon "edit"
      eDelete <- icon "trash-alt"
      return (DeleteClicked <$> tagA dA eDelete, EditClicked <$> tagA dA eEdit)
  in
    row [c1, c2, c3, c4] c5)

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