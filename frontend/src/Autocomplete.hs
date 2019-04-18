{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Autocomplete where

import Control.Lens ((^.))
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Concurrent.MVar (takeMVar, putMVar, newEmptyMVar)
import Data.String (fromString)
import Data.Text
import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.Classes
import Language.Javascript.JSaddle.Object
import Language.Javascript.JSaddle.Value
import GHCJS.DOM.Element (Element)
import Reflex.Dom.Core hiding (Element)

import Common.Dto
import FrontendCommon

-- autocompleteBox :: MonadWidget t m =>
--   m ()
-- autocompleteBox = do 
--   inputElement <- _textInput_element <$> textInput def
--   autocomplete <- liftJSM $ new (jsg "Autocomplete") [inputElement]
--   return ()

-- autocompleteBoxMain :: MonadWidget t m => m ()
-- autocompleteBoxMain = do 
--   eBtnClicked <- button (fromString "Attach")
--   (e, _) <- elAttr' (fromString "input") ((fromString "type") =: (fromString "text") <> (fromString "id") =: (fromString "myLabel")) $ blank 
--   performEvent $ liftJSM (autocompleteBox $ _element_raw e) <$ eBtnClicked
--   -- widgetHold blank (mkAutocompleteBox <$> eBtnClicked)
--   -- mkAutocompleteBox <$> eBtnClicked
--   return ()

-- autocompleteBox :: Element -> JSM ()
-- autocompleteBox parent = void $ do 
--     autocompleteBox <- printJSE $ new (jsg "google.maps.places.Autocomplete") ()
--     text <- valToText autocompleteBox
--     liftIO $ putStrLn (show text)

eAutocompleteBox :: MonadWidget t m
  => Element
  -> m (Event t (Text, LL, Text))
eAutocompleteBox parent = do 
  (onChangeEvent, onChangeCallback) <- newTriggerEvent
  schedulePostBuild $ liftJSM $ autocompleteBox parent $ liftIO . onChangeCallback
  return (flattenMaybe' onChangeEvent)

autocompleteBox :: Element -> (Maybe (Text, LL, Text) -> JSM ()) -> JSM ()
autocompleteBox parent = \applyResults -> void $ do
  -- options <- new (jsg "google" ^. js "maps" . js "places" . js "AutocompleteOptions") ()
  -- options 
  autocompleteObj <- new (jsg "google" ^. js "maps" . js "places" . js "Autocomplete") (toJSVal parent)
  autocompleteObj ^. jsf "setTypes" [["establishment"]]
  -- bounds <- makeLatLngBounds
  -- autocompleteObj ^. jsf "setBounds" [bounds]
  autocompleteObj ^. jsf "addListener" ("place_changed", fun $ \_ _ _ -> void $ do       
        placeResult <- autocompleteObj ^. js0 "getPlace"

        jsPlaceId <- placeResult ^. js "place_id" 
        location <- placeResult ^. js "geometry" . js "location"
        jsLat <- location ^. js0 "lat"
        jsLng <- location ^. js0 "lng"

        placeId <- fromJSVal jsPlaceId
        lat <- fromJSVal jsLat
        lng <- fromJSVal jsLng

        let latlng = LL <$> lat <*> lng
        -- TODO: get city from address_components: long_name when types contains "locality"
        applyResults $ (,,) <$> placeId <*> latlng <*> pure (fromString "Detroit")
    )

-- coerceTuple :: JSVal -> JSVal -> JSVal -> Maybe (a, b, c)
-- coerceTuple a b c = do 
--   a' <- fromJSVal a
--   b' <- fromJSVal b 
--   c' <- fromJSVal c
--   return (a', b', c')

makeLatLngBounds :: JSM Object
makeLatLngBounds = do 
  bounds <- create
  bounds ^. jss "west" (-83.0647 :: Double)
  bounds ^. jss "south" (42.3500 :: Double)
  bounds ^. jss "north" (42.3564 :: Double)
  bounds ^. jss "east" (-83.056557 :: Double)
  return bounds

testme :: JSM ()
testme = do 
  autocompleteBox <- printJSE $ new (jsg "Autocomplete") ()
  text <- valToText autocompleteBox
  liftIO $ putStrLn (show text)

printJSE fun = catch fun $ \(JSException e) -> 
    (valToText e) >>= (liftIO . putStrLn . show) >> return e
