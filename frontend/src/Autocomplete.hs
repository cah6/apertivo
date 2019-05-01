{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Autocomplete where

import Control.Lens ((^.), makeLenses)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Concurrent.MVar (takeMVar, putMVar, newEmptyMVar)
import Data.String (fromString)
import Data.List (find)
import Data.Text hiding (find)
import Language.Javascript.JSaddle
import GHCJS.DOM.Element (Element)
import Reflex.Dom.Core hiding (Element)
import GHC.Generics

import Common.Dto
import FrontendCommon

eFilledGlobalVal :: MonadWidget t m
  => m (Event t ())
eFilledGlobalVal = do 
  (onChangeEvent, onChangeCallback) <- newTriggerEvent
  schedulePostBuild $ liftJSM $ fillGlobalVal $ liftIO . onChangeCallback
  return onChangeEvent

fillGlobalVal :: (() -> JSM ()) -> JSM ()
fillGlobalVal = \fillMe -> void $ do 
  setProp (toJSString "mapsLoaded") jsTrue global
  -- global ^. jss "mapsLoaded" $ undefined
  fillMe ()

eAutocompleteBox :: MonadWidget t m
  => Element
  -> m (Event t AutocompleteResults)
eAutocompleteBox parent = do 
  (onChangeEvent, onChangeCallback) <- newTriggerEvent
  schedulePostBuild $ liftJSM $ autocompleteBox parent $ liftIO . onChangeCallback
  return (flattenMaybe' onChangeEvent)

autocompleteBox :: Element -> (Maybe AutocompleteResults -> JSM ()) -> JSM ()
autocompleteBox parent = \applyResults -> void $ do
  autocompleteObj <- new (jsg "google" ^. js "maps" . js "places" . js "Autocomplete") (toJSVal parent)
  autocompleteObj ^. jsf "setTypes" [["establishment"]]
  autocompleteObj ^. jsf "addListener" ("place_changed", fun $ \_ _ _ -> void $ do       
        placeResult <- autocompleteObj ^. js0 "getPlace"

        jsPlaceId <- placeResult ^. js "place_id" 
        location <- placeResult ^. js "geometry" . js "location"
        jsLat <- location ^. js0 "lat"
        jsLng <- location ^. js0 "lng"
        jsName <- placeResult ^. js "name"

        jsAddressComponents <- placeResult ^. js "address_components"

        placeId <- fmap PlaceId <$> fromJSVal jsPlaceId
        lat <- fromJSVal jsLat
        lng <- fromJSVal jsLng
        name <- fmap PlaceName <$> fromJSVal jsName
        addressComponents <- fromJSVal jsAddressComponents

        let city = getCityFromComponents <$> addressComponents
        let latlng = LL <$> lat <*> lng

        applyResults $ AutocompleteResults <$> placeId <*> city <*> latlng <*> name
    )

getCityFromComponents :: [AddressComponent] -> City
getCityFromComponents xs = case find (\ac -> elem (pack "locality") (types ac)) xs of 
  Nothing -> City $ pack "City not found"
  Just ac -> City $ short_name ac

data AutocompleteResults = AutocompleteResults 
  { _acPlaceId :: PlaceId
  , _acCity :: City
  , _acLatLng :: LL
  , _acPlaceName :: PlaceName
  }

data AddressComponent = AddressComponent
  { long_name :: Text
  , short_name :: Text
  , types :: [Text]
  } deriving (Generic, Show, FromJSVal)

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

makeLenses ''AutocompleteResults