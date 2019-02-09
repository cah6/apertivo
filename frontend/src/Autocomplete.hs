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
import Language.Javascript.JSaddle.Object
import Language.Javascript.JSaddle.Value
import GHCJS.DOM.Element (Element)
import Reflex.Dom.Core hiding (Element)

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
  -> m (Event t Text)
eAutocompleteBox parent = do 
  (onChangeEvent, onChangeCallback) <- newTriggerEvent
  schedulePostBuild $ liftJSM $ autocompleteBox parent $ liftIO . onChangeCallback
  return onChangeEvent

autocompleteBox :: Element -> (Text -> JSM ()) -> JSM ()
autocompleteBox parent = \needsText -> void $ do
  -- options <- new (jsg "google" ^. js "maps" . js "places" . js "AutocompleteOptions") ()
  -- options 
  autocompleteObj <- new (jsg "google" ^. js "maps" . js "places" . js "Autocomplete") (toJSVal parent)
  autocompleteObj ^. jsf "setTypes" [["establishment"]]
  bounds <- makeLatLngBounds
  autocompleteObj ^. jsf "setBounds" [bounds]
  autocompleteObj ^. jsf "addListener" ("place_changed", fun $ \_ _ _ -> void $ do       
        placeResult <- autocompleteObj ^. js0 "getPlace" . js "formatted_address"
        toPrint <- valToText placeResult
        liftIO $ putStrLn (show toPrint)
        needsText toPrint
    )

makeLatLngBounds :: JSM Object
makeLatLngBounds = do 
  bounds <- create
  bounds ^. jss "west" (-83.0647 :: Double)
  bounds ^. jss "south" (42.3500 :: Double)
  bounds ^. jss "north" (42.3564 :: Double)
  bounds ^. jss "east" (-83.056557 :: Double)
  return bounds

attachToWindow :: JSM ()
attachToWindow = void $ do
  window <- jsg "window"
  window ^. jss ("testme") (fun $ \_ _ _ -> testme)

testme :: JSM ()
testme = do 
  autocompleteBox <- printJSE $ new (jsg "Autocomplete") ()
  text <- valToText autocompleteBox
  liftIO $ putStrLn (show text)

js_autocomplete :: JSM JSVal
js_autocomplete = jsg "google.maps.places.Autocomplete"

printJSE fun = catch fun $ \(JSException e) -> 
    (valToText e) >>= (liftIO . putStrLn . show) >> return e

newtype AutocompleteBox = AutocompleteBox JSVal
  deriving (ToJSVal, MakeObject)