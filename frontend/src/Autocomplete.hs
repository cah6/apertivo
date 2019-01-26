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
import GHCJS.DOM.Element
import Reflex.Dom.Core hiding (Element)

-- autocompleteBox :: MonadWidget t m =>
--   m ()
-- autocompleteBox = do 
--   inputElement <- _textInput_element <$> textInput def
--   autocomplete <- liftJSM $ new (jsg "Autocomplete") [inputElement]
--   return ()

autocompleteBoxMain :: MonadWidget t m => m ()
autocompleteBoxMain = do 
  eBtnClicked <- button (fromString "Attach")
  (e, _) <- elAttr' (fromString "input") ((fromString "type") =: (fromString "text") <> (fromString "id") =: (fromString "myLabel")) $ blank 
  performEvent $ liftJSM (autocompleteBox $ _element_raw e) <$ eBtnClicked
  -- widgetHold blank (mkAutocompleteBox <$> eBtnClicked)
  -- mkAutocompleteBox <$> eBtnClicked
  return ()

-- autocompleteBox :: Element -> JSM ()
-- autocompleteBox parent = void $ do 
--     autocompleteBox <- printJSE $ new (jsg "google.maps.places.Autocomplete") ()
--     text <- valToText autocompleteBox
--     liftIO $ putStrLn (show text)

autocompleteBox :: Element -> JSM ()
autocompleteBox parent = void $ do 
    -- dateVal <- eval "new google.maps.places.Autocomplete(document.getElementById('myLabel'))"
    doc <- jsg "document"
    myLabel <- doc ^. (js1 "getElementById") "myLabel"
    dateVal <- printJSE $ new (jsg "google" ^. js "maps" . js "places" . js "Autocomplete") (myLabel)
    text <- valToText dateVal
    liftIO $ putStrLn (show text)

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