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
  (e, _) <- elAttr' (fromString "input") ((fromString "type") =: (fromString "text")) $ blank 
  performEvent $ liftJSM (autocompleteBox $ _element_raw e) <$ eBtnClicked
  -- widgetHold blank (mkAutocompleteBox <$> eBtnClicked)
  -- mkAutocompleteBox <$> eBtnClicked
  return ()

autocompleteBox :: Element -> JSM ()
autocompleteBox parent = do 
    void $ new (jsg "google.maps.places.Autocomplete") (toJSVal parent)
    return ()

js_autocomplete :: JSM JSVal
js_autocomplete = jsg "Autocomplete"

newtype AutocompleteBox = AutocompleteBox JSVal
  deriving (ToJSVal, MakeObject)