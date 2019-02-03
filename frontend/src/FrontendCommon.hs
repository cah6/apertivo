{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FrontendCommon where

import qualified Data.Text as T

import Reflex.Dom 

b_button :: MonadWidget t m => T.Text -> m (Event t ())
b_button s = do
  (e, _) <- elClass' "button" "button" $ text s
  return $ domEvent Click e

b_delete :: MonadWidget t m => m (Event t ())
b_delete = do 
  (e, _) <- elClass' "button" "delete" $ blank
  return $ domEvent Click e

horizontalInput :: MonadWidget t m => T.Text -> m (TextInput t)
horizontalInput label = elAttr "div" ("class" =: "field") $ do
  elClass "label" "label" $ text label
  textInput $ def { _textInputConfig_attributes = constDyn ("class" =: "control" )}

horizontalInputWithInit :: MonadWidget t m => T.Text -> T.Text -> m (TextInput t)
horizontalInputWithInit initial label = elAttr "div" ("class" =: "field") $ do
  elClass "label" "label" $ text label
  textInput $ def 
    { _textInputConfig_attributes = constDyn ("class" =: "control" )
    , _textInputConfig_initialValue = initial
    }
  
flattenMaybe :: Reflex t => Maybe (Event t a) -> Event t a
flattenMaybe Nothing  = never
flattenMaybe (Just a) = a

boolToMaybe :: Bool -> a -> Maybe a
boolToMaybe True a = Just a
boolToMaybe False _ = Nothing

-- putDebugLnE :: MonadWidget t m => Event t a -> (a -> String) -> m ()
-- putDebugLnE e mkStr = performEvent_ (liftIO . putStrLn . mkStr <$> e)

icon :: MonadWidget t m => T.Text -> m (Event t ())
icon name = do
  (e, _) <- elClass "a" "button is-white is-small" $ elClass' "span" "icon" $ elClass "i" ("fas fa-" <> name) blank
  return $ domEvent Click e

iconNoButton :: MonadWidget t m => T.Text -> m ()
iconNoButton name = elClass "span" "icon" $ elClass "i" ("fas fa-" <> name) blank

toggleIcon :: MonadWidget t m => Dynamic t Bool -> m ()
toggleIcon isActive = elClass "span" "icon" $ elDynClass "i" (toggleIconClass <$> isActive) blank

toggleIconClass :: Bool -> T.Text
toggleIconClass isActive = if isActive 
  then
    "fas fa-toggle-on is-primary"
  else
    "fas fa-toggle-off"