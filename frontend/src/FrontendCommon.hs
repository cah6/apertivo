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
  