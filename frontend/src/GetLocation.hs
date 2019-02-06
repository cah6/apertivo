{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
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

module GetLocation where 

import Control.Lens ((^.))
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Concurrent.MVar (takeMVar, putMVar, newEmptyMVar)
import Data.String (fromString)
import Data.Text
import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.Object
import Language.Javascript.JSaddle.Value
import Reflex.Dom.Core hiding (Element)
import GHC.Generics

import FrontendCommon

data Position = Position
  { coords :: Coordinates
  } deriving (Show, Generic, FromJSVal)

data Coordinates = Coordinates 
  { latitude :: Double
  , longitude :: Double
  } deriving (Show, Generic, FromJSVal)

eGetLocation :: MonadWidget t m
  => m (Event t Coordinates)
eGetLocation = do 
  (onChangeEvent, onChangeCallback) <- newTriggerEvent
  schedulePostBuild $ liftJSM $ getLocation $ liftIO . onChangeCallback
  return (flattenMaybe' onChangeEvent)

getLocation :: (Maybe Coordinates -> JSM ()) -> JSM ()
getLocation = \needsCoords -> do 
  nav <- jsg "navigator"
  geo <- nav ^. js "geolocation" . jsf "getCurrentPosition"
    [ fun $ \_ _ [pos] -> void $ do       
        coords <- pos ^. js "coords"
        toPrint :: Maybe Coordinates <- fromJSVal coords
        -- liftIO $ putStrLn (show toPrint)
        needsCoords toPrint
    ]
  return ()

