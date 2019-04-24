{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module GeocodingReflexClient(
    getCity
  ) where 

import Data.Maybe (listToMaybe, mapMaybe)
import Data.UUID.Types
import Reflex.Dom
import Data.Text hiding (any)
import Servant.API
import Servant.Reflex
import Data.Proxy
import Web.Google.Geocoding

import FrontendCommon
import GetLocation

geocoderClients :: forall t m. (MonadWidget t m) => _
geocoderClients = client api (Proxy @m) (Proxy @()) (constDyn url)
  where url :: BaseUrl
        url = BaseFullUrl Https "maps.googleapis.com/maps/api" 443 "/"
        -- url = BaseFullUrl Http "localhost" 3000 "/"

geoKey :: Text 
geoKey = "AIzaSyDxM3_sjDAP1kDHzbRMkZ6Ky7BYouXfVOs"

-- Reflex bindings to google API, for docs see
-- http://hackage.haskell.org/package/google-maps-geocoding-0.5.0.0/docs/Web-Google-Geocoding.html
genGeocode :: MonadWidget t m
  => Dynamic t (QParam Web.Google.Geocoding.Key)
  -> Dynamic t (QParam Address)
  -> Dynamic t (QParam [FilterComponent])
  -> Dynamic t (QParam Viewport)
  -> Dynamic t (QParam Language)
  -> Dynamic t (QParam Region)
  -> Event t ()
  -> m (Event t (ReqResult () GeocodingResponse))
genBackGeocode :: MonadWidget t m
  => Dynamic t (QParam Web.Google.Geocoding.Key)
  -> Dynamic t (QParam LatLng)
  -> Dynamic t (QParam PlaceId)
  -> Dynamic t (QParam AddressType)
  -> Dynamic t (QParam LocationType)
  -> Dynamic t (QParam Language)
  -> Event t ()
  -> m (Event t (ReqResult () GeocodingResponse))
genGeocode :<|> genBackGeocode = geocoderClients

getCity :: MonadWidget t m
  => Event t Coordinates
  -> m (Event t Text)
getCity eCoords = do 
  let dynGeoKey = QParamSome . Key <$> constDyn geoKey
      eLatLng = QParamSome . toGeoLatLng <$> eCoords
      eTrigger = () <$ eLatLng
  dynLatLng <- holdDyn QNone eLatLng
  result <- genBackGeocode dynGeoKey dynLatLng dynQNone dynQNone dynQNone dynQNone eTrigger
  return $ flattenMaybe' (reqResultToCity <$> result)

reqResultToCity :: ReqResult () GeocodingResponse -> Maybe Text
reqResultToCity result = case result of
  ResponseSuccess _ xs _ -> geoResponseToMaybeCity xs
  _ -> Nothing

geoResponseToMaybeCity :: GeocodingResponse -> Maybe Text
geoResponseToMaybeCity geoResponse = case status geoResponse of 
  OK -> listToMaybe $ mapMaybe resultToMaybeCity (results geoResponse)
  _ -> Nothing

resultToMaybeCity :: Result -> Maybe Text
resultToMaybeCity Result{address_components, ..} = listToMaybe $ mapMaybe addressToMaybeCity address_components

addressToMaybeCity :: AddressComponent -> Maybe Text
addressToMaybeCity AddressComponent{address_component_types, long_name, ..} = 
  let isLocality = any (\t -> (==) t (AddressType "locality")) address_component_types 
  in  case isLocality of 
        False -> Nothing
        True -> Just long_name

dynQNone :: Reflex t => Dynamic t (QParam a)
dynQNone = constDyn QNone

toGeoLatLng :: Coordinates -> LatLng
toGeoLatLng Coordinates{..} = LatLng latitude longitude