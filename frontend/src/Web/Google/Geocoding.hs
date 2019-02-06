{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

-- |
-- Module      : Web.Google.Geocoding
-- Description : Bindings to the Google Geocoding API (formerly Maps Geocoding
--               API)
-- Copyright   : (c) Mike Pilgrem 2017, 2018
-- Maintainer  : public@pilgrem.com
-- Stability   : experimental
module Web.Google.Geocoding
    ( -- * API
      GoogleGeocodingAPI
    , api
      -- * Types
    , Key                  (..)
    , Address              (..)
    , FilterComponent      (..)
    , Viewport             (..)
    , Language             (..)
    , Region               (..)
    , GeocodingResponse    (..)
    , Status               (..)
    , Result               (..)
    , AddressType          (..)
    , AddressComponent     (..)
    , PostcodeLocality     (..)
    , Geometry             (..)
    , LatLng               (..)
    , PlaceId              (..)
    , Location             (..)
    , LocationType         (..)
    ) where

import Data.Aeson hiding (Result)
import Data.Aeson.Types (Options (..))
import Data.List (intersperse)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T (concat, unpack)
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager)
import Servant.API
import Web.Google.Maps.Common (Address (..), Key (..),
    Language (..), LatLng (..), Location (..), Region (..))

-- | Fliter component: a component that can be used to filter the results
-- returned in a geocoding response.
data FilterComponent
    = Route Text
    | Locality Text
    | AdministrativeArea Text
    | PostalCode Text
    | Country Region
    deriving (Eq, Show)

instance ToHttpApiData FilterComponent where
    toUrlPiece filterComponent
        | Route route <- filterComponent
          = T.concat ["route:", route]
        | Locality locality <- filterComponent
          = T.concat ["locality:", locality]
        | AdministrativeArea adminArea <- filterComponent
          = T.concat ["administrative_area:", adminArea]
        | PostalCode postalCode <- filterComponent
          = T.concat ["postal_code:", postalCode]
        | Country country <- filterComponent
          = T.concat ["country:", toUrlPiece country]

instance ToHttpApiData [FilterComponent] where
    toUrlPiece [] = ""
    toUrlPiece cs = T.concat $ intersperse "|" $ map toUrlPiece cs

-- | Geocoding Reponse
data GeocodingResponse = GeocodingResponse
    { status        :: Status
    , error_message :: Maybe Text
    , results       :: [Result]
    } deriving (Eq, Show, Generic)

instance FromJSON GeocodingResponse

-- | Contains the status of the request and may contain debugging information to
--  help you track down why geocoding is not working.
data Status
    = OK              -- ^ Indicates that no errors occurred; the address was
                      -- successfully parsed and at least one geocode was
                      -- returned.
    | ZeroResults     -- ^ Indicates that the geocode was successful but
                      -- returned no results. This may occur if the geocoder was
                      -- passed a non-existent address.
    | OverQueryLimit
    | RequestDenied
    | InvalidRequest  -- ^ Generally indicates that the query (address,
                      -- components or latlng) is missing.
    | UnknownError
    deriving (Eq, Show)

instance FromJSON Status where
    parseJSON = withText "Status" $ \t -> case t of
        "OK"               -> return OK
        "ZERO_RESULTS"     -> return ZeroResults
        "OVER_QUERY_LIMIT" -> return OverQueryLimit
        "REQUEST_DENIED"   -> return RequestDenied
        "INVALID_REQUEST"  -> return InvalidRequest
        "UNKNOWN_ERROR"    -> return UnknownError
        _                  -> fail $ "Unrecognised status type, namely: " ++
                                  T.unpack t

-- | A result of the geocoder.
data Result = Result
    { types :: [AddressType]
    , formatted_address :: Text
    , address_components :: [AddressComponent]
    , postcode_localities :: Maybe [PostcodeLocality]
    , geometry :: Geometry
    , partial_match :: Maybe Bool
    , place_id :: PlaceId
    } deriving (Eq, Show, Generic)

instance FromJSON Result

-- | Address (and address component) type: The list of types provided by Google
-- (as at 4 March 2017) is incomplete.
newtype AddressType = AddressType Text
    deriving (Eq, Show, Generic, ToHttpApiData)

instance FromJSON AddressType

-- | Address component
data AddressComponent = AddressComponent
    { address_component_types      :: [AddressType]
    , long_name  :: Text
    , short_name :: Text
    } deriving (Eq, Show, Generic)

instance FromJSON AddressComponent where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = \l -> case l of
            "address_component_types" -> "types"
            _ -> l
        }

-- | Postcode locality: a locality contained in a postal code.
newtype PostcodeLocality = PostcodeLocality Text
    deriving (Eq, Show, Generic)

instance FromJSON PostcodeLocality

-- | Geometry
data Geometry = Geometry
    { location :: LatLng
    , location_type :: LocationType
    , viewport :: Viewport
    , bounds :: Maybe Viewport
    } deriving (Eq, Show, Generic)

instance FromJSON Geometry

-- | Location type
data LocationType
    = Rooftop
    | RangeInterpolated
    | GeometricCenter
    | Approximate
    deriving (Eq, Show)

instance ToHttpApiData LocationType where
    toUrlPiece locationType = case locationType of
        Rooftop           -> "ROOFTOP"
        RangeInterpolated -> "RANGE_INTERPOLATED"
        GeometricCenter   -> "GEOMETRIC_CENTER"
        Approximate       -> "APPROXIMATE"

instance FromJSON LocationType where
    parseJSON = withText "LocationType" $ \t -> case t of
        "ROOFTOP"            -> return Rooftop
        "RANGE_INTERPOLATED" -> return RangeInterpolated
        "GEOMETRIC_CENTER"   -> return GeometricCenter
        "APPROXIMATE"        -> return Approximate
        _ -> fail $ "Unrecognised location type, namely: " ++ T.unpack t

-- | Viewport
data Viewport = Viewport
    { southwest :: LatLng
    , northeast :: LatLng
    } deriving (Eq, Show, Generic)

instance ToHttpApiData Viewport where
    toUrlPiece (Viewport sw ne) = T.concat [toUrlPiece sw, "|", toUrlPiece ne]

instance FromJSON Viewport

-- | Place id
newtype PlaceId = PlaceId Text
    deriving (Eq, Show, Generic, ToHttpApiData)

instance FromJSON PlaceId

-- | Google Geocoding API
type GoogleGeocodingAPI
    =    "geocode"
    :>   "json"
    :>   QueryParam "key"           Key
    :>   QueryParam "address"       Address
    :>   QueryParam "components"    [FilterComponent]
    :>   QueryParam "bounds"        Viewport
    :>   QueryParam "language"      Language
    :>   QueryParam "region"        Region
    :>   Get '[JSON] GeocodingResponse
    :<|> "geocode"
    :>   "json"
    :>   QueryParam "key"           Key
    :>   QueryParam "latlng"        LatLng
    :>   QueryParam "place_id"      PlaceId
    :>   QueryParam "result_type"   AddressType
    :>   QueryParam "location_type" LocationType
    :>   QueryParam "language"      Language
    :>   Get '[JSON] GeocodingResponse

-- | API type
api :: Proxy GoogleGeocodingAPI
api = Proxy

-- geocode'
--     :: Maybe Key
--     -> Maybe Address
--     -> Maybe [FilterComponent]
--     -> Maybe Viewport
--     -> Maybe Language
--     -> Maybe Region
--     -> ClientM GeocodingResponse
-- backGeocode'
--     :: Maybe Key
--     -> Maybe LatLng
--     -> Maybe PlaceId
--     -> Maybe AddressType
--     -> Maybe LocationType
--     -> Maybe Language
--     -> ClientM GeocodingResponse
-- geocode' :<|> backGeocode' = client api

-- -- | Geocode. NB: The use of the Google Geocoding API services is subject to the
-- -- <https://cloud.google.com/maps-platform/terms/ Google Maps Platform Terms of Service>.
-- -- End Users’ use of Google Maps is subject to the then-current Google
-- -- Maps/Google Earth Additional Terms of Service at
-- -- <https://maps.google.com/help/terms_maps.html> and Google Privacy Policy at
-- -- <https://www.google.com/policies/privacy/>.
-- geocode
--     :: Manager
--     -> Key
--     -> Maybe Address
--     -> Maybe [FilterComponent]
--     -> Maybe Viewport
--     -> Maybe Language
--     -> Maybe Region
--     -> IO (Either ServantError GeocodingResponse)
-- geocode
--     mgr
--     key
--     addressOpt
--     filterComponentsOpt
--     viewportOpt
--     languageOpt
--     regionOpt
--     = runClientM (geocode' (Just key) addressOpt filterComponentsOpt viewportOpt
--           languageOpt regionOpt)
--           (ClientEnv mgr googleMapsApis Nothing)
--           -- (ClientEnv mgr googleMapsApis)


-- -- | Reverse (back) geocode. NB: The use of the Google Geocoding API services is
-- -- subject to the
-- -- <https://cloud.google.com/maps-platform/terms/ Google Maps Platform Terms of Service>.
-- -- End Users’ use of Google Maps is subject to the then-current Google
-- -- Maps/Google Earth Additional Terms of Service at
-- -- <https://maps.google.com/help/terms_maps.html> and Google Privacy Policy at
-- -- <https://www.google.com/policies/privacy/>.
-- backGeocode
--     :: Manager
--     -> Key
--     -> Maybe LatLng
--     -> Maybe PlaceId
--     -> Maybe AddressType
--     -> Maybe LocationType
--     -> Maybe Language
--     -> IO (Either ServantError GeocodingResponse)
-- backGeocode
--     mgr
--     key
--     latLngOpt
--     placeIdOpt
--     addressTypeOpt
--     locationTypeOpt
--     languageOpt
--     = runClientM (backGeocode' (Just key) latLngOpt placeIdOpt addressTypeOpt
--           locationTypeOpt languageOpt)
--           (ClientEnv mgr googleMapsApis Nothing)
--           -- (ClientEnv mgr googleMapsApis)