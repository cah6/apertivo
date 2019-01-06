module Common.Swagger where 

-- import Data.Aeson (encode)
-- import qualified Data.ByteString.Lazy.Char8 as BSL
-- import Data.Swagger
-- import Data.Swagger.Internal.Schema
-- import Data.Time
-- import Data.Time.Exts
-- import Servant
-- import Servant.Swagger

-- import GHC.Generics

-- import Common.Dto
-- import Common.Routes

-- genSwagger = BSL.putStrLn $ encode $ toSwagger hhApi

-- instance ToSchema HappyHour where
--   declareNamedSchema = genericDeclareNamedSchema $ defaultSchemaOptions { fieldLabelModifier = drop 1 }
-- instance ToSchema Schedule where
--   declareNamedSchema = genericDeclareNamedSchema $ defaultSchemaOptions { fieldLabelModifier = drop 1 }
-- instance ToSchema (DayOfWeek 'Gregorian)
-- instance ToSchema TimeRange where
--   declareNamedSchema _ = pure $ named "TimeRange" $ timeSchema "hh:mm-hh:mm"