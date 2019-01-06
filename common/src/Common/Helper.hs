module Common.Helper where

import Data.Aeson (FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Attoparsec.Text (parseOnly)
import qualified Data.Attoparsec.Text as Attoparsec

attoToAeson :: Attoparsec.Parser a -> Value -> Aeson.Parser a
attoToAeson parser (String x) = case parseOnly parser x of
  Left e -> fail e
  Right x -> pure x
attoToAeson _ x = typeMismatch "Attoparsec" x