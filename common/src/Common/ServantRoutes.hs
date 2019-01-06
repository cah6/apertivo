{-# LANGUAGE DataKinds, TypeOperators #-}
module Common.ServantRoutes where 

import Servant.API
import Data.Proxy
import Data.UUID (UUID)
import Data.Text (Text)

import Common.Dto

hhApi :: Proxy HappyHourApi
hhApi = Proxy

type HappyHourApi =
      -- createHH
      "happy-hours" :> ReqBody '[JSON] HappyHour :> Post '[JSON] UUID
      -- updateHH
  :<|>"happy-hours" :> Capture "happy-hour-id" UUID :> ReqBody '[JSON] HappyHour :> Put '[JSON] NoContent
      -- deleteHH
  :<|>"happy-hours" :> Capture "happy-hour-id" UUID :> Delete '[JSON] NoContent
      -- getHH
  :<|>"happy-hours" :> Capture "happy-hour-id" UUID :> Get '[JSON] HappyHour
      -- queryHHs
  :<|>"happy-hours" :> Get '[JSON] [HappyHour]
