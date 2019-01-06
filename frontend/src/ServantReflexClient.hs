{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ServantReflexClient
  ( createHH
  , updateHH
  , deleteHH
  , queryHH
  )
  where

import Data.UUID
import Reflex.Dom
import qualified Data.Text as T
import Servant.API
import Servant.Reflex
import Data.Proxy

import Common.Dto
import Common.ServantRoutes

apiClients :: forall t m. (MonadWidget t m) => _
apiClients = client hhApi (Proxy @m) (Proxy @()) (constDyn url)
  where url :: BaseUrl
        url = BaseFullUrl Http "3.83.204.196" 3000 "/"
        -- url = BaseFullUrl Http "localhost" 3000 "/"

genCreateHH :: MonadWidget t m
  => Dynamic t (Either T.Text HappyHour)
  -> Event t ()
  -> m (Event t (ReqResult () UUID))
genUpdateHH :: MonadWidget t m
  => Dynamic t (Either T.Text UUID)
  -> Dynamic t (Either T.Text HappyHour)
  -> Event t ()
  -> m (Event t (ReqResult () NoContent))
genDeleteHH :: MonadWidget t m
  => Dynamic t (Either T.Text UUID)
  -> Event t ()
  -> m (Event t (ReqResult () NoContent))
-- getHH :: MonadWidget t m
--   => Dynamic t (Either T.Text UUID)
--   -> Event t () 
--   -> m (Event t (ReqResult () HappyHour))
genQueryHH :: MonadWidget t m
  => Event t ()
  -> m (Event t (ReqResult () [HappyHour]))
genCreateHH :<|> genUpdateHH :<|> genDeleteHH :<|> _ :<|> genQueryHH = apiClients

createHH :: MonadWidget t m
  => Event t HappyHour
  -> m (Event t ())
createHH eHH = do
  dHH <- holdDyn defaultHH eHH
  eCreateResult <- genCreateHH (Right <$> dHH) (() <$ eHH)
  return $ () <$ eCreateResult

updateHH :: MonadWidget t m
  => Event t HappyHour
  -> m (Event t ())
updateHH eHH = do
  let eUUID = fmapMaybe _id eHH
  dHH <- holdDyn defaultHH eHH
  dUUID <- holdDyn nil eUUID
  eCreateResult <- genUpdateHH (Right <$> dUUID) (Right <$> dHH) (() <$ eUUID)
  return $ () <$ eCreateResult 

deleteHH :: MonadWidget t m
  => Event t UUID
  -> m (Event t ())
deleteHH eId = do
  dId <- holdDyn nil eId
  eDeleteResult <- genDeleteHH (Right <$> dId) (() <$ eId)
  return $ () <$ eDeleteResult

queryHH :: MonadWidget t m
  => Event t ()
  -> m (Event t [HappyHour])
queryHH e = do
  eReqResult <- genQueryHH e
  return $ valueOrEmpty <$> eReqResult

valueOrEmpty :: ReqResult () [a] -> [a]
valueOrEmpty result = case result of
  ResponseSuccess _ xs _ -> xs
  _ -> []

_showReqResult :: Show a => ReqResult () a -> String
_showReqResult result = case result of
  ResponseSuccess _ a _ -> "Response success: " ++ show a
  ResponseFailure _ t _ -> "Response failure: " ++ show t
  RequestFailure _ t    -> "Request failure: " ++ show t