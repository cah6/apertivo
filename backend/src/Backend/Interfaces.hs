{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
module Backend.Interfaces where

import Control.Monad.Except (ExceptT, void)
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans (MonadTrans(..), liftIO)
import Data.UUID
import Data.UUID.V4 (nextRandom)
import Database.V5.Bloodhound.Client
import Database.V5.Bloodhound.Types

import Common.Dto

-- This file defines my "custom effects". The general procedure for this is:
-- 1. Define the monad
-- 2. Give it a default way to thread itself through transformer stack
-- 3. Generate an instance for each transformer in the stack
-- 4. Give it an IO instance that it will use for production. This should be as
--    short and passthrough-y as possible.

-- GenUUID section
class (Monad m) => GenUUID m where
  genUUID :: m UUID

  default genUUID :: (MonadTrans t, GenUUID m', m ~ t m') => m UUID
  genUUID = lift genUUID 

instance GenUUID m => GenUUID (LoggingT m)
instance GenUUID m => GenUUID (ExceptT e m)

instance GenUUID IO where
  genUUID = nextRandom

-- MonadCrudHappyHour section
class (Monad m) => MonadCrudHappyHour m where
  upsertHappyHour :: UUID -> HappyHour -> m ()
  getHappyHour :: UUID -> m Reply
  deleteHappyHour :: UUID -> m ()
  queryHappyHours :: QueryParams -> m Reply
  
  default upsertHappyHour :: (MonadTrans t, MonadCrudHappyHour m', m ~ t m') => UUID -> HappyHour -> m ()
  upsertHappyHour id = lift . (upsertHappyHour id)

  default getHappyHour :: (MonadTrans t, MonadCrudHappyHour m', m ~ t m') => UUID -> m Reply
  getHappyHour = lift . getHappyHour

  default deleteHappyHour :: (MonadTrans t, MonadCrudHappyHour m', m ~ t m') => UUID -> m ()
  deleteHappyHour = lift . deleteHappyHour

  default queryHappyHours :: (MonadTrans t, MonadCrudHappyHour m', m ~ t m') => QueryParams -> m Reply
  queryHappyHours = lift . queryHappyHours

instance MonadCrudHappyHour m => MonadCrudHappyHour (LoggingT m)
instance MonadCrudHappyHour m => MonadCrudHappyHour (ExceptT e m)

hhIndex = IndexName "happy_hours"
hhMapping = MappingName "happy_hours"

data QueryParams = QueryParams

instance (MonadBH m) => MonadCrudHappyHour (BH m) where
  upsertHappyHour uuid hh = 
    let 
      docId = DocId (toText uuid)
    in
      void $ indexDocument hhIndex hhMapping defaultIndexDocumentSettings hh docId 

  getHappyHour uuid = 
    let
      docId = DocId (toText uuid)
    in  
      getDocument hhIndex hhMapping docId

  deleteHappyHour uuid = 
    let
      docId = DocId (toText uuid)
    in
      void $ deleteDocument hhIndex hhMapping docId

  queryHappyHours qp = do 
    reply <- searchByIndex hhIndex (mkSearch Nothing Nothing)
    liftIO $ putStrLn (show reply)
    return reply