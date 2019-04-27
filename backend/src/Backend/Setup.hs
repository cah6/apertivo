{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module Backend.Setup where

import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Database.V5.Bloodhound.Client
import Database.V5.Bloodhound.Types
import Data.Text
import Data.Aeson
import Data.String (IsString)
import GHC.Generics
import Network.Connection (TLSSettings(..))
import Network.HTTP.Client.TLS (mkManagerSettings, newTlsManagerWith, tlsManagerSettings)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Logger (withStdoutLogger)
import Servant
import System.Environment
import System.IO

import Backend.Interfaces
import Backend.MtlServer
import Common.Dto
import Common.ServantRoutes

-- Define servant level settings and run server
run :: IO ()
run = do
  withStdoutLogger $ \apacheLogger -> do
    let port = 3000
        mainLoopF = hPutStrLn stderr ("listening on port " ++ show port)
        settings = (setPort port . setBeforeMainLoop mainLoopF . setLogger apacheLogger) defaultSettings
    runSettings settings mkApp

mkApp :: Application
mkApp = corsWithContentType $ serve hhApi serverDefinition
    where
  serverDefinition = hoistServer hhApi nt server

-- | Allow Content-Type header with values other then allowed by simpleCors.
corsWithContentType :: Middleware
corsWithContentType = cors (const $ Just policy)
    where
      policy = simpleCorsResourcePolicy
        { corsRequestHeaders = ["Content-Type"] 
        , corsMethods = ["POST", "GET", "PUT", "DELETE"]
        }

-- My custom servant stack
newtype MyApp a = MyApp { runMyApp :: BH (ExceptT ServantErr (LoggingT IO)) a }
  deriving (Functor, Applicative, Monad, MonadLogger, MonadIO, MonadError ServantErr, 
            MonadCrudHappyHour, GenUUID, MonadBH)

instance MonadBH IO where
  getBHEnv = liftIO getBHEnv

instance MonadBH m => MonadBH (ExceptT e m) where
  getBHEnv = lift getBHEnv

instance MonadBH m => MonadBH (LoggingT m) where
  getBHEnv = lift getBHEnv

instance MonadLogger m => MonadLogger (BH m)
instance GenUUID m => GenUUID (BH m)

-- Custom stack -> predefined servant Handler stack
nt :: MyApp a -> Handler a
nt (MyApp m) = do
  bhEnv <- liftIO provideEnv
  res <- (liftIO . runStdoutLoggingT . runExceptT . runBH bhEnv) m
  case res of
    Left e    -> throwError e
    Right res -> return res

provideEnv :: IO BHEnv
provideEnv = do
  manager <- newTlsManagerWith tlsSettings
  espw <- pack <$> getEnv "ES_PASSWORD"
  return $ addAuth espw $ mkBHEnv (Server "https://d645aa815ce645b695844914a1ccb37f.us-east-1.aws.found.io:9243/") manager
    where
  addAuth pw env = env { bhRequestHook = basicAuthHook (EsUsername "elastic") (EsPassword pw) }
  tlsSettings = mkManagerSettings (TLSSettingsSimple False True True) Nothing

server :: ServerT HappyHourApi MyApp
server = createHH :<|> updateHH :<|> deleteHH :<|> getHH :<|> queryHHs