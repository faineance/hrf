
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances          #-}
module API where

import Network.Wai.Handler.Warp
import Protolude hiding (get, from)
import Database.Persist.TH
import CustomKey
import Database.Esqueleto
import Control.Monad.Logger                 (runNoLoggingT, runStdoutLoggingT)
import Servant.Server
import App
import Database.Persist.Postgresql (ConnectionPool,ConnectionString,createPostgresqlPool, runSqlPool)
import Data.Swagger
import Servant
import Servant.Swagger
import Servant.Swagger.UI

share [mkPersist sqlSettings, mkMigrate "authDefs"] [persistLowerCase|
User json
    username Text
    password Text
    UniqueUser username
    deriving Show Eq Generic Typeable

Todo json
    name Text
    completed Bool
    user UserId
    deriving Show Eq Generic Typeable
|]

instance ToSchema (Entity Todo)
instance ToSchema Todo
instance ToSchema TodoId
instance ToParamSchema TodoId

instance ToSchema (Entity User)
instance ToSchema User
instance ToSchema UserId
instance ToParamSchema UserId

type AppAPI = (CRUDAPI "users" User UserId) :<|> (CRUDAPI "todos" Todo TodoId)

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

type API = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> AppAPI

byUser id = \p -> do
  where_ (p ^. TodoUser ==. val id)
  return p

all_ :: a -> SqlQuery a
all_ = \p -> do
  return p

appAPI :: Proxy AppAPI
appAPI = Proxy

api :: Proxy API
api = Proxy

server :: ServerT AppAPI App
server =  (users :<|> todos)
 where
  users = ((listModel (all_)) :<|> (retrieveModel all_) :<|> (createModel))
  todos =
    (listModel defaultInitial)
      :<|> (retrieveModel defaultInitial)
      :<|> (createModel)
  defaultInitial = byUser (toSqlKey 1)

appToServer :: Config -> Server API
appToServer cfg =
  swaggerSchemaUIServer (toSwagger appAPI)
    :<|> (hoistServer appAPI (runAppAsHandler cfg) server)

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration authDefs

runAPI :: IO ()
runAPI = do
  pool <- runStdoutLoggingT
    $ createPostgresqlPool "postgresql://postgres@localhost" 1
  runSqlPool doMigrations pool
  run 8080 . serve api $ (appToServer (Config pool))

