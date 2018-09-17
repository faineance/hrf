
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances          #-}
module Main where

import Network.Wai.Handler.Warp
import Protolude hiding (get)
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite

import Control.Monad.Logger                 (runNoLoggingT, runStdoutLoggingT)
import Servant.Server
import App
import Database.Persist.Postgresql (ConnectionPool,ConnectionString,createPostgresqlPool, runSqlPool)

import           Servant
share [mkPersist sqlSettings, mkMigrate "authDefs"] [persistLowerCase|
User json
    username Text
    password Text
    UniqueUser username
    deriving Show Eq Generic

Todo json
    name Text
    completed Bool
    user UserId
    deriving Show Eq Generic
|]

type API = (CRUDAPI "users" User UserId) :<|> (CRUDAPI "todos" Todo TodoId)

api :: Proxy API
api= Proxy

userServer :: ServerT API App
userServer =
  crudAPI (listModel) (retrieveModel) (createModel)
  :<|> crudAPI (listModel) (retrieveModel) (createModel)

appToServer :: Config -> Server API
appToServer cfg = hoistServer api (runAppAsHandler cfg) userServer

doMigrations :: ReaderT SqlBackend IO ()
doMigrations =
  runMigration authDefs

main :: IO ()
main = do
  pool <- runStdoutLoggingT
    $ createPostgresqlPool "postgresql://postgres@localhost" 1
  runSqlPool doMigrations pool
  run 8080 . serve api $ (appToServer (Config pool))
