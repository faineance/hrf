


{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

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

import Database.Persist.Postgresql (ConnectionPool,ConnectionString,createPostgresqlPool)

share [mkPersist sqlSettings, mkSave "authDefs"] [persistLowerCase|
User json
    username Text
    password Text
    deriving Show Eq Generic
|]

type UserAPI = (CRUDAPI "users" User UserId)
userAPI :: Proxy UserAPI
userAPI = Proxy

userServer :: ServerT UserAPI App
userServer = crudAPI (runDb listModel) undefined undefined

appToServer :: Config -> Server UserAPI
appToServer cfg = hoistServer userAPI (runAppAsHandler cfg) userServer

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration $ migrate  authDefs $ entityDef (Nothing :: Maybe User)

main :: IO ()
main = do
  pool <-runStdoutLoggingT$  createPostgresqlPool "postgresql://localhost" 1
  run 8080 . serve userAPI $  (appToServer (Config pool) )
  {-where-}
    {-app = serve userAPI (appToServer (Config pool) )-}

