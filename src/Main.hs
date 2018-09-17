
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
import Database.Persist.Postgresql (ConnectionPool,ConnectionString,createPostgresqlPool, runSqlPool)

share [mkPersist sqlSettings, mkSave "authDefs"] [persistLowerCase|
User json
    username Text
    password Text
    UniqueUser username
    deriving Show Eq Generic
|]

type UserAPI = (CRUDAPI "users" User UserId)

userAPI :: Proxy UserAPI
userAPI = Proxy

userServer :: ServerT UserAPI App
userServer =
  crudAPI (listModel) (retrieveModel) (createModel)

appToServer :: Config -> Server UserAPI
appToServer cfg = hoistServer userAPI (runAppAsHandler cfg) userServer

doMigrations :: ReaderT SqlBackend IO ()
doMigrations =
  runMigration $ migrate authDefs $ entityDef (Nothing :: Maybe User)

main :: IO ()
main = do
  pool <- runStdoutLoggingT
    $ createPostgresqlPool "postgresql://postgres@localhost" 1
  runSqlPool doMigrations pool
  run 8080 . serve userAPI $ (appToServer (Config pool))
