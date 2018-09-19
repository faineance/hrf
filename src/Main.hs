
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
import Protolude hiding (get, from)
import Database.Persist.TH

import Database.Esqueleto
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
{-xad-}
  {-:: ( Esqueleto query expr backend-}
     {-, PersistEntity Todo-}
     {-, PersistEntityBackend Todo ~ backend-}
     {-)-}
  {-=>-}
  {-{-=> Key User-}-}
   {-(expr (Entity Todo))-}
{-xad = \p -> do-}
  {-{-where_ (p ^. TodoUser ==. val id)-}-}
  {-return p-}

{-asd :: Key User -> a -> SqlQuery a-}

asd
  :: Esqueleto query expr backend
  => Key User
  -> expr (Entity Todo)
  -> query (expr (Entity Todo))
asd id = \p -> do
  where_ (p ^. TodoUser ==. val id)
  return p


all_ ::  a -> SqlQuery a
all_= \p -> do
  return p

api :: Proxy API
api= Proxy

userServer :: ServerT API App
userServer =
  ((listModel (all_) ) :<|> (retrieveModel) :<|> (createModel))
    :<|> (    (listModel (asd (toSqlKey 1) ))
         :<|> (retrieveModel)
         :<|> (createModel)
         )
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
