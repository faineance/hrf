


{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE FlexibleInstances          #-}
module Main where
import Protolude hiding (get)
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Servant.Server
import App

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
main = runSqlite "db.sqlite3" $ do
    -- this line added: that's it!
    runMigration $ migrate  authDefs $ entityDef (Nothing :: Maybe User)
    michaelId <- insert $ User "Michael" "a"
    michael <- get michaelId
    liftIO $ print michael
