{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module App where
import Protolude hiding (from)
import Servant.Server
import Database.Esqueleto
import qualified Database.Persist as P
import Servant
import Database.Persist.Postgresql (ConnectionPool,ConnectionString,createPostgresqlPool)

data Config = Config {
  dbPool :: ConnectionPool
}

newtype AppT m a = AppT
  { runApp :: ReaderT Config (ExceptT ServantErr m) a
  } deriving ( Functor, Applicative, Monad, MonadReader Config, MonadError ServantErr, MonadIO)

type App = AppT IO

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks dbPool
  liftIO $ runSqlPool query pool

runAppAsHandler :: Config -> App a -> Handler a
runAppAsHandler cfg app = Handler $ runReaderT (runApp app) cfg


type ListAPI a = Get '[JSON] [Entity a]
type RetrieveAPI a i = Capture "id" i :> Get '[JSON] (Entity a)
type CreateAPI a = ReqBody '[JSON] a :> Post '[JSON] (Entity a)


type CRUDAPI (name :: Symbol) a i = name :>
  ( ListAPI a
  :<|> RetrieveAPI a i
  :<|> CreateAPI a
  )

{-crudAPI-}
  {-:: App [Entity a]-}
  {--> (i -> App ((Entity a)))-}
  {--> (a -> App (Entity a))-}
  {--> ServerT (CRUDAPI name a i) App-}
{-crudAPI listAs getA postA = listAs :<|> getA :<|> postA-}

listModel
  :: ( MonadIO m
     , PersistRecordBackend a SqlBackend
     , (From SqlQuery SqlExpr SqlBackend a1)
     )
  => (a1 -> SqlQuery (SqlExpr (Entity a)))
  -> AppT m [Entity a]
listModel initial = runDb $ select (from initial)


retrieveModel
  :: (MonadIO m, PersistRecordBackend a SqlBackend)
  => Key a
  -> AppT m (Entity a)
retrieveModel id = do
  maybeUser <- runDb $ P.getEntity id
  case maybeUser of
    (Just v) -> return v
    Nothing  -> throwError err404

createModel
  :: (MonadIO m, PersistRecordBackend a SqlBackend) => a -> AppT m (Entity a)
createModel a = do
  maybeUser <- runDb $ insertUniqueEntity a
  case maybeUser of
    (Just v) -> return v
    Nothing  -> throwError err400
