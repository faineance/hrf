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

newtype App a = AppT
  { runApp :: ReaderT Config (ExceptT ServantErr IO) a
  } deriving ( Functor, Applicative, Monad, MonadReader Config, MonadError ServantErr, MonadIO)

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks dbPool
  liftIO $ runSqlPool query pool

runAppAsHandler :: Config -> App a -> Handler a
runAppAsHandler cfg app = Handler $ runReaderT (runApp app) cfg


type ListAPI a = Get '[JSON] [Entity a]
type RetrieveAPI a i = Capture "id" i :> Get '[JSON] (Maybe (Entity a))
type CreateAPI a = ReqBody '[JSON] a :> Post '[JSON] (Entity a)


type CRUDAPI (name :: Symbol) a i = name :>
  ( ListAPI a
  :<|> RetrieveAPI a i
  :<|> CreateAPI a
  )

crudAPI
  :: App [Entity a]
  -> (i -> App (Maybe (Entity a)))
  -> (a -> App (Entity a))
  -> ServerT (CRUDAPI name a i) App
crudAPI listAs getA postA = listAs :<|> getA :<|> postA

listModel
  :: (MonadIO m, PersistRecordBackend a SqlBackend) => SqlReadT m [Entity a]
listModel = select $ from $ \p -> return p

{-retrieveModel-}
  {-:: (MonadIO m, PersistRecordBackend a SqlBackend)-}
  {-=> Key a-}
  {--> SqlReadT m (Maybe (Entity a))-}
retrieveModel id = P.getEntity id
{-retrieveModel-}
  {-:: (MonadIO m, PersistRecordBackend a SqlBackend)-}
  {-=> Key a-}
  {--> SqlReadT m (Entity a)-}
{-retrieveModel id = select $ from $ \p -> do-}
  {-where_ (p ^. persistIdField ==. val id)-}
  {-return p-}

{-createModel-}
  {-:: (MonadIO m, PersistRecordBackend a SqlBackend) => a -> SqlWriteT m (Key a)-}
createModel a = insertEntity a
