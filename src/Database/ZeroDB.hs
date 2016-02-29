{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.ZeroDB
       ( -- * Connection
         ConnectionInfo(..)
       , local
         -- * ZeroDB Monad
       , ZeroDB
       , runZeroDB
         -- * Querying
       , get
       , select
       , query
       , insert
       , eq
       , ne
       , lt
       , gt
         -- * Misc Types
       , Model
       , Query
       , ObjectID
       , Host
       , Port
       , Username
       , Passphrase
       ) where

import           Control.Exception (SomeException)

import           Data.Data (Data)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           GHC.Word (Word16)

import           Control.Error
import           Control.Lens hiding ((.=))
import           Control.Monad.Reader (ReaderT, MonadReader, runReaderT, ask)
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Aeson.Types (Pair)
import qualified Data.ByteString.Lazy as BSL
import           Data.Hashable (Hashable)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Network.Wreq (FormParam(..), param, partBS,
                               defaults, responseBody)
import           Network.Wreq.Session (Session)
import qualified Network.Wreq.Session as S

type Host = Text
type Port = Word16
type Username = Text
type Passphrase = Text

data ConnectionInfo = ConnectionInfo { hostName :: !Host
                                     , port :: !Port
                                     , username :: !Username
                                     , passphrase :: !Passphrase
                                     } deriving (Eq, Show, Read)

data Connection = Connection { connectionInfo :: ConnectionInfo
                             , session :: Session
                             } deriving (Show)

newtype ZeroDB a =
  ZeroDB { unZeroDB :: ReaderT Connection IO a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader Connection
             )

local :: Username -> Passphrase -> ConnectionInfo
local user pass = ConnectionInfo { hostName = "localhost"
                                 , port = 17234
                                 , username = user
                                 , passphrase = pass
                                 }

mkUri :: ConnectionInfo -> [Text] -> String
mkUri ConnectionInfo{..} pathSegments =
  mconcat ["http://", T.unpack hostName, ":", show port, "/", T.unpack path]
 where path = T.intercalate "/" pathSegments

runZeroDB :: ConnectionInfo -> ZeroDB a -> IO (Either SomeException a)
runZeroDB ci@ConnectionInfo{..} z =
  S.withSession $ \sess -> runExceptT $ syncIO $ do
    let uri = mkUri ci ["_connect"]
    _ <- S.post sess uri ["username" := username, "passphrase" := passphrase]

    res <- runReaderT (unZeroDB z) (Connection ci sess)

    let uri' = mkUri ci ["_disconnect"]
    _ <- S.post sess uri' ([] :: [FormParam])

    pure res

type Model = Text
type Query = Value

newtype ObjectID = ObjectID { unObjectID :: Integer }
                 deriving (Eq, Show, Read, Ord, Num, Enum,
                           Generic, Typeable, Data)

instance Hashable ObjectID where

instance FromJSON ObjectID where
  parseJSON = withObject "oid" $ \o -> do
    oid <- o .: "$oid"
    pure (ObjectID oid)

newtype InsertResponse = InsertResponse (Vector ObjectID)
                       deriving (Eq, Show)

instance FromJSON InsertResponse where
  parseJSON = withObject "insert response" $ \o -> do
    oids <- o .: "oids"
    pure (InsertResponse oids)

get :: FromJSON a => Model -> ObjectID -> ZeroDB a
get model oid = do
  Connection{..} <- ask
  let uri = mkUri connectionInfo [model, "_get"]
  let opts = defaults & param "_id" .~ [T.pack (show (unObjectID oid))]
  resp <- liftIO (S.getWith opts session uri)
  case eitherDecode' (resp ^. responseBody) of
    Left e -> fail e
    Right doc -> pure doc

query :: FromJSON a => Model -> Query -> ZeroDB (Vector a)
query model q = do
  Connection{..} <- ask
  let uri = mkUri connectionInfo [model, "_find"]
  let encodedQuery = BSL.toStrict (encode q)
  resp <- liftIO (S.post session uri [partBS "criteria" encodedQuery])
  case eitherDecode' (resp ^. responseBody) of
    Left e -> fail e
    Right docs -> pure docs

insert :: ToJSON a => Model -> [a] -> ZeroDB (Vector ObjectID)
insert model docs = do
  Connection{..} <- ask
  let uri = mkUri connectionInfo [model, "_insert"]
  let encodedDocs = BSL.toStrict (encode (V.fromList docs))

  resp <- liftIO (S.post session uri [partBS "docs" encodedDocs])

  case eitherDecode' (resp ^. responseBody) of
    Left e -> fail e
    Right (InsertResponse oids) -> pure oids

select :: [Pair] -> Query
select criteria =
  object ["$and" .= Array (V.fromList (map (object . replicate 1) criteria))]

type Field = Text

eq :: Field -> Value -> Pair
eq field value = field .= object ["$eq" .= value]

ne :: Field -> Value -> Pair
ne field value = field .= object ["$ne" .= value]

lt :: Field -> Value -> Pair
lt field value = field .= object ["$lt" .= value]

gt :: Field -> Value -> Pair
gt field value = field .= object ["$gt" .= value]
