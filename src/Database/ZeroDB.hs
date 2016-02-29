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
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Hashable (Hashable)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import           Network.Wreq (Response, FormParam(..), param, partBS,
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

get :: FromJSON a => Model -> ObjectID -> ZeroDB a
get model oid = do
  Connection{..} <- ask
  let uri = mkUri connectionInfo [model, "_get"]
  let opts = defaults & param "_id" .~ [T.pack (show (unObjectID oid))]
  resp <- liftIO (S.getWith opts session uri)
  case eitherDecode' (resp ^. responseBody) of
    Left e -> fail e
    Right doc -> pure doc

query :: Model -> Query -> ZeroDB (Response ByteString)
query model q = do
  Connection{..} <- ask
  let uri = mkUri connectionInfo [model, "_find"]
  let encodedQuery = BSL.toStrict (encode q)
  liftIO (S.post session uri [partBS "criteria" encodedQuery])

insert :: ToJSON a => Model -> [a] -> ZeroDB (Response ByteString)
insert model docs = do
  Connection{..} <- ask
  let uri = mkUri connectionInfo [model, "_insert"]
  let encodedDocs = BSL.toStrict (encode (V.fromList docs))

  liftIO (S.post session uri [partBS "docs" encodedDocs])

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
