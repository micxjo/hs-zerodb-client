# hs-zerodb-client

[![BSD3 License](https://img.shields.io/github/license/micxjo/hs-zerodb-client.svg)](https://github.com/micxjo/hs-zerodb-client/blob/master/LICENSE)

This is a Haskell client for the simple JSON API that ships with [ZeroDB](https://www.zerodb.io). It is currently very much  a work in progress.

### Examples

The following is an example of querying the demo database that comes with [zerodb-server](https://github.com/zero-db/zerodb-server).

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
import           Control.Monad
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import           GHC.Generics (Generic)

import           Database.ZeroDB

data Employee = Employee { name :: Text
                         , surname :: Text
                         , salary :: Integer
                         , description :: Text
                         } deriving (Eq, Show, Generic)

instance ToJSON Employee where
instance FromJSON Employee where

main :: IO ()
main = void $ runZeroDB (local "root" "foobar") $ do
  insert "Employee" [ Employee "John" "Doe" 42000 "An employee"
                    , Employee "John" "Smith" 230430 "Another employee"
                    , Employee "Fred" "Jenkins" 10000 "Yet another employee"
                    ]

  emps <- query "Employee" (select [ eq "name" "John"
                                   , gt "salary" (Number 175000)])

  liftIO $ putStrLn ("There are " <> show (V.length emps) <>
                     " employees named John making over $175000.")

  liftIO $ V.forM_ emps (\emp -> do
                            let fullName = name emp <> " " <> surname emp
                            T.putStr fullName
                            putStr " is making $"
                            putStrLn (show (salary emp)))
```
