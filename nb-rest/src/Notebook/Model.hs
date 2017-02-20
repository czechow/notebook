{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Notebook.Model where

import Prelude hiding (id)
import GHC.Generics
import Data.Aeson
import Web.HttpApiData (FromHttpApiData, parseUrlPiece)

newtype Id = Id Int
           deriving (Eq, Ord, Enum, Show, Generic, FromJSON, ToJSON)

instance FromHttpApiData Id where
  parseUrlPiece s = Id <$> parseUrlPiece s

newtype FirstName = FirstName String
                  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

newtype Surname = Surname String
                deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data NewUser = NewUser { firstName :: FirstName
                       , surname :: Surname
                       }
             deriving (Show, Generic)
instance FromJSON NewUser
instance ToJSON NewUser

data User = User { id ::Id
                 , firstName :: FirstName
                 , surname :: Surname }
          deriving (Eq, Ord, Show, Generic)
instance FromJSON User
instance ToJSON User
