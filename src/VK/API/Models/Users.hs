{-# LANGUAGE DeriveGeneric #-}
-- | Models for <http://vk.com/dev/users.get Users API>


module VK.API.Models.Users where

import           Data.Aeson
import           Data.Aeson.Casing
import           GHC.Generics        (Generic)
import           Network.API.Builder


import           VK.API.Models.Types


data SubscriptionsResult = SubscriptionsResult {
  subresultUsers    :: Maybe (Items User)
  , subresultGroups :: Maybe (Items Group)
  , subresultItems  :: Maybe [User]
  }
                         deriving (Show, Generic)

instance FromJSON SubscriptionsResult where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance Receivable SubscriptionsResult where
  receive = useFromJSON

