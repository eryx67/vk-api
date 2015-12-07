{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
-- |

module VK.API.Models.Types.Counters where

import           Data.Aeson
import           Data.Aeson.Casing
import           GHC.Generics      (Generic)


data Counters = Counters {
  countersPhotos          :: !(Maybe Int)
  , countersAlbums        :: !(Maybe Int)
  , countersAudios        :: !(Maybe Int)
  , countersVideos        :: !(Maybe Int)
  , countersTopics        :: !(Maybe Int)
  , countersDoc           :: !(Maybe Int)
  , countersNotes         :: !(Maybe Int)
  , countersFriends       :: !(Maybe Int)
  , countersGroups        :: !(Maybe Int)
  , countersOnlineFriends :: !(Maybe Int)
  , countersMutualFriends :: !(Maybe Int)
  , countersUserVideos    :: !(Maybe Int)
  , countersFollowers     :: !(Maybe Int)
  , countersUserPhotos    :: !(Maybe Int)
  , countersSubscriptions :: !(Maybe Int)

  }
              deriving (Show, Generic)

instance FromJSON Counters where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
