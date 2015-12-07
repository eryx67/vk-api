{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Common models

module VK.API.Models.Types (module Exp
                           , Items(..)
                           , Likes(..)
                           , ActionResponse(..)
                           , Album(..)
                           ) where

import           Data.Aeson
import           Data.Aeson.Casing
import qualified Data.Text                 as T
import           Data.Time.Clock.POSIX     (POSIXTime)
import           GHC.Generics              (Generic)
import           Network.API.Builder


import           VK.API.CommonTypes        (OwnerId)
import           VK.API.Models.Types.Group as Exp
import           VK.API.Models.Types.User  as Exp
import           VK.Internal.Orphans       ()
import           VK.Internal.Utils

-- import           Debug.Trace

data ActionResponse r = AR { actionResponse :: r}
                      deriving (Show, Generic)

instance (Generic r, FromJSON r) => FromJSON (ActionResponse r) where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance (Generic r, FromJSON r) => Receivable (ActionResponse r) where
  receive = useFromJSON -- . traceShowId

data Items a = Items {items  :: ![a]
                     , count :: !Int
                     }
               deriving (Show, Generic)


instance FromJSON a => FromJSON (Items a)

instance FromJSON a => Receivable (Items a) where
  receive = useFromJSON

data Album = Album {
  albumId            :: !Int
                        -- ^ идентификатор альбома
  , albumOwnerId     :: !OwnerId
                       -- ^ идентификатор владельца
  , albumTitle       :: !T.Text
                        -- ^ название
  , albumCount       :: !(Maybe Int)
  , albumPhoto160    :: !(Maybe T.Text)
                        -- ^ url изображения-обложки ролика с размером 130x98px.
  , albumPhoto320    :: !(Maybe T.Text)
                        -- ^ url изображения-обложки ролика с размером 320x240px.
  , albumUpdatedTime :: !(Maybe POSIXTime)
                        -- ^ дата создания аудиозаписи в формате unixtime.
  }
           deriving (Show, Generic)

instance FromJSON Album where
  parseJSON = genericParseJSON $ aesonPrefix aesonPhotoCase

instance Receivable Album where
  receive = useFromJSON

data Likes = Likes {
  likesUserLikes :: !Int
                    -- ^ число пользователей, которым понравился комментарий,
  , likesCount   :: !Int
                      -- ^ наличие отметки «Мне нравится» от текущего пользователя
  , likesCanLike :: !(Maybe Int)
                    -- ^ информация о том, может ли текущий
                    -- пользователь поставить отметку «Мне нравится»
  }
           deriving (Show, Generic)

instance FromJSON Likes where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
