{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | Models for <http://vk.com/dev/video.get Video API>

module VK.API.Models.Video where


import           Data.Aeson
import           Data.Aeson.Casing
import qualified Data.Text             as T
import           Data.Time.Clock.POSIX (POSIXTime)
import           GHC.Generics          (Generic)
import           Network.API.Builder


import           VK.API.CommonTypes
import           VK.API.Models.Types
import           VK.Internal.Orphans   ()
import           VK.Internal.Utils

data Video = Video {
  videoId               :: !Int
                        -- ^ идентификатор видеозаписи.
  , videoOwnerId        :: !OwnerId
                       -- ^ идентификатор владельца видеозаписи.
  , videoTitle          :: !T.Text
                        -- ^ название видеозаписи.
  , videoDescription    :: !T.Text
                        -- ^ текст описания видеозаписи.
  , videoDuration       :: !Int
                        -- ^ длительность ролика в секундах.
  , videoPhoto130       :: !T.Text
                        -- ^ url изображения-обложки ролика с размером 130x98px.
  , videoPhoto320       :: !T.Text
                        -- ^ url изображения-обложки ролика с размером 320x240px.
  , videoPhoto640       :: !T.Text
                        -- ^ url изображения-обложки ролика с размером 640x480px (если размер есть).
  , videoDate           :: !POSIXTime
                        -- ^ дата создания видеозаписи в формате unixtime.
  , videoAddingDate     :: !POSIXTime
                       -- ^ дата добавления видеозаписи пользователем или группой в формате unixtime.
  , videoViews          :: !Int
                        -- ^ количество просмотров видеозаписи.
  , videoComments       :: !Int
                        -- ^ количество комментариев к видеозаписи.
  , videoPlayer         :: !T.Text
                        -- ^ адрес страницы с плеером, который можно
                        -- использовать для воспроизведения ролика в
                        -- браузере. Поддерживается flash и html5, плеер всегда
                        -- масштабируется по размеру окна.
  , videoAccessKey      :: !T.Text
                       -- ^ ключ доступа к объекту. Подробнее см. [Ключ
                       -- доступа к данным access_key].
  , videoProcessing     :: !(Maybe Int)
                        -- ^ поле возвращается в том случае, если
                        -- видеоролик находится в процессе обработки,
                        -- всегда содержит 1
  , videoPrivacyView    :: !(Maybe [Privacy])
                           -- ^ настройки приватности в формате настроек приватности
  , videoPrivacyComment :: !(Maybe [Privacy])
                           -- ^ настройки приватности в формате настроек приватности
  , videoCanComment     :: !(Maybe Int)
                         -- ^ может ли текущий пользователь оставлять комментарии к ролику
  , videoCanRepost      :: ! (Maybe Int)
                           -- ^ может ли текущий пользователь
                           -- скопировать ролик с помощью функции
                           -- «Рассказать друзьям»
  , videoLikes          :: !(Maybe Likes)
                           -- ^ информация об отметках «Мне нравится»
  , videoRepeat         :: !(Maybe Int)
                           -- ^ зацикливание воспроизведения
                           -- видеозаписи
  }
           deriving (Show, Generic)

instance FromJSON Video where
  parseJSON = genericParseJSON $ aesonPrefix aesonPhotoCase

instance Receivable Video where
  receive = useFromJSON

newtype VideoEditResponse = VideoEditResponse { videoeditResponse :: Int }
                          deriving (Show, Generic)

instance FromJSON VideoEditResponse where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance Receivable VideoEditResponse where
  receive = useFromJSON

data VideoSaveResponse = VideoSaveResponse {
  videosaveUploadUrl     :: !T.Text
  , videosaveVid         :: !Int
  , videosaveOwnerId     :: !OwnerId
  , videosaveName        :: !T.Text
  , videosaveDescription :: !T.Text
  , videosaveAccessKey   :: !T.Text
                                           }
                       deriving (Show, Generic)

instance FromJSON VideoSaveResponse where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance Receivable VideoSaveResponse where
  receive = useFromJSON

data Comment = Comment {
  commentId           :: !Int
  , commentFromId     :: !Int
  , commentDate       :: !POSIXTime
  , commentText       :: !T.Text
  , commentLikes      :: !Likes
  , commentRealOffset :: !(Maybe Int)
  , commentProfiles   :: !(Maybe [User])
  , commentGroups     :: !(Maybe [Group])
  }
             deriving (Show, Generic)

instance FromJSON Comment where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance Receivable Comment where
  receive = useFromJSON

data Tag = Tag {
  tagUserId       :: Int
  , tagTagId      :: Int
  , tagPlacerId   :: Int
  , tagTaggedName :: T.Text
  , tagDate       :: POSIXTime
  , tagViewed     :: Int
  }
         deriving (Show, Generic)

instance FromJSON Tag where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance Receivable Tag where
  receive = useFromJSON

data NewTag = NewTag {
  newtagTagId      :: Int
  , newtagPlacerId :: Int
  , newtagCreated  :: POSIXTime

  }
         deriving (Show, Generic)

instance FromJSON NewTag where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance Receivable NewTag where
  receive = useFromJSON
