{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | Models for <http://vk.com/dev/audio.get Audio API>


module VK.API.Models.Audio where


import           Data.Aeson
import           Data.Aeson.Casing
import qualified Data.Text             as T
import           Data.Time.Clock.POSIX (POSIXTime)
import           GHC.Generics          (Generic)
import           Network.API.Builder


import           VK.API.CommonTypes
import           VK.API.Models.Types
import           VK.Internal.Orphans   ()



data AudioUser = AudioUser {
  audiouserId        :: !Int
  , audiouserPhoto   :: T.Text
  , audiouserName    :: T.Text
  , audiouserNameGen :: T.Text
  }
               deriving (Show, Generic)

instance FromJSON AudioUser where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Audio = Audio {
  audioId         :: !Int
                        -- ^ идентификатор аудиозаписи.
  , audioOwnerId  :: !OwnerId
                       -- ^ идентификатор владельца аудиозаписи.
  , audioArtist   :: !T.Text
                        -- ^ текст описания аудиозаписи.
  , audioTitle    :: !T.Text
                        -- ^ название аудиозаписи.
  , audioDuration :: !Int
                        -- ^ длительность ролика в секундах.
  , audioDate     :: !POSIXTime
                        -- ^ дата создания аудиозаписи в формате unixtime.
  , audioUrl      :: T.Text
  , audioGenreId  :: !(Maybe AudioGenre)
  , audioAlbumId  :: !(Maybe Int)
  , audioLyricsId :: !(Maybe Int)
                              -- ^ идентификатор текста аудиозаписи (если доступно).
  , audioUser     :: !(Maybe AudioUser)
  }
           deriving (Show, Generic)

instance FromJSON Audio where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance Receivable Audio where
  receive = useFromJSON

data Lyrics = Lyrics {
  lyricsLyricsId :: Int
  , lyricsText   :: T.Text
  }
            deriving (Show, Generic)

instance FromJSON Lyrics where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance Receivable Lyrics where
  receive = useFromJSON

data UploadServer = UploadServer {uploadserverUploadUrl :: !T.Text}
                  deriving (Show, Generic)

instance FromJSON UploadServer where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance Receivable UploadServer where
  receive = useFromJSON

data SavedAudio = SavedAudio {
  savedaudioId        :: !Int
                        -- ^ идентификатор аудиозаписи.
  , savedaudioOwnerId :: !OwnerId
                       -- ^ идентификатор владельца аудиозаписи.
  , savedaudioArtist  :: !T.Text
                        -- ^ текст описания аудиозаписи.
  , savedaudioTitle   :: !T.Text
                        -- ^ название аудиозаписи.
  , savedaudioUrl     :: T.Text
  }
           deriving (Show, Generic)

instance FromJSON SavedAudio where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance Receivable SavedAudio where
  receive = useFromJSON

---
newtype AudioEditResponse = AudioEditResponse { audioeditResponse :: Int }
                          deriving (Show, Generic)

instance FromJSON AudioEditResponse where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance Receivable AudioEditResponse where
  receive = useFromJSON

data AudioSaveResponse = AudioSaveResponse {
  audiosaveUploadUrl     :: !T.Text
  , audiosaveVid         :: !Int
  , audiosaveOwnerId     :: !OwnerId
  , audiosaveName        :: !T.Text
  , audiosaveDescription :: !T.Text
  , audiosaveAccessKey   :: !T.Text
                                           }
                       deriving (Show, Generic)

instance FromJSON AudioSaveResponse where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance Receivable AudioSaveResponse where
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

data BroadcastListResult = BroadcastListUser User
                         | BroadcastListGroup Group
                         deriving Show

instance FromJSON BroadcastListResult where
  parseJSON v@(Object o ) =
    o .:? "type" .!= ("group"::String) >>= parser
    where
      parser "profile" =
        BroadcastListUser <$> parseJSON v
      parser _ =
        BroadcastListGroup <$> parseJSON v
  parseJSON _ =
    mempty

instance Receivable BroadcastListResult where
  receive = useFromJSON

data AlbumResponse = AlbumResponse {
  albumrespAlbumId :: !Int
  }
                 deriving (Show, Generic)

instance FromJSON AlbumResponse where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
