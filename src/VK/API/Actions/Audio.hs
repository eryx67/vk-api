{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
-- | Actions for <http://vk.com/dev/audio.get Audio API>

module VK.API.Actions.Audio where


import           Data.Aeson
import           Data.Aeson.Casing
import qualified Data.Default.Generics         as D
import qualified Data.Text                     as T
import           GHC.Generics                  (Generic)


import           Network.API.Builder.Queryable
import           Network.API.Builder.Receive
import           Network.API.Builder.TH        (deriveQueryable', standard)

import           VK.API.Actions.Types
import           VK.API.CommonTypes
import           VK.Internal.Orphans           ()
import           VK.Internal.Utils

data Get = Get {
  getOwnerId    :: !(Maybe OwnerId)
                 -- ^ идентификатор пользователя или сообщества,
                 -- которому принадлежат видеозаписи.
  , getAudioIds :: !(Maybe [Int])
                 -- ^ перечисленные через запятую идентификаторы
  , getAlbumId  :: !(Maybe Int)
                   -- ^ идентификатор альбома, записи из которого нужно вернуть.
  , getCount    :: !(Maybe Int)
                -- ^ количество возвращаемых записей.
  , getOffset   :: !(Maybe Int)
                 -- ^ смещение относительно первой найденной записи для
                 -- выборки определенного подмножества.
  , getNeedUser :: !(Maybe Int)
                   -- ^ возвращать информацию о пользователях, загрузивших аудиозапись
  }
           deriving Show

instance D.Default Get where
  def = Get
        Nothing Nothing Nothing
        Nothing Nothing Nothing

$(deriveQueryable' (standard . dropLPrefix) ''Get)

data GetById = GetById {getbyidAudios :: ![(OwnerId, Int)]}
           deriving Show

$(deriveQueryable' (standard . dropLPrefix) ''GetById)

data GetLyrics = GetLyrics {getlyricsLyricsId :: !Int}
               deriving Show

$(deriveQueryable' (standard . dropLPrefix) ''GetLyrics)

data Search = Search {
  searchQ               :: !T.Text
                       -- ^ строка поискового запроса. Например, The Beatles.
  , searchAutoComplete  :: !(Maybe Int)
  , searchLyrics        :: !(Maybe Int)
                    -- ^ Если этот параметр равен 1, поиск будет производиться только
                    -- по тем аудиозаписям, которые содержат тексты
  , searchPerformerOnly :: !(Maybe Int)
                           -- ^ Если этот параметр равен 1, поиск
                           -- будет осуществляться только по названию
                           -- исполнителя.
  , searchSort          :: !(Maybe AudioSort)
  , searchSearchOwn     :: !(Maybe OwnFilter)
  , searchOffset        :: !(Maybe Int)
                       -- ^ смещение относительно первой найденной записи
                       -- для выборки определенного подмножества.
  , searchCount         :: !(Maybe Int)
                       -- ^ количество возвращаемых записей.
  }
              deriving Show

instance D.Default Search where
  def = Search ""
        Nothing Nothing Nothing Nothing Nothing
        Nothing Nothing

$(deriveQueryable' (standard . dropLPrefix) ''Search)

data GetUploadServer = GetUploadServer
                     deriving Show

instance Queryable GetUploadServer where
  toURLParams _ = []

data Save = Save {
  saveServer   :: !Int
                -- ^ параметр, возвращаемый в результате загрузки аудиофайла на сервер.
  , saveAudio  :: !T.Text
                   -- ^ параметр, возвращаемый в результате загрузки аудиофайла на сервер.
  , saveHash   :: !T.Text
                -- ^ параметр, возвращаемый в результате загрузки аудиофайла на сервер.
  , saveArtist :: !(Maybe T.Text)
                  -- ^ автор композиции. По умолчанию берется из ID3 тегов.
  , saveTitle  :: !(Maybe T.Text)
                 -- ^ название композиции. По умолчанию берется из ID3 тегов.
  }
          deriving (Show, Generic)

instance D.Default Save where
  def = Save 0 "" ""
        Nothing Nothing

$(deriveQueryable' (standard . dropLPrefix) ''Save)

instance FromJSON Save where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance Receivable Save where
  receive = useFromJSON

data Add = Add {
  addAudioId   :: !Int
  , addOwnerId :: !OwnerId
  , addGroupId :: !(Maybe Int)
  }
         deriving Show

instance D.Default Add where
  def = Add 0 (UserId 0) Nothing

$(deriveQueryable' (standard . dropLPrefix) ''Add)

data Delete = Delete {
  deleteAudioId   :: !Int
  , deleteOwnerId :: !OwnerId
  }
         deriving Show

instance D.Default Delete where
  def = Delete 0 (UserId 0)

$(deriveQueryable' (standard . dropLPrefix) ''Delete)

data Edit = Edit {
  editAudioId    :: !Int
                        -- ^ идентификатор аудиозаписи.
  , editOwnerId  :: !OwnerId
                   -- ^ идентификатор владельца аудиозаписи.
  , editArtist   :: !(Maybe T.Text)
                    -- ^ текст описания аудиозаписи.
  , editTitle    :: !(Maybe T.Text)
                        -- ^ название аудиозаписи.
  , editText     :: !(Maybe T.Text)
  , editGenreId  :: !(Maybe AudioGenre)
  , editNoSearch :: !(Maybe Int)
  }
           deriving Show

instance D.Default Edit where
  def = Edit 0 (UserId 0)
        Nothing Nothing Nothing
        Nothing Nothing

$(deriveQueryable' (standard . dropLPrefix) ''Edit)


data Reorder = Reorder {
  reorderAudioId   :: !Int
  , reorderOwnerId :: !(Maybe OwnerId)
  , reorderBefore  :: !(Maybe Int)
                     -- ^ идентификатор записи, перед которым нужно поместить текущий
  , reorderAfter   :: !(Maybe Int)
                     -- ^ идентификатор, после которого нужно поместить текущий
  }
          deriving Show

instance D.Default Reorder where
  def = Reorder 0 Nothing Nothing Nothing

$(deriveQueryable' (standard . dropLPrefix) ''Reorder)

data Restore = Restore {
  restoreAudioId   :: !Int
  , restoreOwnerId :: !(Maybe OwnerId)
  }
         deriving Show

instance D.Default Restore where
  def = Restore 0 Nothing

$(deriveQueryable' (standard . dropLPrefix) ''Restore)

data Albums = Albums {
  albumsOwnerId  :: !(Maybe OwnerId)
                   -- ^ идентификатор владельца альбомов (пользователь или
                   -- сообщество). По умолчанию — идентификатор текущего пользователя.
  , albumsOffset :: !(Maybe Int)
                           -- ^ смещение, необходимое для выборки определенного подмножества
                           -- альбомов. По умолчанию — 0.
  , albumsCount  :: !(Maybe Int)
  }
              deriving Show

instance D.Default Albums where
  def = Albums
        Nothing Nothing Nothing

$(deriveQueryable' (standard . dropLPrefix) ''Albums)

data AddAlbum = AddAlbum {
  addalbumTitle     :: T.Text
  , addalbumGroupId :: !(Maybe Int)
  }
          deriving Show

instance D.Default AddAlbum where
  def = AddAlbum "" Nothing

$(deriveQueryable' (standard . dropLPrefix) ''AddAlbum)

data EditAlbum = EditAlbum {
  editalbumAlbumId   :: !Int
  , editalbumTitle   :: !T.Text
  , editalbumGroupId :: !(Maybe Int)
  }
          deriving Show

instance D.Default EditAlbum where
  def = EditAlbum 0 ""
        Nothing

$(deriveQueryable' (standard . dropLPrefix) ''EditAlbum)

data DeleteAlbum = DeleteAlbum {
  deletealbumAlbumId   :: !Int
  , deletealbumGroupId :: !(Maybe Int)
  }
          deriving Show

instance D.Default DeleteAlbum where
  def = DeleteAlbum 0 Nothing

$(deriveQueryable' (standard . dropLPrefix) ''DeleteAlbum)

data MoveToAlbum = MoveToAlbum {
  movetoalbumAudioIds  :: ![Int]
  , movetoalbumAlbumId :: !Int
  , movetoalbumGroupId :: !(Maybe Int)
  }
                 deriving Show

instance D.Default MoveToAlbum where
  def = MoveToAlbum [] 0 Nothing

$(deriveQueryable' (standard . dropLPrefix) ''MoveToAlbum)

data Broadcast = Broadcast {
  broadcastAudio       :: !(OwnerId, Int)
  , broadcastTargetIds :: !(Maybe [OwnerId])
  }
                 deriving Show

instance D.Default Broadcast where
  def = Broadcast (UserId 0, 0) Nothing

$(deriveQueryable' (standard . dropLPrefix) ''Broadcast)

data BroadcastList = BroadcastList {
  broadcastlistFilter   :: !(Maybe BroadcastsFilter)
  , broadcastlistActive :: !(Maybe Int)
  }
                   deriving Show

instance D.Default BroadcastList where
  def = BroadcastList Nothing Nothing

$(deriveQueryable' (standard . dropLPrefix) ''BroadcastList)


data Recommendations = Recommendations {
  recommendationsTargetAudio :: !(OwnerId, Int)
                               -- ^ идентификатор аудиозаписи, на
                               -- основе которой будет строиться
                               -- список рекомендаций. Используется
                               -- вместо параметра uid. Идентификатор
                               -- представляет из себя разделённые
                               -- знаком подчеркивания id
                               -- пользователя, которому принадлежит
                               -- аудиозапись, и id самой
                               -- аудиозаписи. Если аудиозапись
                               -- принадлежит сообществу, то в
                               -- качестве первого параметра
                               -- используется -id сообщества.
  , recommendationsUserId    :: !(Maybe Int)
                          -- ^ идентификатор пользователя для
                          -- получения списка рекомендаций на основе
                          -- его набора аудиозаписей (по умолчанию —
                          -- идентификатор текущего пользователя).
  , recommendationsOffset    :: !(Maybe Int)
                            -- ^ смещение относительно первой
                            -- найденной аудиозаписи для выборки
                            -- определенного подмножества.
  , recommendationsCount     :: !(Maybe Int)
                         -- ^ количество возвращаемых аудиозаписей.
  , recommendationsShuffle   :: !(Maybe Int)
                           -- ^ 1 — включен случайный порядок.
  }
                      deriving Show

instance D.Default Recommendations where
  def = Recommendations (UserId 0, 0)
        Nothing Nothing Nothing Nothing

$(deriveQueryable' (standard . dropLPrefix) ''Recommendations)

data Popular = Popular {
  popularOnlyEng   :: !(Maybe Int)
  , popularGenreId :: !(Maybe Int)
                          -- ^ идентификатор пользователя для
                          -- получения списка рекомендаций на основе
                          -- его набора аудиозаписей (по умолчанию —
                          -- идентификатор текущего пользователя).
  , popularOffset  :: !(Maybe Int)
                            -- ^ смещение относительно первой
                            -- найденной аудиозаписи для выборки
                            -- определенного подмножества.
  , popularCount   :: !(Maybe Int)
                         -- ^ количество возвращаемых аудиозаписей.
  }
                      deriving Show

instance D.Default Popular where
  def = Popular
        Nothing Nothing Nothing Nothing

$(deriveQueryable' (standard . dropLPrefix) ''Popular)

data GetCount = GetCount {
  getcountOwnerId :: OwnerId
  }
              deriving Show

instance D.Default GetCount where
  def = GetCount (UserId 0)

$(deriveQueryable' (standard . dropLPrefix) ''GetCount)
