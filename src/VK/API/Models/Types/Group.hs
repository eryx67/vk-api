{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- |

module VK.API.Models.Types.Group where

import           Data.Aeson
import           Data.Aeson.Casing
import qualified Data.Text                    as T
import           Data.Time.Clock.POSIX        (POSIXTime)
import           GHC.Generics                 (Generic)


import           VK.API.Models.Types.Counters
import           VK.Internal.Orphans          ()
import           VK.Internal.Utils

data GroupOpenness = OpenGroup
               | ClosedGroup
               | PrivateGroup
         deriving (Show, Generic, Bounded, Enum)

instance FromJSON GroupOpenness where
  parseJSON =
    withScientific "GroupOpenness"
    (\v ->
       case (round v :: Int) of
       idx | idx >= length gts || idx < 0 ->
               mempty
       idx ->
         pure $ gts !! idx
    )
    where
       gts = [minBound ..]::[GroupOpenness]

-- instance ToQuery GroupOpenness where
--  toQuery n v = toQuery n (fromEnum v)

data GroupBlockage = GroupDeleted
               | GroupBanned
         deriving (Show, Generic, Bounded, Enum)

instance FromJSON GroupBlockage where
  parseJSON =
    withText "GroupBlockage"
    (\case
         "deleted" -> pure GroupDeleted
         "banned" -> pure GroupBanned
         _ -> mempty
    )

data GroupAdminLevel = GroupModer
                     | GroupEditor
                     | GroupAdmin
         deriving (Show, Generic, Bounded, Enum)

instance FromJSON GroupAdminLevel where
  parseJSON =
    intEnumFromJSON "GroupAdminLevel" ([minBound ..]::[GroupAdminLevel])

data GroupType = GroupGroup
               | PageGroup
               | EventGroup
         deriving (Show, Generic, Bounded, Enum)

instance FromJSON GroupType where
  parseJSON =
    withText "GroupType"
    (\case
         "group" -> pure GroupGroup
         "page" -> pure PageGroup
         "event" -> pure EventGroup
         _ -> mempty
    )

data BanInfo = BanInfo {
  baninfoEndDate   :: !POSIXTime
                    -- ^ срок окончания блокировки в формате unixtime;
  , baninfoComment :: T.Text
  }
               deriving (Show, Generic)

instance FromJSON BanInfo where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Place = Place {
  placeId          :: !Int
             -- ^ идентификатор места;
  , placeTitle     :: !T.Text
                  -- ^ название места
  , placeLatitude  :: !Int
                     -- ^ географическая широта, заданная в градусах (от -90 до 90);
  , placeLongitude :: !Int
                      -- ^ географическая долгота, заданная в градусах (от -180 до 180);
  , placeType      :: !T.Text
                 -- ^ тип места
  , placeCountry   :: !Int
                    -- ^ идентификатор страны;
  , placeCity      :: !Int
                 -- ^ идентификатор города;
  , placeAddress   :: !T.Text
  }
             deriving (Show, Generic)

instance FromJSON Place where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Group = Group {
  groupId               :: !Int
             -- ^ идентификатор сообщества.
  , groupName           :: !T.Text
    -- ^ название сообщества.
  , groupScreenName     :: !T.Text
                       -- ^ короткий адрес сообщества, например, apiclub.

  , groupIsClosed       :: !GroupOpenness
                     -- ^ является ли сообщество закрытым. Возможные значения:

  , groupDeactivated    :: !(Maybe GroupBlockage)
                        -- ^ возвращается в случае, если сообщество удалено или заблокировано:

  , groupIsAdmin        :: !(Maybe Int)
                    -- ^ является ли текущий пользователь руководителем сообщества.
  , groupAdminLevel     :: !(Maybe GroupAdminLevel)
                       -- ^ полномочия текущего пользователя (если is_admin=1):
  , groypIsMember       :: !(Maybe Int)
                     -- ^ является ли текущий пользователь участником
                     -- сообщества (0 — не является, 1 — является).
  , groupType           :: GroupType
                 -- ^ тип сообщества
  , groupPhoto50        :: !T.Text
  , groupPhoto100       :: !T.Text
  , groupPhoto200       :: !T.Text
  , groupBanInfo        :: !(Maybe BanInfo)
                    -- ^ информация о занесении в черный список сообщества

  , groupCity           :: !(Maybe Int)
                 -- ^ идентификатор города, указанного в информации о сообществе
  , groupCountry        :: !(Maybe Int)
    -- ^ идентификатор страны, указанной в информации о сообществе
  , groupPlace          :: !(Maybe Place)
  , groupDescription    :: !(Maybe T.Text)
                        -- ^ текст описания сообщества.
  , groupWikiPage       :: !(Maybe T.Text)
                     -- ^ название главной вики-страницы сообщества.
  , groupMembersCount   :: !(Maybe Int)
                         -- ^ количество участников сообщества.
  , groupCounters       :: !(Maybe Counters)
                           -- ^ возвращается объект counters, содержащий счётчики сообщества
  , groupStartDate      :: !(Maybe POSIXTime)
  , groupFinishDate     :: !(Maybe POSIXTime)
                       -- ^ для встреч содержат время начала и окончания встречи
  , groupCanPost        :: !(Maybe Int)
                    -- ^ информация о том, может ли текущий пользователь оставлять
                    -- записи на стене сообщества
  , groupCanSeeAllPosts :: !(Maybe Int)
                         -- ^ информация о том, разрешено видеть чужие записи на стене группы
  , groupCanUploadDoc   :: !(Maybe Int)
                         -- ^ информация о том, может ли текущий
                         -- пользователь загружать документы в
                         -- группу.
  , groupCanUploadVideo :: !(Maybe Int)
                         -- ^ информация о том, может ли текущий
                         -- пользователь загружать видеозаписи в
                         -- группу
  , groypCanCreateTopic :: !(Maybe Int)
                         -- ^ информация о том, может ли текущий
                         -- пользователь создать тему обсуждения в
                         -- группе, используя метод
                         -- board.addTopic.
  , groupActivity       :: !(Maybe T.Text)
                     -- ^ строка состояния публичной страницы. У групп
                     -- возвращается строковое значение, открыта ли
                     -- группа или нет, а у событий дата начала.
  , groupStatus         :: !(Maybe T.Text)
                    -- ^ статус сообщества. Возвращается строка,
                    -- содержащая текст статуса, расположенного на
                    -- странице сообщества под его названием.
  , groupStatusAudio    :: !(Maybe T.Text)
  , groupContacts       :: !(Maybe T.Text)
                     -- ^ информация из блока контактов публичной страницы.
  , groupLinks          :: !(Maybe T.Text)
                  -- ^ информация из блока ссылок сообщества. Поле
                  -- возвращается только при условии, что запрос
                  -- выполняется для одного сообщества.
  , groupFixedPost      :: !(Maybe Int)
                      -- ^ идентификатор post_id закрепленного поста
                      -- сообщества. Сам пост можно получить,
                      -- используя wall.getById, передав в поле posts
                      -- – {group_id}_{post_id}.

  , groupVerified       :: !(Maybe Int)
                     -- ^ возвращает информацию о том, является ли
                     -- сообщество верифицированным.
  , groupSite           :: !(Maybe T.Text)
                 -- ^ адрес сайта из поля «веб-сайт» в описании сообщества.
  , groupMainAlbumId    :: !(Maybe Int)
                      -- ^ идентификатор основного альбома сообщества.
  , groupIsFavorite     :: !(Maybe Int)
                       -- ^ возвращается 1, если сообщество находится
                       -- в закладках у текущего пользователя.
  }
             deriving (Show, Generic)


instance FromJSON Group where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

