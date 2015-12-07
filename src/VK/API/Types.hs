{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric     #-}
-- |

module VK.API.Types where

import           Data.Aeson
import           Data.Aeson.Casing
import qualified Data.Default.Generics as D
import qualified Data.Text as T
import           GHC.Generics (Generic)
-- for UTCTime Show
import           Data.Time ()
import           Data.Time.Clock (UTCTime)
import           Network.API.Builder

#ifdef __GHCJS__
#else

import           Network.HTTP.Client (Manager)

#endif

import           VK.API.Errors

-- | Type for VKontakte API
type VKAPI a r = API (VKApp a) VKError r


-- | Internal type for VKontakte application
data VKApp a = VKApp {
  _vkSettings    :: !VKSettings
  , _vkApp       :: !a
  , _vkManager   :: !Manager
  , _vkAuthToken :: !(Maybe AuthToken)
                   -- ^ temporary token returned on authorization stage
  }

-- | Authorization settings for VKontakte application
data VKSettings = VKSettings {
  vkAppId      :: !Integer
                   -- ^ application client id, as registered on VKontakte
  , vkUser     :: !AuthUser
                   -- ^ VKontakte user
  , vkAuthArgs :: !AuthArgs
                  -- ^ configuration for authorization
  , vkHttps    :: !Bool
               -- ^ return https links
  , vkTestMode :: !Bool
                  -- ^ allows to use API without making application
                  -- globally visible
  , vkLang     :: !(Maybe T.Text)
  }
           deriving Show

data AuthPermissions =
  Notify
  -- ^ (+1) Пользователь разрешил отправлять ему уведомления (для flash/ifrаme-приложений).
  | Friends
    -- ^ (+2) 	Доступ к друзьям.
  | Photos
    -- ^ (+4) 	Доступ к фотографиям.
  | Audio
    -- ^ (+8) 	Доступ к аудиозаписям.
  | Video
    -- ^ (+16) 	Доступ к видеозаписям.
  | Docs
    -- ^ (+131072) 	Доступ к документам.
  | Notes
    -- ^ (+2048) 	Доступ к заметкам пользователя.
  | Pages
    -- ^ (+128) 	Доступ к wiki-страницам.
    -- +256 	Добавление ссылки на приложение в меню слева.
  | Status
    -- ^ (+1024) 	Доступ к статусу пользователя.
  | Offers
    -- ^ (+32) 	Доступ к предложениям (устаревшие методы).
  | Questions
    -- ^ (+64) 	Доступ к вопросам (устаревшие методы).
  | Wall
    -- ^ (+8192) Доступ к обычным и расширенным методам работы со
    -- стеной.  Внимание, данное право доступа недоступно для сайтов
    -- (игнорируется при попытке авторизации).
  | Groups
    -- ^ (+262144) 	Доступ к группам пользователя.
  | Messages
    -- ^ (+4096) (для Standalone-приложений) Доступ к расширенным
    -- методам работы с сообщениями.
  | Email
    -- ^ (+4194304) 	Доступ к email пользователя.
  | Notifications
    -- ^ (+524288) 	Доступ к оповещениям об ответах пользователю.
  | Stats
    -- ^ (+1048576) Доступ к статистике групп и приложений
    -- пользователя, администратором которых он является.
  | Ads
    -- ^ (+32768) Доступ к расширенным методам работы с рекламным API.
  | Offline
    -- ^ (+65536) Доступ к API в любое время со стороннего сервера
    -- (при использовании этой опции параметр expires_in, возвращаемый
    -- вместе с access_token, содержит 0 — токен бессрочный).
  | Nohttps
    -- ^ Возможность осуществлять запросы к API без HTTPS.
    deriving (Show, Eq, Enum, Bounded)

data AuthDisplay = Page
                 | Popup
                 | Mobile
                 deriving (Show, Eq, Enum, Bounded)

data AuthResponseType = Code
                        -- ^ если Вы хотите делать запросы со
                        -- стороннего сервера (по умолчанию)
                      | Token
                        -- ^ если Вы хотите делать запросы с клиента
                      deriving (Show, Eq, Enum, Bounded)

data AuthArgs = AuthArgs {authDisplay       :: AuthDisplay
                         , authRedirectURI  :: T.Text
                         , authResponseType :: AuthResponseType
                         , authApiVersion   :: T.Text
                         , authScope        :: Maybe [AuthPermissions]
                         , authState        :: Maybe T.Text
                         }
                deriving Show

instance D.Default AuthArgs where
  def = AuthArgs Mobile "https://oauth.vk.com/blank.html"
        Token "5.37" Nothing Nothing

data AuthUser = AuthUser {
  authUsername   :: !T.Text
  , authPassword :: !T.Text
  }
              deriving Show

data AuthToken = AuthToken {
  authToken      :: !T.Text
  , authUserId   :: !Int
  , authExpireAt :: !UTCTime
  }
               deriving (Show, Generic)

instance FromJSON AuthToken where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON AuthToken where
  toJSON = genericToJSON $ aesonPrefix snakeCase
