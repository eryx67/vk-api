{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
-- | Actions for <http://vk.com/dev/users.get Users API>


module VK.API.Actions.Users where

import qualified Data.Default.Generics  as D
import qualified Data.Text              as T


import           Network.API.Builder.TH (deriveQueryable', standard)

import           VK.API.Actions.Types
import           VK.API.CommonTypes
import           VK.Internal.Orphans    ()
import           VK.Internal.Utils


data Get = Get {
  getUserIds    :: !(Maybe [GetUserId])
  , getFields   :: !(Maybe [UserField])
  , getNameCase :: !(Maybe NameCase)
  }
           deriving Show

instance D.Default Get where
  def = Get Nothing Nothing Nothing

$(deriveQueryable' (standard . dropLPrefix) ''Get)

data Search = Search {
  searchQ                   :: !(Maybe T.Text)
  , searchSort              :: !(Maybe UsersSort)
  , searchOffset            :: !(Maybe Int)
  , searchCount             :: !(Maybe Int)
  , searchFields            :: !(Maybe [UserField])

  , searchCity              :: !(Maybe Int)
            -- ^ идентификатор города.
  , searchCountry           :: !(Maybe Int)
               -- ^ идентификатор страны.
  , searchHometown          :: !(Maybe T.Text)
                -- ^ название города строкой.
  , searchUniversityCountry :: !(Maybe Int)
                         -- ^ идентификатор страны, в которой пользователи закончили ВУЗ
  , searchUniversity        :: !(Maybe Int)
                  -- ^ идентификатор ВУЗа.
  , searchUniversityYear    :: !(Maybe Int)
                      -- ^ год окончания ВУЗа.
  , searchUniversityFaculty :: !(Maybe Int)
                         -- ^ идентификатор факультета.
  , searchUniversityChair   :: !(Maybe Int)
                       -- ^ идентификатор кафедры.
  , searchSex               :: !(Maybe Sex)
  , searchStatus            :: !(Maybe UserRelation)
  , searchAgeFrom           :: !(Maybe Int)
               -- ^ начиная с какого возраста.
  , searchAgeTo             :: !(Maybe Int)
             -- ^ до какого возраста.
  , searchBirthDay          :: !(Maybe Int)
                -- ^ день рождения.
  , searchBirthMonth        :: !(Maybe Int)
                  -- ^ месяц рождения.
  , searchBirthYear         :: !(Maybe Int)
                 -- ^ год рождения.
  , searchOnline            :: !(Maybe Int)
              -- ^ 1 — только в сети, 0 — все пользователи.
  , searchHasPhoto          :: !(Maybe Int)
                -- ^ 1 — только с фотографией, 0 — все пользователи.
  , searchSchoolCountry     :: !(Maybe Int)
                     -- ^ идентификатор страны, в которой пользователи закончили школу.
  , searchSchoolCity        :: !(Maybe Int)
                  -- ^ идентификатор города, в котором пользователи закончили школу.
  , searchSchoolClass       :: !(Maybe Int)
  , searchSchool            :: !(Maybe Int)
              -- ^ идентификатор школы, которую закончили пользователи.
  , searchSchoolYear        :: !(Maybe Int)
                  -- ^ год окончания школы.
  , searchReligion          :: !(Maybe T.Text)
                -- ^ религиозные взгляды.
  , searchInterests         :: !(Maybe T.Text)
                 -- ^ интересы.
  , searchCompany           :: !(Maybe T.Text)
               -- ^ название компании, в которой работают пользователи.
  , searchPosition          :: !(Maybe T.Text)
                -- ^ название должности.
  , searchGroupId           :: !(Maybe Int)
               -- ^ идентификатор группы, среди пользователей которой
               -- необходимо проводить поиск.
  , searchFromList          :: !(Maybe [UserSearchSection])
  }
             deriving Show

instance D.Default Search where
  def = Search
        Nothing Nothing Nothing Nothing Nothing Nothing
        Nothing Nothing Nothing Nothing Nothing Nothing
        Nothing Nothing Nothing Nothing Nothing Nothing
        Nothing Nothing Nothing Nothing Nothing Nothing
        Nothing Nothing Nothing Nothing Nothing Nothing
        Nothing Nothing Nothing

$(deriveQueryable' (standard . dropLPrefix) ''Search)

data IsAppUser = IsAppUser {
  isappuserUserId :: Int
  }
                 deriving Show

instance D.Default IsAppUser where
  def = IsAppUser 0

$(deriveQueryable' (standard . dropLPrefix) ''IsAppUser)

data Subscriptions = Subscriptions {
  subscriptionsUserId   :: !(Maybe Int)
  -- , subscriptionsExtended :: !(Maybe AddExtended)
  , subscriptionsOffset :: !(Maybe Int)
  , subscriptionsCount  :: !(Maybe Int)
  , subscriptionsFields :: !(Maybe [UserField])
  }
              deriving Show

instance D.Default Subscriptions where
  def = Subscriptions Nothing Nothing Nothing Nothing

$(deriveQueryable' (standard . dropLPrefix) ''Subscriptions)

data Followers = Followers {
  followersUserId     :: !(Maybe Int)
  , followersOffset   :: !(Maybe Int)
  , followersCount    :: !(Maybe Int)
  , followersFields   :: !(Maybe [UserField])
  , followersNameCase :: !(Maybe [NameCase])

  }
              deriving Show

instance D.Default Followers where
  def = Followers Nothing Nothing Nothing Nothing Nothing

$(deriveQueryable' (standard . dropLPrefix) ''Followers)

data Report = Report {
  reportOwnerId   :: !Int
  , reportReason  :: !ReportReason
  , reportComment :: !(Maybe T.Text)
  }
              deriving Show

instance D.Default Report where
  def = Report 0 Spam
        Nothing

$(deriveQueryable' (standard . dropLPrefix) ''Report)

data NearBy = NearBy {
  nearbyLatitude    :: !Double
                    -- ^ географическая широта точки, в которой в данный
                    -- момент находится пользователь, заданная в градусах
                    -- (от -90 до 90).
  , nearbyLongitude :: !Double
                       -- ^ географическая долгота точки, в которой в данный
                       -- момент находится пользователь, заданная в градусах
                       -- (от -180 до 180).
  , nearbyAccuracy  :: !(Maybe Int)
                      -- ^ точность текущего местоположения пользователя в метрах.
  , nearbyTimeout   :: !(Maybe Int)
                     -- ^ время в секундах через которое пользователь должен
                     -- перестать находиться через поиск по местоположению.
  , nearbyRadius    :: !(Maybe NearByRadius)
                    -- ^ тип радиуса зоны поиска (от 1 до 4)
  , nearbyFields    :: !(Maybe [UserField])
  , nearbyNameCase  :: !(Maybe NameCase)
  }
              deriving Show

instance D.Default NearBy where
  def = NearBy 0 0
        Nothing Nothing Nothing Nothing Nothing

$(deriveQueryable' (standard . dropLPrefix) ''NearBy)
