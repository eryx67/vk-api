{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- |

module VK.API.Models.Types.User where

import           Data.Aeson
import           Data.Aeson.Casing
import qualified Data.Text                    as T
import qualified Data.Text.Read               as TR
import           Data.Time.Clock.POSIX        (POSIXTime)
import           GHC.Generics                 (Generic)


import           VK.API.CommonTypes
import           VK.API.Models.Types.Counters
import           VK.Internal.Utils


data City = City {
  cityId      :: Int
  , cityTitle :: T.Text
  }
               deriving (Show, Generic)

instance FromJSON City where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Country = Country {
  countryId      :: Int
  , countryTitle :: T.Text
  }
               deriving (Show, Generic)

instance FromJSON Country where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Contacts = Contacts {
  contactsMobilePhone :: !(Maybe T.Text)
  , contactsHomePhone :: !(Maybe T.Text)
  }
               deriving (Show, Generic)

instance FromJSON Contacts where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Education = Education {
  educationUniversity       :: !(Maybe Int)
  , educationUniversityName :: !(Maybe T.Text)
  , educationFaculty        :: !(Maybe Int)
  , educationFacultyName    :: !(Maybe T.Text)
  , educationGraduation     :: !(Maybe Int)
  }
               deriving (Show, Generic)

instance FromJSON Education where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data University = University {
  universityId            :: !Int
  , universityCountry     :: !Int
  , universityCity        :: !Int
  , universityName        :: !T.Text
  , universityFaculty     :: !Int
  , universityFacultyName :: !T.Text
  , universityChair       :: !Int
  , universityChairName   :: !T.Text
  , universityGraduation  :: !Int
  }
               deriving (Show, Generic)

instance FromJSON University where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data School = School {
  schoolId              :: !Int
  , schoolCountry       :: !Int
  , schoolCity          :: !Int
  , schoolName          :: !T.Text
  , schoolYearFrom      :: !Int
  , schoolYearTo        :: !Int
  , schoolYearGraduated :: !Int
  , schoolClass         :: !T.Text
  , schoolSpeciality    :: !T.Text
  , schoolType          :: !Int
  , schoolTypeStr       :: !T.Text
  }
               deriving (Show, Generic)

instance FromJSON School where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data LastSeen = LastSeen {
  lastseenTime       :: !POSIXTime
  , lastseenPlatform :: !Int
  }
              deriving (Show, Generic)

instance FromJSON LastSeen where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data OccupationType = OccupationWork
                    | OccupationSchool
                    | OccupationUniversity
                    deriving (Show, Generic)

instance FromJSON OccupationType where
  parseJSON =
    withText "OccupationType"
    (\case
         "work" -> pure OccupationWork
         "school" -> pure OccupationSchool
         "university" -> pure OccupationUniversity
         _ -> mempty
    )

data Occupation = Occupation {
  occupationId     :: !Int
  , occupationName :: !T.Text
  , occupationType :: !OccupationType
  }
               deriving (Show, Generic)

instance FromJSON Occupation where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data UserRelative = UserRelative {
  userrelativeId     :: !(Maybe Int)
  , userrelativeName :: !(Maybe T.Text)
  , userrelativeType :: !(Maybe T.Text)
  }
                  deriving (Show, Generic)

instance FromJSON UserRelative where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data UserRelationPartner = UserRelationPartner {
  userrelationpartnerId     :: !Int
  , userrelationpartnerName :: !T.Text
  }
                         deriving (Show, Generic)

instance FromJSON UserRelationPartner where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Personal = Personal {
  personalPolitical    :: !Political
  , personalLangs      :: ![T.Text]
  , personalReligion   :: !T.Text
  , personalInspiredBy :: !T.Text
  , personalPeopleMain :: !PeopleMain
  , personalLifeMain   :: !LifeMain
  , personalSmoking    ::  !Attitude
  , personalAlcohol    :: !Attitude
  }
              deriving (Show, Generic)

instance FromJSON Personal where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Political = PoliticalUnknown
               | Communist
               | Socialist
               | Moderate
               | Liberal
               | Conservative
               | Monarchical
               | Ultraconservative
               | Indifferent
               | Libertarian
               deriving (Show, Generic, Enum, Bounded)

instance FromJSON Political where
  parseJSON =
    intEnumFromJSON "Political" ([minBound ..]::[Political])

data PeopleMain = PeopleMainUnknown
                | MindAndCreativity
                | KindnessAndHonesty
                | Beauty
                | PowerAndWealth
                | CourageAndPerseverance
                | HumorAndLoveForLife
               deriving (Show, Generic, Enum, Bounded)

instance FromJSON PeopleMain where
  parseJSON =
    intEnumFromJSON "PeopleMain" ([minBound ..]::[PeopleMain])

data LifeMain = LifeMainUnknown
     | FamilyAndChildren
     | CareerAndMoney
     | EntertainmentAndLeisure
     | ScienceAndResearch
     | ImprovementOfTheWorld
     | SelfDevelopment
     | BeautyAndArt
     | FameAndInfluence
     deriving (Show, Generic, Enum, Bounded)

instance FromJSON LifeMain where
  parseJSON =
    intEnumFromJSON "LifeMain" ([minBound ..]::[LifeMain])

data Attitude = AttitudeUnknown
      | VeryNegative
      | Negative
      | Neutral
      | Compromise
      | Positive
     deriving (Show, Generic, Enum, Bounded)

instance FromJSON Attitude where
  parseJSON =
    intEnumFromJSON "Attitude" ([minBound ..]::[Attitude])

-- | вырезанная фотография пользователя, поля: x, y, x2, y2, координаты указаны в процентах.
data Crop = Crop {
  x    :: !Int
  , y  :: !Int
  , x2 :: !Int
  , y2 :: !Int
  }
          deriving (Show, Generic)

instance FromJSON Crop where

data CropPhoto = CropPhoto {
  cropphotoPhoto  :: !T.Text
           -- ^ объект photo фотографии пользователя из которой
           -- вырезается профильная аватарка.
  , cropphotoCrop :: !Crop
  , cropphotoRect :: !Crop
                     -- ^ миниатюрная квадратная фотография,
                     -- вырезанная из фотографии crop
  }
               deriving (Show, Generic)

instance FromJSON CropPhoto where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data FriendStatus = NotFriend
                    -- ^ пользователь не является другом,
                  | SubscriptionSent
                    -- ^ отправлена заявка/подписка пользователю,
                  | SubscriptionExists
                    -- ^ имеется входящая заявка/подписка от пользователя,
                  | IsFriend
                    -- ^ пользователь является другом;
                  deriving (Show, Generic, Enum, Bounded)

instance FromJSON FriendStatus where
  parseJSON =
    intEnumFromJSON "FriendStatus" ([minBound ..]::[FriendStatus])

data Career = Career {
  careerGroupId     :: !(Maybe Int)
             -- ^ идентификатор сообщества (если доступно, иначе company);
  , careerCompany   :: !(Maybe T.Text)
               -- ^ название организации (если доступно, иначе group_id);
  , careerCountryId :: !(Maybe Int)
                 -- ^ идентификатор страны;
  , careerCityId    :: !(Maybe Int)
              -- ^ идентификатор города (если доступно, иначе city_name);
  , careerCityName  :: !(Maybe T.Text)
                -- ^ название города (если доступно, иначе city_id);
  , careerFrom      :: !(Maybe Int)
            -- ^ год начала работы;
  , careerUntil     :: !(Maybe Int)
             -- ^ год окончания работы;
  , careerPosition  :: !(Maybe T.Text)
                      -- ^ должность
  }
            deriving (Show, Generic)

instance FromJSON Career where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Military = Military {
  militaryUnitId      :: !(Maybe Int)
  , militaryUnit      :: !(Maybe T.Text)
  , militaryCountryId :: !(Maybe Int)
  , militaryFrom      :: !(Maybe Int)
  , militaryUntil     :: !(Maybe Int)
  }
              deriving (Show, Generic)

instance FromJSON Military where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data UserPhotoId = UserPhotoId {
  userphotoUserId    :: Int
  , userphotoPhotoId :: Int
  }
                   deriving Show

instance FromJSON UserPhotoId where
  parseJSON =
    withText "UserPhotoId" parsePhid
    where
      parsePhid v =
        either (const mempty) pure $ do
          let vs = T.splitOn "_" v

          case vs of
            [uidTxt, phidTxt] -> do
              uidv <- TR.decimal uidTxt
              phidv <- TR.decimal phidTxt
              case (uidv, phidv) of
                ((uid, ""), (phid, "")) ->
                  return $ UserPhotoId uid phid
                _ ->
                  fail "bad value"
            _ ->
              fail "bad value"

data User = User {
  userId                       :: !Int
  , userFirstName              :: !T.Text
  , userLastName               :: !T.Text
  , userDeactivated            :: !(Maybe Int)
  , userHidden                 :: !(Maybe Int)
  , userPhotoId                :: !(Maybe UserPhotoId)
  , userVerified               :: !(Maybe Int)
  , userBlacklisted            :: !(Maybe Int)
  , userSex                    :: !(Maybe Sex)
  , userBdate                  :: !(Maybe T.Text)
  , userCity                   :: !(Maybe City)
  , userCountry                :: !(Maybe Country)
  , userHomeTown               :: !(Maybe T.Text)
  , userPhoto50                :: !(Maybe T.Text)
  , userPhoto100               :: !(Maybe T.Text)
  , userPhoto200               :: !(Maybe T.Text)
  , userPhoto200Orig           :: !(Maybe T.Text)
  , userPhoto400               :: !(Maybe T.Text)
  , userPhoto400Orig           :: !(Maybe T.Text)
  , userPhotoMax               :: !(Maybe T.Text)
  , userPhotoMaxOrig           :: !(Maybe T.Text)
  , userOnline                 :: !(Maybe Int)
  , userLists                  :: !(Maybe [Int])
  , userDomain                 :: !(Maybe T.Text)
  , userHasMobile              :: !(Maybe Int)
  , userContacts               :: !(Maybe Contacts)
  , userSite                   :: !(Maybe T.Text)
  , userEducation              :: !(Maybe Education)
  , userUniversities           :: !(Maybe [University])
  , userSchools                :: !(Maybe [School])
  , userStatus                 :: !(Maybe T.Text)
  , userStatusAudio            :: !(Maybe T.Text)
  , userLastSeen               :: !(Maybe LastSeen)
  , userFollowersCount         :: !(Maybe Int)
  , userCommonCount            :: !(Maybe Int)
  , userCounters               :: !(Maybe Counters)
  , userOccupation             :: !(Maybe Occupation)
  , userNickname               :: !(Maybe T.Text)
  , userRelatives              :: !(Maybe [UserRelative])
  , userRelation               :: !(Maybe UserRelation)
  , userRelationPartner        :: !(Maybe UserRelationPartner)
  , userPersonal               :: !(Maybe Personal)

  , userConnections            :: !(Maybe T.Text)
                       -- ^ возвращает данные об указанных в профиле сервисах
                       -- пользователя, таких как: skype, facebook, twitter, livejournal,
                       -- instagram.
  , userExports                :: !(Maybe T.Text)
                   -- ^ внешние сервисы, в которые настроен экспорт из ВК (twitter,
                   -- facebook, livejournal, instagram).
  , userWallComments           :: !(Maybe Int)
                        -- ^ доступно ли комментирование стены (1 — доступно, 0 — недоступно).
  , userActivities             :: !(Maybe T.Text)
                                  -- ^ деятельность.
  , userInterests              :: !(Maybe T.Text)
                                  -- ^ интересы.
  , userMusic                  :: !(Maybe T.Text)
                                  -- ^ любимая музыка.
  , userMovies                 :: !(Maybe T.Text)
                                  -- ^ любимые фильмы.
  , userTv                     :: !(Maybe T.Text)
                                  -- ^ любимые телешоу.
  , userBooks                  :: !(Maybe T.Text)
                                  -- ^ любимые книги.
  , userGames                  :: !(Maybe T.Text)
                                  -- ^ любимые игры.
  , userAbout                  :: !(Maybe T.Text)
                                  -- ^ «О себе».
  , userQuotes                 :: !(Maybe T.Text)
                                  -- ^ любимые цитаты.
  , userCanPost                :: !(Maybe Int)
                   -- ^ информация о том, разрешено ли оставлять записи на стене у
                   -- пользователя. Возвращаемые значения: 1 —разрешено, 0 — не
                   -- разрешено.
  , userCanSeeAllPosts         :: !(Maybe Int)
                          -- ^ информация о том, разрешено ли видеть
                          -- чужие записи на стене
                          -- пользователя. Возвращаемые значения: 1
                          -- —разрешено, 0 — не разрешено.
  , userCanSeeAudio            :: !(Maybe Int)
                       -- ^ информация о том, разрешено ли видеть
                       -- чужие аудиозаписи на стене
                       -- пользователя. Возвращаемые значения: 1
                       -- —разрешено, 0 — не разрешено.
  , userCanWritePrivateMessage :: !(Maybe Int)
                                  -- ^ информация о том, разрешено ли
                                  -- написание личных сообщений
                                  -- данному
                                  -- пользователю. Возвращаемые
                                  -- значения: 1 —разрешено, 0 — не
                                  -- разрешено.
  , userCanSendFriendRequest   :: !(Maybe Int)
                                -- ^ информация о том, будет ли
                                -- отправлено уведомление пользователю
                                -- о заявке в друзья. Возвращаемые
                                -- значения: 1 — уведомление будет
                                -- отправлено, 0 — уведомление не
                                -- будет оптравлено.
  , userIsFavorite             :: !(Maybe Int)
                      -- ^ возвращается 1, если пользователь находится
                      -- в закладках у текущего пользователя.
  , userTimezone               :: !(Maybe Int)
                    -- ^ временная зона пользователя. Возвращается только при запросе
                    -- информации о текущем пользователе.
  , userScreenName             :: !(Maybe T.Text)
                      -- ^ короткое имя (поддомен) страницы пользователя.
  , userMaidenName             :: !(Maybe T.Text)
                      -- ^ девичья фамилия.
  , userCropPhoto              :: !(Maybe CropPhoto)
                     -- ^ возвращает данные о точках, по которым
                     -- вырезаны профильная и миниатюрная фотографии
                     -- пользователя.

  , userIsFriend               :: !(Maybe Int)
                           -- ^ 1 – пользователь друг, 2 – пользователь не в друзьях.
  , userFriendStatus           :: !(Maybe FriendStatus)
                        -- ^ статус дружбы с пользователем:

  , userCareer                 :: !(Maybe Career)
                  -- ^ информация о карьере пользователя.
  , userMilitary               :: !(Maybe Military)
                    -- ^ информация о военной службе пользователя.
  }
             deriving (Show, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON $ aesonPrefix aesonPhotoCase
