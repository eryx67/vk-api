{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Common types for actions

module VK.API.Actions.Types where


import           Data.List           (stripPrefix)
import           Data.Maybe          (fromMaybe, maybeToList)
import qualified Data.Text           as T
import           GHC.Generics        (Generic)
import           Network.API.Builder


import           VK.API.CommonTypes  (OwnerId, ownerIdToInt)
import           VK.Internal.Utils

data SortOrder = Asc | Desc
               deriving (Show, Generic)

instance ToQuery SortOrder where
  toQuery n = toQuery n . T.toLower . T.pack . show

data VideoSort = VideoByDate
         | VideoBySize
         | VideoByDuration
         | VideoByRelevance
         deriving (Show, Eq, Enum, Bounded)

instance ToQuery VideoSort where
  toQuery n v = toQuery n (fromEnum v)

data QualityFilter = AnyQuality
             | HD
             deriving (Show, Eq, Enum, Bounded)
instance ToQuery QualityFilter where
  toQuery n v = toQuery n (fromEnum v)

data AdultFilter = NoAdult
                 | Adult
                 deriving (Show, Eq, Enum, Bounded)

instance ToQuery AdultFilter where
  toQuery n v = toQuery n (fromEnum v)

-- | 'ShortVideo' - возвращать только короткие видеозаписи
--   'LongVideo'    - возвращать только длинные видеозаписи
data CriteriaFilter = MP4
                    | Youtube
                    | Vimeo
                    | ShortVideo
                    | LongVideo
                    deriving (Show, Eq, Enum, Bounded)

instance ToQuery CriteriaFilter where
  toQuery n v = toQuery n (T.toLower . T.pack $ show v)

-- | 'AnyRecords' - не искать по записям пользователя (по умолчанию)
-- 'OwnRecords' - искать по записям пользователя
data OwnFilter = AnyRecords
               | OwnRecords
               deriving (Show, Eq, Enum, Bounded)

instance ToQuery OwnFilter where
  toQuery n v = toQuery n (fromEnum v)

-- | возвращать дополнительные объекты profiles и groups,
-- которые содержат id и имя/название владельцев видео.
data AddExtended = NoExtended
                 | AddExtended
                 deriving (Show, Eq, Enum, Bounded)

instance ToQuery AddExtended where
  toQuery n v = toQuery n (fromEnum v)

data VideoId = VideoId {
  videoidOwnerId     :: !OwnerId
  , videoidId        :: Int
  , videoidAccessKey :: !(Maybe T.Text)
  }
               deriving Show

instance ToQuery VideoId where
  toQuery n (VideoId oid vid ac) =
    toQuery n $
    T.intercalate "_" $
    map (T.pack . show) [ownerIdToInt oid, vid] ++ maybeToList ac

data AttachmentType = PhotoAttachment
                    | VideoAttachment
                    | AudioAttachment
                    | DocAttachment
                    deriving (Show, Enum)

data Attachment = Attachment {
  attachmentType      :: AttachmentType
  , attachmentOwnerId :: OwnerId
  , attachmentMediaId :: Int
  }
                  deriving Show

instance ToQuery Attachment where
  toQuery n (Attachment at oid mid) =
    let attxt = case T.stripSuffix "Attachment" $ tshow at of
          Nothing ->
            error "should never happen"
          Just v ->
            T.toLower v
    in
      toQuery n $
      T.intercalate "_" $
      [T.append attxt (tshow $ ownerIdToInt oid)
      , tshow mid
      ]

data ReportReason = Spam
                  | ChildPorno
                  | Extremism
                  | Violence
                  | DrugsPropaganda
                  | AdultMaterial
                  | Offence
                  deriving (Show, Eq, Enum, Bounded)

instance ToQuery ReportReason where
  toQuery n v = toQuery n (fromEnum v)

-- audio

data AudioSort = AudioByDate
         | AudioByDuration
         | AudioByPopularity
         deriving (Show, Eq, Enum, Bounded)

instance ToQuery AudioSort where
  toQuery n v = toQuery n (fromEnum v)

data BroadcastsFilter = FriendsBroadcasts
             | GroupBroadcasts
             | AllBroadcasts
             deriving (Show, Eq, Enum, Bounded)

instance ToQuery BroadcastsFilter where
  toQuery n v = toQuery n (T.toLower $ tshow v)

data TargetAudio = TargetAudio {
  targetaudioOwnerId   :: !OwnerId
  , targetaudioAudioId :: !Int
  }
                 deriving Show

instance ToQuery TargetAudio where
  toQuery n (TargetAudio oid aid) =
    toQuery n $
    T.intercalate "_" $ map (T.pack . show) [ownerIdToInt oid, aid]

data GetUserId = GetUserId !Int
               | GetUserName !T.Text
                 deriving Show

instance ToQuery GetUserId where
  toQuery n (GetUserId uid) =
    toQuery n $ tshow uid
  toQuery n (GetUserName un) =
    toQuery n un

-- users
-- | дополнительное поле профиля пользователя, которое необходимо вернуть
data UserField = UserFieldSex
                 | UserFieldBdate
                 | UserFieldCity
                 | UserFieldCountry
                 | UserFieldPhoto50
                 | UserFieldPhoto100
                 | UserFieldPhoto200Orig
                 | UserFieldPhoto200
                 | UserFieldPhoto400Orig
                 | UserFieldPhotoMax
                 | UserFieldPhotoMaxOrig
                 | UserFieldPhotoId
                 | UserFieldOnline
                 | UserFieldOnlineMobile
                 | UserFieldDomain
                 | UserFieldHasMobile
                 | UserFieldContacts
                 | UserFieldConnections
                 | UserFieldSite
                 | UserFieldEducation
                 | UserFieldUniversities
                 | UserFieldSchools
                 | UserFieldCanPost
                 | UserFieldCanSeeAllPosts
                 | UserFieldCanSeeAudio
                 | UserFieldCanWritePrivateMessage
                 | UserFieldStatus
                 | UserFieldLastSeen
                 | UserFieldCommonCount
                 | UserFieldRelation
                 | UserFieldRelatives
                 | UserFieldCounters
                 | UserFieldScreenName
                 | UserFieldMaidenName
                 | UserFieldTimezone
                 | UserFieldOccupation
                 | UserFieldActivities
                 | UserFieldInterests
                 | UserFieldMusic
                 | UserFieldMovies
                 | UserFieldTv
                 | UserFieldBooks
                 | UserFieldGames
                 | UserFieldAbout
                 | UserFieldQuotes
                 | UserFieldPersonal
                 | UserFieldFriend_status
                 | UserFieldMilitary
                 | UserFieldCareer
                   deriving Show

instance ToQuery UserField where
  toQuery n v = toQuery n qval
    where
      qval = T.pack $ aesonPhotoCase (fromMaybe vstr $ stripPrefix "UserField" vstr)
      vstr = show v

-- | падеж для склонения имени и фамилии пользователя
data NameCase = NameCaseNom -- ^ именительный
              | NameCaseGen -- ^ родительный
              | NameCaseDat -- ^ дательный
              | NameCaseAcc -- ^ винительный
              | NameCaseIns -- ^ творительный
              | NameCaseAbl -- ^ предложный
                deriving Show

instance ToQuery NameCase where
  toQuery n v = toQuery n qval
    where
      qval =  T.toLower . T.pack $  (fromMaybe vstr $ stripPrefix "NameCase" vstr)
      vstr = show v

data UsersSort = UsersByPopularity
         | UsersByRegistrationDate
         deriving (Show, Eq, Enum, Bounded)

instance ToQuery UsersSort where
  toQuery n v = toQuery n (fromEnum v)

data UserSearchSection = UserFriends
         | UserSubscriptions
         deriving (Show, Eq, Enum, Bounded)

instance ToQuery UserSearchSection where
  toQuery n v = toQuery n qval
    where
      qval =  T.toLower . T.pack $  (fromMaybe vstr $ stripPrefix "User" vstr)
      vstr = show v

data NearByRadius = NearByRadiusUnknown
                  | M300  -- ^ 1 — 300 метров;
                  | M2400 -- ^ 2 — 2400 метров;
                  | KM18  -- ^ 3 — 18 километров;
                  | KM150 -- ^ 4 — 150 километров.
         deriving (Show, Eq, Enum, Bounded)

instance ToQuery NearByRadius where
  toQuery n v = toQuery n (fromEnum v)
