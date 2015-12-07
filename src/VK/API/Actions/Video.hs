{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
-- | Actions for <http://vk.com/dev/video.get Video API>


module VK.API.Actions.Video where


import qualified Data.Default.Generics  as D
import qualified Data.Text              as T


import           Network.API.Builder.TH (deriveQueryable', standard)


import           VK.API.Actions.Types
import           VK.API.CommonTypes
import           VK.Internal.Utils

data Get = Get {
  getOwnerId    :: !(Maybe OwnerId)
                 -- ^ идентификатор пользователя или сообщества,
                 -- которому принадлежат видеозаписи.
  , getVideos   :: !(Maybe [VideoId])
                 -- ^ перечисленные через запятую идентификаторы — идущие через
                 -- знак подчеркивания id пользователей, которым принадлежат
                 -- видеозаписи, и id самих видеозаписей. Если видеозапись
                 -- принадлежит сообществу, то в качестве первого параметра
                 -- используется -id сообщества.
  , getAlbumId  :: !(Maybe Int)
                   -- ^ идентификатор альбома, видеозаписи из которого нужно вернуть.
  , getCount    :: !(Maybe Int)
                -- ^ количество возвращаемых видеозаписей.

  , getOffset   :: !(Maybe Int)
                 -- ^ смещение относительно первой найденной видеозаписи для
                 -- выборки определенного подмножества.
  , getExtended :: !(Maybe AddExtended)
                   -- ^ определяет, возвращать ли информацию о настройках приватности
                   -- видео для текущего пользователя.
  }
           deriving Show

instance D.Default Get where
  def = Get
        Nothing Nothing Nothing
        Nothing Nothing Nothing

$(deriveQueryable' (standard . dropLPrefix) ''Get)

data Edit = Edit {
  editOwnerId          :: !OwnerId
  , editVideoId        :: !Int
  , editName           :: !(Maybe T.Text)
                -- ^ новое название для видеозаписи
  , editDesc           :: !(Maybe T.Text)
                -- ^ новое описание для видеозаписи
  , editPrivacyView    :: !(Maybe [Privacy])
                          -- ^ настройки приватности в формате настроек приватности
  , editPrivacyComment :: !(Maybe [Privacy])
                          -- ^ настройки приватности в формате настроек приватности
  , editNoComments     :: !(Maybe Int)
                      -- ^ закрыть комментарии для видео из сообществ.
  , editRepeat         :: !(Maybe Int)
                          -- ^ зацикливание воспроизведения
                          -- видеозаписи
  }
          deriving Show

instance D.Default Edit where
  def = Edit (UserId 0) 0
        Nothing Nothing Nothing
        Nothing Nothing Nothing

$(deriveQueryable' (standard . dropLPrefix) ''Edit)

data Add = Add {
  addTargetId  :: !OwnerId
                          -- ^ идентификатор пользователя или
                          -- сообщества, в которое нужно добавить
                          -- видео.
  , addOwnerId :: !OwnerId
  , addVideoId :: !Int
  }
         deriving Show

instance D.Default Add where
  def = Add (UserId 0) (UserId 0) 0

$(deriveQueryable' (standard . dropLPrefix) ''Add)

data Search = Search {
  searchQ          :: !T.Text
                       -- ^ строка поискового запроса. Например, The Beatles.
  , searchSort     :: !(Maybe VideoSort)
  , searchHd       :: !(Maybe QualityFilter)
  , searchAdult    :: !(Maybe AdultFilter)
                       -- ^ фильтр «Безопасный поиск» (1 — выключен, 0 — включен).
  , searchFilters  :: !(Maybe [CriteriaFilter])
                       -- ^ список критериев, по которым требуется отфильтровать видео:
  , searchOwn      :: !(Maybe OwnFilter)
  , searchOffset   :: !(Maybe Int)
                       -- ^ смещение относительно первой найденной видеозаписи
                       -- для выборки определенного подмножества.
  , searchLonger   :: !(Maybe Int)
                       -- ^ количество секунд, видеозаписи длиннее которого
                       -- необходимо вернуть.
  , searchShorter  :: !(Maybe Int)
                       -- ^ количество секунд, видеозаписи короче которого
                       -- необходимо вернуть.
  , searchCount    :: !(Maybe Int)
                       -- ^ количество возвращаемых видеозаписей.
  , searchExtended :: !(Maybe AddExtended)
  }
              deriving Show

instance D.Default Search where
  def = Search ""
        Nothing Nothing Nothing Nothing Nothing
        Nothing Nothing Nothing Nothing Nothing

$(deriveQueryable' (standard . dropLPrefix) ''Search)

data Save = Save {
  saveName             :: !(Maybe T.Text)
              -- ^ название видеофайла.
  , saveDescription    :: !(Maybe T.Text)
                           -- ^ описание видеофайла.
  , saveIsPrivate      :: !(Maybe Int)
                           -- ^ указывается 1 в случае последующей отправки видеозаписи личным
                           -- сообщением. После загрузки с этим параметром видеозапись не будет
                           -- отображаться в списке видеозаписей пользователя и не будет
                           -- доступна другим пользователям по id. По умолчанию 0.
  , saveWallpost       :: !(Maybe Int)
                  -- ^ требуется ли после сохранения опубликовать
                  -- запись с видео на стене
  , saveLink           :: !(Maybe T.Text)
              -- ^ url для встраивания видео с внешнего сайта, например, с
              -- youtube. В этом случае нужно вызвать полученный upload_url не
              -- прикрепляя файл, достаточно просто обратиться по этому адресу.
  , saveGroupId        :: !(Maybe Int)
                   -- ^ идентификатор сообщества, в которое будет сохранен
                   -- видеофайл. По умолчанию файл сохраняется на страницу текущего
                   -- пользователя.
  , saveAlbumId        :: !(Maybe Int)
                   -- ^ идентификатор альбома, в который будет загружен видео файл.
  , savePrivacyView    :: !(Maybe [Privacy])
                       -- ^ уровень доступа к просмотру видеозаписи в специальном формате.
  , savePrivacyComment :: !(Maybe [Privacy])
                          -- ^ уровень доступа к комментированию
                          -- видеозаписи в специальном формате.
  , saveNoComments     :: !(Maybe Int)
  , saveRepeat         :: !(Maybe Int)
                   -- ^ зацикливание воспроизведения видеозаписи.
  }
          deriving Show

instance D.Default Save where
  def = Save
        Nothing Nothing Nothing Nothing Nothing Nothing
        Nothing Nothing Nothing Nothing Nothing

$(deriveQueryable' (standard . dropLPrefix) ''Save)

data Delete = Delete {
  deleteTargetId  :: !OwnerId
  , deleteVideoId :: !Int
  , deleteOwnerId :: !(Maybe OwnerId)
  }
         deriving Show

instance D.Default Delete where
  def = Delete (UserId 0) 0 Nothing

$(deriveQueryable' (standard . dropLPrefix) ''Delete)

data Restore = Restore {
  restoreVideoId   :: !Int
  , restoreOwnerId :: !(Maybe OwnerId)
  }
         deriving Show

instance D.Default Restore where
  def = Restore 0 Nothing

$(deriveQueryable' (standard . dropLPrefix) ''Restore)

data UserVideos = UserVideos {
  uservideosUserId     :: !(Maybe Int)
                         -- ^ идентификатор пользователя
  , uservideosCount    :: !(Maybe Int)
                          -- ^ количество возвращаемых видеозаписей.
  , uservideosOffset   :: !(Maybe Int)
                          -- ^ смещение относительно первой найденной видеозаписи для
                          -- выборки определенного подмножества.
  , uservideosExtended :: !(Maybe AddExtended)
                          -- ^ определяет, возвращать ли информацию о настройках приватности
                          -- видео для текущего пользователя.
  }
           deriving Show

instance D.Default UserVideos where
  def = UserVideos
        Nothing Nothing Nothing Nothing

$(deriveQueryable' (standard . dropLPrefix) ''UserVideos)

data Albums = Albums {
  albumsOwnerId      :: !(Maybe OwnerId)
                   -- ^ идентификатор владельца альбомов (пользователь или
                   -- сообщество). По умолчанию — идентификатор текущего пользователя.
  , albumsOffset     :: !(Maybe Int)
                           -- ^ смещение, необходимое для выборки определенного подмножества
                           -- альбомов. По умолчанию — 0.
  , albumsExtended   :: !(Maybe Int)
                      -- ^ 1 – позволяет получать поля count, photo_320,
                      -- photo_160 и updated_time для каждого альбома. Если
                      -- альбом пустой, то поля photo_320 и photo_160
                      -- возвращены не будут. По умолчанию – 0.
  , albumsNeedSystem :: !(Maybe Int)
                        -- ^ 1 — возвращать системные альбомы.
  }
              deriving Show

instance D.Default Albums where
  def = Albums
        Nothing Nothing Nothing Nothing

$(deriveQueryable' (standard . dropLPrefix) ''Albums)

data GetAlbum = GetAlbum {
  getalbumOwnerId :: !(Maybe OwnerId)
                   -- ^ идентификатор владельца альбомов (пользователь или
                   -- сообщество). По умолчанию — идентификатор текущего пользователя.
  , getalbumId    :: !Int
  }
              deriving Show

instance D.Default GetAlbum where
  def = GetAlbum
        Nothing 0

$(deriveQueryable' (standard . dropLPrefix) ''GetAlbum)

data EditAlbum = EditAlbum {
  editalbumGroupId   :: !(Maybe Int)
  , editalbumAlbumId :: !Int
  , editalbumTitle   :: !(Maybe T.Text)
  , editalbumPrivacy :: !(Maybe [Privacy])
                          -- ^ настройки приватности в формате настроек приватности
  }
          deriving Show

instance D.Default EditAlbum where
  def = EditAlbum Nothing 0
        Nothing Nothing

$(deriveQueryable' (standard . dropLPrefix) ''EditAlbum)

data AddAlbum = AddAlbum {
  addalbumTitle     :: T.Text
  , addalbumGroupId :: !(Maybe Int)
  , addalbumPrivacy :: !(Maybe [Privacy])
                          -- ^ настройки приватности в формате настроек приватности
  }
          deriving Show

instance D.Default AddAlbum where
  def = AddAlbum "" Nothing Nothing

$(deriveQueryable' (standard . dropLPrefix) ''AddAlbum)

data DeleteAlbum = DeleteAlbum {
  deletealbumGroupId   :: !(Maybe Int)
  , deletealbumAlbumId :: !Int
  }
          deriving Show

instance D.Default DeleteAlbum where
  def = DeleteAlbum Nothing 0

$(deriveQueryable' (standard . dropLPrefix) ''DeleteAlbum)

data ReorderAlbums = ReorderAlbums {
  reorderalbumAlbumId  :: !Int
  , reorderalbumBefore :: !(Maybe Int)
                          -- ^ идентификатор альбома, перед которым нужно поместить текущий
  , reorderalbumAfter  :: !(Maybe Int)
                          -- ^ идентификатор альбома, после которого нужно поместить текущий
  }
          deriving Show

instance D.Default ReorderAlbums where
  def = ReorderAlbums 0 Nothing Nothing

$(deriveQueryable' (standard . dropLPrefix) ''ReorderAlbums)

data AddToAlbum = AddToAlbum {
  addtoalbumOwnerId    :: OwnerId
                 -- ^ идентификатор владельца видеозаписи.
  , addtoalbumVideoId  :: Int
               -- ^ идентификатор видеозаписи.
  , addtoalbumTargetId :: !(Maybe OwnerId)
              -- ^ идентификатор владельца альбома, в который нужно добавить видео.
  , addtoalbumAlbumId  :: !(Maybe Int)
                   -- ^ идентификатор альбома, в который нужно
                   -- добавить видео (0 соответствует общему списку
                   -- видеозаписей «без альбома»).
  , addtoalbumAlbumIds :: !(Maybe [Int])
                -- ^ идентификаторы альбомов, в которые нужно добавить видео.
  }
                deriving Show

instance D.Default AddToAlbum where
  def = AddToAlbum (UserId 0) 0 Nothing Nothing Nothing

$(deriveQueryable' (standard . dropLPrefix) ''AddToAlbum)

data RemoveFromAlbum = RemoveFromAlbum {
  removefromalbumOwnerId    :: OwnerId
  , removefromalbumVideoId  :: Int
  , removefromalbumTargetId :: !(Maybe OwnerId)
  , removefromalbumAlbumId  :: !(Maybe Int)
  , removefromalbumAlbumIds :: !(Maybe [Int])
  }
                     deriving Show

instance D.Default RemoveFromAlbum where
  def = RemoveFromAlbum (UserId 0) 0 Nothing Nothing Nothing

$(deriveQueryable' (standard . dropLPrefix) ''RemoveFromAlbum)

data AlbumsByVideo = AlbumsByVideo {
  albumsbyvideoOwnerId    :: OwnerId
  , albumsbyvideoVideoId  :: Int
  , albumsbyvideoTargetId :: !(Maybe OwnerId)
  , albumsbyvideoExtended :: !(Maybe Int)
  }
          deriving Show

instance D.Default AlbumsByVideo where
  def = AlbumsByVideo (UserId 0) 0 Nothing Nothing

$(deriveQueryable' (standard . dropLPrefix) ''AlbumsByVideo)

data Comments = Comments {
  commentsVideoId          :: !Int
  , commentsOwnerId        :: !(Maybe OwnerId)
  , commentsNeedLikes      :: !(Maybe Int)
                   -- ^ 1 — будет возвращено дополнительное поле
                   -- likes. По умолчанию поле likes не возвращается.
  , commentsStartCommentId :: !(Maybe Int)
                      -- ^ идентификатор комментария, начиная с которого
                      -- нужно вернуть список (подробности см. ниже).
  , commentsOffset         :: !(Maybe Int)
                      -- ^ смещение, необходимое для выборки определенного подмножества
                      -- комментариев. По умолчанию — 0.
  , commentsCount          :: !(Maybe Int)
                     -- ^ количество комментариев, информацию о которых необходимо
                     -- вернуть.
  , commentsSort           :: !(Maybe SortOrder)
                    -- ^ порядок сортировки комментариев (asc — от старых к новым,
                    -- desc — от новых к старым)
  , commentsExtended       :: !(Maybe Int)
                        -- ^ 1 — комментарии в ответе будут возвращены в виде
                        -- пронумерованных объектов, дополнительно будут
                        -- возвращены списки объектов profiles, groups.
  }
              deriving Show

instance D.Default Comments where
  def = Comments 0
        Nothing Nothing Nothing
        Nothing Nothing Nothing Nothing

$(deriveQueryable' (standard . dropLPrefix) ''Comments)

data CreateComment = CreateComment {
  createcommentVideoId          :: !Int
  , createcommentOwnerId        :: !(Maybe OwnerId)
  , createcommentMessage        :: !(Maybe T.Text)
  , createcommentAttachments    :: !(Maybe [Attachment])
  , createcommentFromGroup      :: !(Maybe Int)
  , createcommentReplyToComment :: !(Maybe Int)
  , createcommentStickerId      :: !(Maybe Int)
  }
                   deriving Show

instance D.Default CreateComment where
  def = CreateComment 0
        Nothing Nothing Nothing
        Nothing Nothing Nothing

$(deriveQueryable' (standard . dropLPrefix) ''CreateComment)

data DeleteComment = DeleteComment {
  deletecommentCommentId :: !Int
  , deletecommentOwnerId :: !(Maybe OwnerId)
  }
         deriving Show

instance D.Default DeleteComment where
  def = DeleteComment 0 Nothing

$(deriveQueryable' (standard . dropLPrefix) ''DeleteComment)

data RestoreComment = RestoreComment {
  restorecommentCommentId :: !Int
  , restorecommentOwnerId :: !(Maybe OwnerId)
  }
         deriving Show

instance D.Default RestoreComment where
  def = RestoreComment 0 Nothing

$(deriveQueryable' (standard . dropLPrefix) ''RestoreComment)

data EditComment = EditComment {
  editcommentCommentId     :: !Int
  , editcommentOwnerId     :: !(Maybe OwnerId)
  , editcommentMessage     :: !(Maybe T.Text)
  , editcommentAttachments :: !(Maybe [Attachment])
  }
                   deriving Show

instance D.Default EditComment where
  def = EditComment 0
        Nothing Nothing Nothing

$(deriveQueryable' (standard . dropLPrefix) ''EditComment)

data GetTags = GetTags {
  gettagsVideoId   :: Int
  , gettagsOwnerId :: !(Maybe OwnerId)
  }
               deriving Show

instance D.Default GetTags where
  def = GetTags 0 Nothing

$(deriveQueryable' (standard . dropLPrefix) ''GetTags)

data PutTag = PutTag {
  puttagVideoId      :: !Int
  , puttagUserId     :: !Int
  , puttagOwnerId    :: !(Maybe OwnerId)
  , puttagTaggedName :: !(Maybe T.Text)
  }
                   deriving Show

instance D.Default PutTag where
  def = PutTag 0 0
        Nothing Nothing

$(deriveQueryable' (standard . dropLPrefix) ''PutTag)

data RemoveTag = RemoveTag {
  removetagTagId     :: !Int
  , removetagVideoId :: !Int
  , removetagOwnerId :: !(Maybe OwnerId)
  }
                   deriving Show

instance D.Default RemoveTag where
  def = RemoveTag 0 0
        Nothing

$(deriveQueryable' (standard . dropLPrefix) ''RemoveTag)

data NewTags = NewTags {
  newtagsOffset  :: !(Maybe Int)
  , newtagsCount :: !(Maybe Int)
  }
                   deriving Show

instance D.Default NewTags where
  def = NewTags
        Nothing Nothing

$(deriveQueryable' (standard . dropLPrefix) ''NewTags)

data Report = Report {
  reportOwnerId       :: !OwnerId
  , reportVideoId     :: !Int
  , reportReason      :: !(Maybe ReportReason)
  , reportComment     :: !(Maybe T.Text)
  , reportSearchQuery ::  !(Maybe T.Text)
  }
              deriving Show

instance D.Default Report where
  def = Report (UserId 0) 0
        Nothing Nothing Nothing

$(deriveQueryable' (standard . dropLPrefix) ''Report)

data ReportComment = ReportComment {
  reportcommentOwnerId     :: !OwnerId
  , reportcommentCommentId :: !Int
  , reportcommentReason    :: !(Maybe ReportReason)
  }
              deriving Show

instance D.Default ReportComment where
  def = ReportComment (UserId 0) 0
        Nothing

$(deriveQueryable' (standard . dropLPrefix) ''ReportComment)
