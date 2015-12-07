{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- | Bindings for <https://vk.com/dev VKontakte>
--
-- Prepare our own state for application:
--
-- > data AppState = AppState [T.Text]
-- >
-- > appState = AppState []
--
-- * Authentication and authorization
--
-- Prepare settings for authorization:
--
-- >userName :: T.Text
-- >userName = "myname@gmail.com"
-- >
-- >userPass :: T.Text
-- >userPass = "mypass"
-- >
-- >appId :: Integer
-- >appId = 1234567
-- >
-- >appScope :: [AuthPermissions]
-- >appScope = [Audio, Video]
-- >
-- >vksettings :: VKSettings
-- >vksettings = createSettings appId userName userPass (Just appScope)
--
-- Authorize and get user's VKontakte id now:
--
-- >execVKAPI appState vksettings $ do
-- >liftState $ gets getUserId
--
-- * Usage
--
-- __Actions(queries)__ are defined in:
--
-- * "VK.API.Actions.Audio"
-- * "VK.API.Actions.Video"
-- * "VK.API.Actions.Users"
--
-- Every action has reasonable default 'Data.Default.Generics.def' instance.
--
-- __Models(query results)__ are defined in:
--
-- * "VK.API.Models.Audio"
-- * "VK.API.Models.Video"
-- * "VK.API.Models.Users"
--
-- __Mappings between actions and models__
--
-- are defined in "VK.API.Routes".
--
-- Actions are mapped to results with 'Routable' class.
-- To execute action in 'VKAPI' you need to call 'toAPI' on it.
--
-- >execVKAPI appState vksettings $ do
-- >uid <- liftState $ gets getUserId
-- >-- get first 10 from my own audio records
-- >toAPI $ D.def{A.getOwnerId = Just $ UserId uid
-- >              , A.getCount = Just 10
-- > }
--
module VK.API (execVKAPI
              , startVKAPI
              , runVKAPI
              , vkGetApp
              , vkSetApp
              , apiBaseURL
              , createSettings
              , isAuthorized
              , setAuthToken
              , getAuthToken
              , getUserId
              , authorizeVKAppFromURL
              , module VK) where


import           Control.Monad (unless)
import           Control.Monad.Trans.State.Lazy (gets, modify)
import qualified Data.Default.Generics as D
import           Data.Maybe (isJust)
import qualified Data.Text as T
import           Network.API.Builder

#ifdef __GHCJS__

import           JavaScript.Web.XMLHttpRequest

#else

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

#endif

import           VK.API.Auth as VK
import           VK.API.CommonTypes as VK
import           VK.API.Errors as VK
import           VK.API.Models.Types as VK
import           VK.API.Routable as VK
import           VK.API.Routes ()
import           VK.API.Types as VK

-- | Base URL for VKontakte API.
apiBaseURL :: T.Text
apiBaseURL = "https://api.vk.com"

-- | Create application settings with required parameters
createSettings :: Integer -> T.Text -> T.Text -> Maybe [AuthPermissions]
               -> VKSettings
createSettings appId userName userPass scope =
  VKSettings appId (AuthUser userName userPass) (D.def{authScope = scope})
  False False Nothing

-- | Creates connection to VKontakte API, authorizes, executes actions
-- and closes connection.
execVKAPI :: app -> VKSettings -> VKAPI app r -> IO (Either (APIError VKError) r)
execVKAPI app settings action = do
  (res, _) <- startVKAPI app settings action
  return res

-- | Creates connection to VKontakte API, authorizes and executes
-- actions. Returns a result and internal state for subsequent
-- calls.
startVKAPI :: app -> VKSettings -> VKAPI app r -> IO (Either (APIError VKError) r, VKApp app)
startVKAPI app settings action = do
  mgr <- newManager tlsManagerSettings
  runVKAPI (VKApp settings app mgr Nothing) action

-- | Authorize from existing VKontakte answered url. Useful for
-- browser applications only. Returns the state with filled
-- authorization token.
authorizeVKAppFromURL :: VKApp app -> T.Text -> IO (Either (APIError VKError) (VKApp app))
authorizeVKAppFromURL vkapp aurl = do
  at <- VK.authorizeFromURL $ T.unpack aurl
  return $ (setAuthToken vkapp) <$> at

-- | Use this function to execute actions with VKontakte API following the 'startVKAPI'.
-- Returns a result and internal state for subsequent calls.
runVKAPI :: VKApp app -> VKAPI app r -> IO (Either (APIError VKError) r, VKApp app)
runVKAPI vkapp action = do
  (res, _, vkapp1) <- runAPI vkBuilder (_vkManager vkapp) vkapp $ do
    unless (isAuthorized vkapp)
      doAuth
    mat <- liftState $ gets _vkAuthToken
    case mat of
      (Just at) -> do
        let vks = _vkSettings vkapp
            av = authApiVersion $ vkAuthArgs vks
            rargs = ["v" =. av
                    , "access_token" =. authToken at
                    , "lang"         =. vkLang vks
                    , "https"     =. if (vkHttps vks) then (Just ("1"::T.Text))
                                     else Nothing
                    , "test_mode" =. if (vkTestMode vks) then (Just ("1"::T.Text))
                                     else Nothing
                    ]
        customizeRoute (\r ->
                          r{urlPieces = "method":urlPieces r
                            , urlParams = rargs ++ urlParams r
                           })
        customizeRequest changeRequest
        baseURL apiBaseURL
      _ ->
        error "should never happen"
    action
  return (res, vkapp1)
  where
    doAuth = do
      at <- VK.authorize
      liftState $ modify (flip setAuthToken at)

#ifdef __GHCJS__
    changeRequest r = r{reqHeaders = ("Content-Type", "application/json") : reqHeaders r}
#else
    changeRequest r = r{requestHeaders = ("Content-Type", "application/json") : requestHeaders r}
#endif

-- | Check if we are authorized already.
isAuthorized :: VKApp app -> Bool
isAuthorized vkapp = isJust $ _vkAuthToken vkapp

-- | Set authorization token for future use.
setAuthToken :: VKApp app -> AuthToken -> VKApp app
setAuthToken vkapp at = vkapp{_vkAuthToken = Just at}

getAuthToken :: VKApp app -> Maybe AuthToken
getAuthToken = _vkAuthToken

-- | Get id of authorized user, returned by VKontakte.
getUserId :: VKApp app -> Maybe Int
getUserId vkapp = authUserId <$> _vkAuthToken vkapp

-- | Gets user application state inside API action.
vkGetApp :: VKAPI a a
vkGetApp =
  liftState $ gets _vkApp

-- | Save user application state to internal state inside API action.
vkSetApp :: a -> VKAPI a ()
vkSetApp app =
  liftState $ modify (\vkapp -> vkapp{_vkApp = app})

vkBuilder :: Builder
vkBuilder = basicBuilder "vk" apiBaseURL
