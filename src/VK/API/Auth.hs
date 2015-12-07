-- | VKontakte OAuth authorization
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VK.API.Auth (authorize
                   , authorizeFromURL
                   , oauthURL
                   , oauthRoute
                   ) where

import           Control.Error.Util
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import           Data.Time.Clock (addUTCTime, getCurrentTime)
import           Network.API.Builder
import           Text.Read (readMaybe)
import           Text.URI

#ifdef __GHCJS__
#else

import           Control.Arrow ((***))
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Lazy (get, gets, put)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Maybe (listToMaybe)
import qualified Data.Text.Encoding as TE
import           Network.HTTP.Client
import           Network.HTTP.Types.Status (Status (..))
import           Text.HTML.TagSoup

#endif

import           VK.API.Errors
import           VK.API.Types hiding (Status)
import           VK.Internal.Utils


oauthBaseURL :: T.Text
oauthBaseURL = "https://oauth.vk.com"

-- | Build authorization url for client id
oauthURL :: Integer -> AuthArgs -> T.Text
oauthURL clientId args =
  routeURL oauthBaseURL $ oauthRoute clientId args

-- | Build authorization 'Route' for client id
oauthRoute :: Integer -> AuthArgs -> Route
oauthRoute cid AuthArgs{..} = Route ["authorize"] args "GET"
  where
    scope = (\ps -> T.intercalate "," $ map (T.toLower . tshow) ps) <$> authScope
    args = ["client_id" =. tshow cid
           , "display" =. (T.toLower $ tshow authDisplay)
           , "redirect_uri" =. authRedirectURI
           , "scope" =. scope
           , "response_type" =. (T.toLower $ tshow authResponseType)
           , "v" =. authApiVersion
           , "state" =. authState
           ]

#ifdef __GHCJS__
#else

-- for login form processing
userFormArg :: T.Text
userFormArg = "email"

-- for login form processing
passFormArg :: T.Text
passFormArg = "pass"

#endif

-- | Get authorization info from url provided by VKontakte after
-- manual authorization.
authorizeFromURL :: String -> IO (Either (APIError VKError) AuthToken)
authorizeFromURL aurl = do
  mat <- parseTokens aurl
  return $ note authError mat


-- | Authorize application with user credentials and  required permissions.
-- Returns token for application usage.
--
-- The requests chain when no permissions are requested:
--
-- >+-------------+    +------------+    +--------+    +------------+
-- >|             |    |            |    |        |    |            |
-- >|oauth.vk.com +---->login.vk.com+---->Redirect+---->Token in    |
-- >|             |    |            |    |2 times |    |redirect URL|
-- >+-------------+    +------------+    +--------+    +------------+
--
-- The requests chain when scope permissions are requested:
--
-- >+-------------+    +------------+    +------------+     +------------+
-- >|             |    |            |    |            |     |            |
-- >|oauth.vk.com +---->login.vk.com+---->login.vk.com+----->Token in    |
-- >|             |    |            |    |            |     |redirect URL|
-- >+-------------+    +------------+    +------------+     +------------+

#ifdef __GHCJS__
authorize :: VKAPI a AuthToken
authorize =
  error "to be implemented"
#else
authorize :: VKAPI a AuthToken
authorize =
  catchE requestAuth checkError
  where
    requestAuth = do
      VKSettings{..} <- liftState $ gets _vkSettings
      oldBuilder <- liftBuilder get

      baseURL oauthBaseURL
      resp1 <- routeResponse $ oauthRoute vkAppId vkAuthArgs

      resp2 <- sendForm resp1 2 (substituteForm vkUser)
      _ <- sendForm resp2 0 (map (\kv -> [kv]))

      liftBuilder $ put oldBuilder

      throwE authError
    checkError (HTTPError (StatusCodeException (Status 302 _) headers _)) =
      parseLocation headers
    checkError (HTTPError (TooManyRedirects (resp:_))) =
      parseLocation $ responseHeaders resp
    checkError _ =
      throwE authError
    sendForm resp rnum fillArgs = do
      (actionUrl, actionPath, actionArgs) <- failWith (mkError "no form")
                                             (parseForm $ responseBody resp)
      customizeRequest (\r ->
                          r{cookieJar = Just $ responseCookieJar resp
                           , redirectCount = rnum})
      baseURL actionUrl
      resp1 <- routeResponse $ Route actionPath (fillArgs actionArgs) "POST"
      return resp1
    parseLocation headers =
      noteT authError $ do
        aurl <-  hoistMaybe $ lookup "Location" headers
        at <- liftIO . parseTokens $ BSC.unpack aurl
        hoistMaybe at

substituteForm :: AuthUser -> [(T.Text, T.Text)] -> [[(T.Text, T.Text)]]
substituteForm AuthUser{..} args =
  map (\case
           (k, _)
             | k == userFormArg -> [(k, authUsername)]
             | k == passFormArg -> [(k, authPassword)]
           kv -> [kv]
      ) args

parseForm :: BL.ByteString -> Maybe (T.Text, [T.Text], [(T.Text, T.Text)])
parseForm src =
  (listToMaybe $ sections (~== TagOpen ("form"::BL.ByteString) []) tags) >>=
  doParse . (takeWhile (/= TagClose ("form"::BL.ByteString)))
  where
    doParse fbody = do
      (aurl, apath, aargs) <- parseAction . BLC.unpack $ formAction fbody
      let fargs = map (decodeTxt *** decodeTxt) $ formInputs fbody

      return (aurl, apath, aargs ++ fargs)
    formAction = fromAttrib "action" . head
    formInputs fb = map parseInput $ filter inputFilter fb
    inputFilter tag = tag ~== (TagOpen ("input"::BL.ByteString) []) && fromAttrib "name" tag /= ""
    parseInput inp = (fromAttrib "name" inp, fromAttrib "value" inp)
    tags = parseTags src
    parseAction aurl = do
      uri <- parseURI aurl
      let qis = map (T.pack *** T.pack) $ uriQueryItems uri
          path = map T.pack . filter (/= "") $ uriPathSegments uri
          burl = T.pack . show $ uri{uriQuery = Nothing, uriPath = "", uriFragment = Nothing}
      return (burl, path, qis)
    decodeTxt = TE.decodeUtf8 . BL.toStrict

#endif

-- for result url parsing
accessTokenArg :: String
accessTokenArg = "access_token"

-- for result url parsing
accessExpireArg :: String
accessExpireArg = "expires_in"

-- for result url parsing
accessUseIdArg :: String
accessUseIdArg = "user_id"

authError :: APIError VKError
authError = mkError "authorization failed"

mkError :: T.Text -> APIError VKError
mkError msg = APIError $ AuthError msg

parseTokens :: String -> IO (Maybe AuthToken)
parseTokens url = do
  ctime <- liftIO getCurrentTime
  let parseExpire v = (flip addUTCTime ctime . fromIntegral) <$> (readMaybe v::Maybe Int)
  return $ do
    uri <- parseURI url
    kvs <- queryToPairs <$> uriFragment uri
    at <- T.pack <$> lookup accessTokenArg kvs
    exptm <- lookup accessExpireArg kvs >>= parseExpire
    uid <- lookup accessUseIdArg kvs >>= readMaybe
    return $ AuthToken at uid exptm
