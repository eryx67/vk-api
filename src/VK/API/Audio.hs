{-# LANGUAGE OverloadedStrings #-}
-- | Basic module to access VKontakte <http://vk.com/dev/audio.get audo api>

module VK.API.Audio (module Exp
                    , uploadAudio) where


import qualified Data.Text                             as T
import           Network.API.Builder

#ifdef __GHCJS__
#else

import           Control.Error.Util
import           Network.API.Builder.Send.Multipart
import           Network.HTTP.Client                   (httpLbs)
import           Network.HTTP.Client.MultipartFormData
import           Control.Exception
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Trans.Reader            (ask)
import           Data.Bifunctor                        (first)
import qualified Data.ByteString.Char8                 as BSC
import           Text.URI

import           VK.API.Routable
import           VK.API.Routes                         ()
import           VK.Internal.Utils

#endif

import           VK.API.Actions.Audio                  as Exp
import           VK.API.Actions.Types                  as Exp
import           VK.API.CommonTypes                    as Exp
import           VK.API.Models.Audio                   as Exp
import           VK.API.Models.Types                   as Exp

import           VK.API.Errors

#ifdef __GHCJS__

uploadAudio :: T.Text -> Maybe T.Text -> Maybe T.Text
               -> API s VKError (ActionResponse SavedAudio)
uploadAudio _ _ _ =
  error "to be implemented"
#else

-- | Upload audio file 'fn' to VKontakte. Register optional 'artist'
-- and 'title' for it.
uploadAudio :: T.Text -> Maybe T.Text -> Maybe T.Text
               -> API s VKError (ActionResponse SavedAudio)
uploadAudio fn artist title = do
  (AR (UploadServer uploadURL)) <- toAPI GetUploadServer

  let msrv = uriToRoute <$> (parseURI $ T.unpack uploadURL)
  (srvURL, srvArgs, srvRoute) <- hoistEither $ note (mkError "bad upload url") msrv
  -- construct request
  let fnPart = partFileSource "file" $ T.unpack fn
      parts = Multipart $ (fnPart:srvArgs)
  mreq <- sendMultipart (basicBuilder "audioUpload" srvURL) srvRoute parts
  req <- hoistEither $ note (mkError "can't construct request") mreq
  -- send request to server
  manager <- liftManager ask
  resp <- liftIO $ try $ httpLbs req manager
  res <- hoistEither $ first HTTPError resp
  -- parse response to 'Save' action and add file to our account
  save <- hoistEither $ receive res
  toAPI save{saveArtist = artist, saveTitle = title}
  where
    uriToRoute uri =
      let args = map (\(k, v) -> partBS (T.pack k) (BSC.pack v)) $ uriQueryItems uri
          path = map T.pack $ uriPathSegments uri
      in
        (tshow uri{uriPath = "", uriQuery = Nothing}, args, Route path [] "POST")
    mkError msg = APIError $ AudioUploadError msg

#endif
