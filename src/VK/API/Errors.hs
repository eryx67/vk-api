{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- |

module VK.API.Errors where

import           Data.Aeson
import qualified Data.HashMap.Strict as H
import           Data.Maybe          (fromMaybe)
import qualified Data.Text           as T
import           Network.API.Builder


-- 204	Нет доступа.

-- video
-- 800	Это видео уже добавлено
-- 214	Нет прав на добавление поста.
-- 219	Рекламный пост уже недавно добавлялс
-- 302	Создано максимальное количество альбомов.

-- audio
-- 121	Неверный хэш.
-- 123	Недопустимый формат аудиозаписи.
-- 270	Аудиозапись была изъята по запросу правообладателя и не может быть загружена.
-- 301	Недопустимое имя файла.
-- 302	Недопустимый размер файла.
-- 19	Контент недоступен

data VKError = AuthError !T.Text
             | AccessTokenExpired
             | AudioUploadError !T.Text
             | VKError !Int !T.Text
             deriving Show

instance FromJSON VKError where
  parseJSON =
    withObject "VKError"
    (\obj ->
       fromMaybe mempty $ parseError <$> H.lookup "error" obj)
    where
      parseError (Object obj) =
        (\err msg ->
          case err of
            5 -> AccessTokenExpired
            _ -> VKError err msg) <$>
        obj .: "error_code"
        <*> obj .: "error_msg"
      parseError _ = mempty

instance ErrorReceivable VKError where
  receiveError = useErrorFromJSON

