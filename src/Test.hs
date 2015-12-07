{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
-- |

module Test where

-- import           Network.API.Builder.TH

-- data A = A1{a1 :: Int, a2 :: Int} | A2{a1 :: Int, a2 :: Int}

-- $(deriveQueryable ''A)

-- import           Control.Applicative        ((<|>))
-- import           Control.Monad.IO.Class         (liftIO)
-- import           Control.Monad.Trans.Except
-- import           Control.Monad.Trans.Maybe
-- import           Control.Monad.Trans.State.Lazy (gets)
-- import qualified Data.ByteString                as BS
-- import qualified Data.ByteString.Char8          as BSC
-- import qualified Data.ByteString.Lazy           as BL
-- import qualified Data.ByteString.Lazy.Char8     as BLC
import qualified Data.Default.Generics as D
-- import           Data.List                      (lookup)
-- import           Data.Maybe                     (listToMaybe)
-- import           Data.Monoid             ((<>))
import qualified Data.Text             as T
-- import qualified Data.Text.Encoding             as TE
-- import qualified Data.Text.Lazy          as LT
-- import qualified Data.Text.Lazy.Encoding as LTE
-- import           Data.Typeable           (Typeable)
-- import           GHC.Generics            (Generic)
-- import           Network.API.Builder
-- import           Network.API.Builder.API
-- import           Network.HTTP.Client
-- import           Network.HTTP.Types.Status      (Status (..))
-- import           Network.HTTP.Client.TLS
-- import           Control.Arrow                  ((***))
-- import           Control.Error.Util
-- import           Data.Time.Clock                (addUTCTime, getCurrentTime)
-- import           Text.HTML.TagSoup
-- import           Text.Read                      (readMaybe)
-- import           Text.URI


-- import qualified VK.App             as VK
import           VK.API
-- import qualified VK.API.Actions.Video  as V
import qualified VK.API.Actions.Audio  as A
import qualified VK.API.Models.Audio   as A

-- -- import           VK.API.Routes

userName :: T.Text
userName = "eryx67@gmail.com"

userPass :: T.Text
userPass = "cl0pdal0"

appId :: Integer
appId = 5082615

vksettings :: Maybe [AuthPermissions] -> VKSettings
vksettings scope = createSettings appId userName userPass scope

main :: IO ()
main = do
  res <- execVKAPI () (vksettings $ Just [Audio, Video]) $ do
    (AR (Items (ar:_) _)) <- searchAudio
    getAudio $ A.audioOwnerId ar
  print res
  where
    getAudio oid =
      toAPI $ D.def{A.getOwnerId = Just oid, A.getCount = Just 5}
    searchAudio =
      toAPI $ D.def{A.searchQ = "ABBA", A.searchCount = Just 5}
      -- liftState $ gets _vkAuthToken
