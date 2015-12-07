{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module VK.API.AuthSpec (main, spec) where

import           Control.Monad.Trans.State.Lazy (gets)
import           Network.API.Builder
import           Test.Hspec

import           VK.API

import           TestSettings

main :: IO ()
main = hspec spec

vksettings :: Maybe [AuthPermissions] -> VKSettings
vksettings scope = createSettings appId userName userPass scope

spec :: Spec
spec = do
  describe "OAuth authorization" $ do
    it "doesn't ask for any permissions" $ do
      execVKAPI () (vksettings Nothing) getAuthToken
        >>= (`shouldSatisfy` checkAuthToken)
    it "asks for some permissions" $ do
      execVKAPI () (vksettings $ Just [Audio, Video]) getAuthToken
        >>= (`shouldSatisfy` checkAuthToken)
  where
    getAuthToken =
      liftState $ gets _vkAuthToken

checkAuthToken :: Either (APIError VKError) (Maybe AuthToken) -> Bool
checkAuthToken (Right (Just (AuthToken _ _ _))) = True
checkAuthToken _ = False
