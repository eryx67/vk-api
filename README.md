Haskell bindings for <https://vk.com/dev VKontakte> API

Currently authentication, users, audio and video parts are fully implemented.

Please let me know if want other parts of API.

Code can be used in both, GHC and GHCJS.

# Usage

```haskell
-- Prepare our own state for application
data AppState = AppState [T.Text]

appState :: AppState
appState = AppState []

userName :: T.Text
userName = "myname@gmail.com"

userPass :: T.Text
userPass = "mypass"

appId :: Integer
appId = 1234567

appScope :: [AuthPermissions]
appScope = [Audio, Video]

vksettings :: VKSettings
vksettings = createSettings appId userName userPass (Just appScope)

execVKAPI appState vksettings $ do
uid <- liftState $ gets getUserId
-- get first 10 from my own audio records
toAPI $ D.def{A.getOwnerId = Just $ UserId uid
              , A.getCount = Just 10
        }
```
