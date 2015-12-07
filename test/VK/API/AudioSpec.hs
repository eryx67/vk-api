{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module VK.API.AudioSpec (main, spec) where

import           Control.Concurrent
import           Control.Monad.Trans.State.Lazy (gets)
import qualified Data.Default.Generics          as D
import           Network.API.Builder
import           Test.Hspec

import           VK.API
import qualified VK.API.Audio                   as A

import           TestSettings

main :: IO ()
main = hspec spec

vksettings :: Maybe [AuthPermissions] -> VKSettings
vksettings scope = createSettings appId userName userPass scope

spec :: Spec
spec = do
  beforeAll authorizeAudio $ after_ (threadDelay 1000000) $ do
    describe "Audio API" $ do
      it "audio.search" $ \s ->
        (`shouldSatisfyAR` checkAudioSearch) =<<
        runVKAPI s searchAudio
      it "getById, add and edit" $ \s ->
        (`shouldSatisfyAR` checkAudioById) =<<
        (runVKAPI s $ do
          (AR (Items (sar:_) _)) <- searchAudio
          (AR (gar:_)) <- getAudioById $ [(A.audioOwnerId sar, A.audioId sar)]
          (AR aid) <- toAPI $ A.Add (A.audioId gar) (A.audioOwnerId gar) Nothing
          uid <- getMyId
          toAPI $ D.def{A.editOwnerId = UserId uid
                       , A.editAudioId = aid
                       , A.editTitle = Just "My Added Record"
                       }
        )
      it "audio.getLyrics" $ \s ->
        (`shouldSatisfyAR` checkLyrics) =<<
        (runVKAPI s $ do
          (AR (Items (ar:_) _)) <- searchAudio
          getAudioLyrics (A.audioLyricsId ar))
      it "audio.getUploadServer" $ \s ->
        (`shouldSatisfyAR` checkUploadServer) =<<
        (runVKAPI s $ toAPI A.GetUploadServer)
      it "upload audio, save and delete it after" $ \s ->
        (`shouldSatisfyAR` checkUploadAudio) =<<
        (runVKAPI s $ do
             let artist = Just "MyArtist"
                 title = Just "MyTitle"
             (AR sa) <- A.uploadAudio "test-data/audio.mp3" artist title
             toAPI $ A.Delete (A.savedaudioId sa) (A.savedaudioOwnerId sa))
      it "get my own records and reorder" $ \s ->
        (`shouldSatisfyAR` checkAudioGet) =<<
        (runVKAPI s $ do
            uid <- getMyId
            (AR (Items ars _))  <- getAudio $ UserId uid
            case ars of
              (ar1:ar2:_) ->
                toAPI $ D.def{A.reorderAudioId = A.audioId ar1
                             , A.reorderAfter = Just $ A.audioId ar2
                             }
              _ ->
                return (AR 1))
      it "delete and restore first record" $ \s ->
        (`shouldSatisfyAR` checkAudioRestore) =<<
        (runVKAPI s $ do
            uid <- getMyId
            (AR (Items ars _))  <- getAudio $ UserId uid
            case ars of
              (ar1:_) -> do
                (AR _) <- toAPI $ A.Delete (A.audioId ar1) (UserId uid)
                toAPI $ D.def{A.restoreAudioId = A.audioId ar1}
              _ ->
                error "should never happen")
      it "add, edit, move to album" $ \s ->
        (`shouldSatisfyAR` checkAudioAlbum) =<<
        (runVKAPI s $ do
            uid <- getMyId
            (AR (Items ars _))  <- getAudio $ UserId uid
            (AR (A.AlbumResponse alid)) <- toAPI $ A.AddAlbum "My Album" Nothing
            (AR _) <- toAPI $ A.EditAlbum alid "My Edited Album" Nothing
            case ars of
              (ar1:_) -> do
                toAPI $ D.def{A.movetoalbumAlbumId = alid
                             , A.movetoalbumAudioIds = [A.audioId ar1]}
              _ ->
                error "should never happen")
      it "list albums and delete the first one" $ \s ->
        (`shouldSatisfyAR` checkAudioAlbumDelete) =<<
        (runVKAPI s $ do
            (AR (Items als _)) <- toAPI $ A.Albums Nothing Nothing Nothing
            case als of
              (al:_) -> do
                toAPI $ A.DeleteAlbum (albumId al) Nothing
              _ ->
                error "should never happen")
      it "set broadcast list" $ \s ->
        (`shouldSatisfyAR` checkBroadcast) =<<
        (runVKAPI s $ do
            uid <- getMyId
            (AR (Items ars _))  <- getAudio $ UserId uid
            case ars of
              (ar1:_) -> do
                toAPI $ A.Broadcast (UserId uid, A.audioId ar1) Nothing
              _ ->
                error "should never happen")
      it "read broadcast list" $ \s ->
        (`shouldSatisfyAR` checkBroadcastList) =<<
        (runVKAPI s $ toAPI $ A.BroadcastList Nothing Nothing)

      it "get recommendations" $ \s ->
        (`shouldSatisfyAR` checkRecommendations) =<<
        (runVKAPI s $ do
            uid <- getMyId
            (AR (Items ars _))  <- getAudio $ UserId uid
            case ars of
              (ar1:_) -> do
                toAPI $ D.def{
                  A.recommendationsTargetAudio = (UserId uid, A.audioId ar1)
                  }
              _ ->
                error "should never happen")
      it "get popular" $ \s ->
        (`shouldSatisfyAR` checkPopular) =<<
        (runVKAPI s $ toAPI $ D.def{A.popularCount = Just 2})
      it "get count" $ \s ->
        (`shouldSatisfyAR` checkCount) =<<
        (runVKAPI s $ do
            uid <- getMyId
            toAPI $ A.GetCount (UserId uid))
  where
    getMyId = do
      muid <- liftState $ gets getUserId
      case muid of
        Nothing ->
          error "should never happen"
        Just uid ->
          return uid
    searchAudio =
      toAPI $ D.def{A.searchQ = "ABBA"
                   , A.searchCount = Just 2
                   , A.searchLyrics = Just 1
                   }
    getAudio oid =
      toAPI $ D.def{A.getOwnerId = Just oid
                   , A.getCount = Just 2
                   -- , A.getNeedUser = Just 1
                   }
    getAudioById q =
      toAPI $ A.GetById q
    getAudioLyrics (Just lid) =
      toAPI $ A.GetLyrics lid
    getAudioLyrics Nothing =
      error "should never happen"

authorizeAudio :: IO (VKApp ())
authorizeAudio = do
  threadDelay 3000000
  res <- startVKAPI () (vksettings $ Just [Audio, Status]) $ return ()
  case res of
    (Left e, _) ->
      error $ show e
    (Right _, s) ->
      return s

checkAudioSearch :: Items A.Audio -> Bool
checkAudioSearch (Items (A.Audio{}:_) _) = True
checkAudioSearch _ = False

checkAudioGet :: Int -> Bool
checkAudioGet _ = True

checkAudioById :: Int -> Bool
checkAudioById _ = True

checkAudioRestore :: A.Audio -> Bool
checkAudioRestore _ = True

checkUploadServer :: A.UploadServer -> Bool
checkUploadServer _ = True

checkLyrics :: A.Lyrics -> Bool
checkLyrics _ = True

checkUploadAudio :: Int -> Bool
checkUploadAudio 1 = True
checkUploadAudio _ = False

checkAudioAlbum :: Int -> Bool
checkAudioAlbum _ = True

checkAudioAlbumDelete :: Int -> Bool
checkAudioAlbumDelete _ = True

checkBroadcast :: [Int] -> Bool
checkBroadcast _ = True

checkBroadcastList :: [A.BroadcastListResult] -> Bool
checkBroadcastList (_:_) = True
checkBroadcastList _ = False

checkRecommendations :: Items A.Audio -> Bool
checkRecommendations _ = True

checkPopular :: [A.Audio] -> Bool
checkPopular _ = True

checkCount :: Int -> Bool
checkCount _ = True

shouldSatisfyAR :: (Show e, Show r) =>
                   (Either e (ActionResponse r), s) -> (r -> Bool) -> Expectation
shouldSatisfyAR (arg, _) predicate =
  shouldSatisfy arg (\case
                      (Right (AR val)) -> predicate val
                      _ -> False)
