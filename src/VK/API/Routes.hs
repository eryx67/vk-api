{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- | Mappings between queries and results

module VK.API.Routes (
  -- * Mappings
  -- $mappings
                     ) where

import           Network.API.Builder
import           Network.API.Builder.Queryable


import qualified VK.API.Actions.Audio          as A
import qualified VK.API.Actions.Users          as U
import qualified VK.API.Actions.Video          as V
import qualified VK.API.Models.Audio           as A
import           VK.API.Models.Types           (ActionResponse (..), Album,
                                                Items (..), User)
import qualified VK.API.Models.Users           as U
import qualified VK.API.Models.Video           as V
import           VK.API.Routable


instance Routable V.Get (ActionResponse (Items V.Video)) where
  toRoute q = Route ["video.get"] (toURLParams q) "GET"

instance Routable V.Add (ActionResponse Int) where
  toRoute q = Route ["video.add"] (toURLParams q) "POST"

instance Routable V.Edit (ActionResponse Int) where
  toRoute q = Route ["video.edit"] (toURLParams q) "POST"

instance Routable V.Delete (ActionResponse Int) where
  toRoute q = Route ["video.delete"] (toURLParams q) "POST"

instance Routable V.Search (ActionResponse (Items V.Video)) where
  toRoute q = Route ["video.search"] (toURLParams q) "GET"

instance Routable V.Save (ActionResponse V.VideoSaveResponse) where
  toRoute q = Route ["video.save"] (toURLParams q) "POST"

instance Routable V.Restore (ActionResponse Int) where
  toRoute q = Route ["video.restore"] (toURLParams q) "POST"

instance Routable V.UserVideos (Items V.Video) where
  toRoute q = Route ["video.getUserVideos"] (toURLParams q) "GET"

instance Routable V.Albums (ActionResponse (Items Album)) where
  toRoute q = Route ["video.getAlbums"] (toURLParams q) "GET"

instance Routable V.GetAlbum (ActionResponse (Items Album)) where
  toRoute q = Route ["video.getAlbumById"] (toURLParams q) "GET"

instance Routable V.AddAlbum (ActionResponse Int) where
  toRoute q = Route ["video.addAlbum"] (toURLParams q) "POST"

instance Routable V.EditAlbum (ActionResponse Int) where
  toRoute q = Route ["video.editAlbum"] (toURLParams q) "POST"

instance Routable V.DeleteAlbum (ActionResponse Int) where
  toRoute q = Route ["video.deleteAlbum"] (toURLParams q) "POST"

instance Routable V.ReorderAlbums (ActionResponse Int) where
  toRoute q = Route ["video.reorderAlbums"] (toURLParams q) "POST"

instance Routable V.AddToAlbum (ActionResponse Int) where
  toRoute q = Route ["video.addToAlbum"] (toURLParams q) "POST"

instance Routable V.RemoveFromAlbum (ActionResponse Int) where
  toRoute q = Route ["video.removeFromAlbum"] (toURLParams q) "POST"

instance Routable V.AlbumsByVideo (ActionResponse (Items Album)) where
  toRoute q = Route ["video.albumsByVideo"] (toURLParams q) "GET"

instance Routable V.Comments (ActionResponse (Items V.Comment)) where
  toRoute q = Route ["video.getComments"] (toURLParams q) "GET"

instance Routable V.CreateComment (ActionResponse Int) where
  toRoute q = Route ["video.createComment"] (toURLParams q) "POST"

instance Routable V.DeleteComment (ActionResponse Int) where
  toRoute q = Route ["video.deleteComment"] (toURLParams q) "POST"

instance Routable V.RestoreComment (ActionResponse Int) where
  toRoute q = Route ["video.restoreComment"] (toURLParams q) "POST"

instance Routable V.EditComment (ActionResponse Int) where
  toRoute q = Route ["video.editComment"] (toURLParams q) "POST"

instance Routable V.GetTags (ActionResponse (Items V.Tag)) where
  toRoute q = Route ["video.getTags"] (toURLParams q) "GET"

instance Routable V.PutTag (ActionResponse Int) where
  toRoute q = Route ["video.putTag"] (toURLParams q) "POST"

instance Routable V.RemoveTag (ActionResponse Int) where
  toRoute q = Route ["video.removeTag"] (toURLParams q) "POST"

instance Routable V.NewTags (ActionResponse (Items V.NewTag)) where
  toRoute q = Route ["video.getNewTags"] (toURLParams q) "GET"

instance Routable V.Report (ActionResponse Int) where
  toRoute q = Route ["video.report"] (toURLParams q) "POST"

instance Routable V.ReportComment (ActionResponse Int) where
  toRoute q = Route ["video.report"] (toURLParams q) "POST"

-- Audio

instance Routable A.Get (ActionResponse (Items A.Audio)) where
  toRoute q = Route ["audio.get"] (toURLParams q) "GET"

instance Routable A.GetById (ActionResponse [A.Audio]) where
  toRoute q = Route ["audio.getById"] (toURLParams q) "GET"

instance Routable A.GetLyrics (ActionResponse A.Lyrics) where
  toRoute q = Route ["audio.getLyrics"] (toURLParams q) "GET"

instance Routable A.Search (ActionResponse (Items A.Audio)) where
  toRoute q = Route ["audio.search"] (toURLParams q) "GET"

instance Routable A.GetUploadServer (ActionResponse A.UploadServer) where
  toRoute q = Route ["audio.getUploadServer"] (toURLParams q) "GET"

instance Routable A.Save (ActionResponse A.SavedAudio) where
  toRoute q = Route ["audio.save"] (toURLParams q) "POST"

instance Routable A.Add (ActionResponse Int) where
  toRoute q = Route ["audio.add"] (toURLParams q) "POST"

instance Routable A.Delete (ActionResponse Int) where
  toRoute q = Route ["audio.delete"] (toURLParams q) "POST"

instance Routable A.Edit (ActionResponse Int) where
  toRoute q = Route ["audio.edit"] (toURLParams q) "POST"

instance Routable A.Reorder (ActionResponse Int) where
  toRoute q = Route ["audio.reorder"] (toURLParams q) "POST"

instance Routable A.Restore (ActionResponse A.Audio) where
  toRoute q = Route ["audio.restore"] (toURLParams q) "POST"

instance Routable A.Albums (ActionResponse (Items Album)) where
  toRoute q = Route ["audio.getAlbums"] (toURLParams q) "GET"

instance Routable A.AddAlbum (ActionResponse A.AlbumResponse) where
  toRoute q = Route ["audio.addAlbum"] (toURLParams q) "POST"

instance Routable A.EditAlbum (ActionResponse Int) where
  toRoute q = Route ["audio.editAlbum"] (toURLParams q) "POST"

instance Routable A.DeleteAlbum (ActionResponse Int) where
  toRoute q = Route ["audio.deleteAlbum"] (toURLParams q) "POST"

instance Routable A.MoveToAlbum (ActionResponse Int) where
  toRoute q = Route ["audio.moveToAlbum"] (toURLParams q) "POST"

instance Routable A.Broadcast (ActionResponse [Int]) where
  toRoute q = Route ["audio.setBroadcast"] (toURLParams q) "POST"

instance Routable A.BroadcastList (ActionResponse [A.BroadcastListResult]) where
  toRoute q = Route ["audio.getBroadcastList"] (toURLParams q) "GET"

instance Routable A.Recommendations (ActionResponse (Items A.Audio)) where
  toRoute q = Route ["audio.getRecommendations"] (toURLParams q) "GET"

instance Routable A.Popular (ActionResponse [A.Audio]) where
  toRoute q = Route ["audio.getPopular"] (toURLParams q) "GET"

instance Routable A.GetCount (ActionResponse Int) where
  toRoute q = Route ["audio.getCount"] (toURLParams q) "GET"

instance Routable U.Get (ActionResponse (Items User)) where
  toRoute q = Route ["users.get"] (toURLParams q) "GET"

instance Routable U.Search (ActionResponse (Items User)) where
  toRoute q = Route ["users.get"] (toURLParams q) "GET"

instance Routable U.IsAppUser (ActionResponse Int) where
  toRoute q = Route ["users.isAppUser"] (toURLParams q) "GET"

instance Routable U.Subscriptions (ActionResponse U.SubscriptionsResult) where
  toRoute q = Route ["users.getSubscriptions"] (toURLParams q) "GET"

instance Routable U.Followers (ActionResponse (Items User)) where
  toRoute q = Route ["users.getFollowers"] (toURLParams q) "GET"

instance Routable U.Report (ActionResponse Int) where
  toRoute q = Route ["users.report"] (toURLParams q) "POST"

instance Routable U.NearBy (ActionResponse (Items User)) where
  toRoute q = Route ["users.getNearBy"] (toURLParams q) "GET"

-- $mappings
-- >import qualified VK.API.Actions.Audio          as A
-- >import qualified VK.API.Actions.Users          as U
-- >import qualified VK.API.Actions.Video          as V
-- >import qualified VK.API.Models.Audio           as A
-- >import           VK.API.Models.Types           (ActionResponse (..), Album,
-- >                                                Items (..), User)
-- >import qualified VK.API.Models.Users           as U
-- >import qualified VK.API.Models.Video           as V
-- >import           VK.API.Routable
-- >
-- >instance Routable V.Get (ActionResponse (Items V.Video)) where
-- >  toRoute q = Route ["video.get"] (toURLParams q) "GET"
-- >
-- >instance Routable V.Add (ActionResponse Int) where
-- >  toRoute q = Route ["video.add"] (toURLParams q) "POST"
-- >
-- >instance Routable V.Edit (ActionResponse Int) where
-- >  toRoute q = Route ["video.edit"] (toURLParams q) "POST"
-- >
-- >instance Routable V.Delete (ActionResponse Int) where
-- >  toRoute q = Route ["video.delete"] (toURLParams q) "POST"
-- >
-- >instance Routable V.Search (ActionResponse (Items V.Video)) where
-- >  toRoute q = Route ["video.search"] (toURLParams q) "GET"
-- >
-- >instance Routable V.Save (ActionResponse V.VideoSaveResponse) where
-- >  toRoute q = Route ["video.save"] (toURLParams q) "POST"
-- >
-- >instance Routable V.Restore (ActionResponse Int) where
-- >  toRoute q = Route ["video.restore"] (toURLParams q) "POST"
-- >
-- >instance Routable V.UserVideos (Items V.Video) where
-- >  toRoute q = Route ["video.getUserVideos"] (toURLParams q) "GET"
-- >
-- >instance Routable V.Albums (ActionResponse (Items Album)) where
-- >  toRoute q = Route ["video.getAlbums"] (toURLParams q) "GET"
-- >
-- >instance Routable V.GetAlbum (ActionResponse (Items Album)) where
-- >  toRoute q = Route ["video.getAlbumById"] (toURLParams q) "GET"
-- >
-- >instance Routable V.AddAlbum (ActionResponse Int) where
-- >  toRoute q = Route ["video.addAlbum"] (toURLParams q) "POST"
-- >
-- >instance Routable V.EditAlbum (ActionResponse Int) where
-- >  toRoute q = Route ["video.editAlbum"] (toURLParams q) "POST"
-- >
-- >instance Routable V.DeleteAlbum (ActionResponse Int) where
-- >  toRoute q = Route ["video.deleteAlbum"] (toURLParams q) "POST"
-- >
-- >instance Routable V.ReorderAlbums (ActionResponse Int) where
-- >  toRoute q = Route ["video.reorderAlbums"] (toURLParams q) "POST"
-- >
-- >instance Routable V.AddToAlbum (ActionResponse Int) where
-- >  toRoute q = Route ["video.addToAlbum"] (toURLParams q) "POST"
-- >
-- >instance Routable V.RemoveFromAlbum (ActionResponse Int) where
-- >  toRoute q = Route ["video.removeFromAlbum"] (toURLParams q) "POST"
-- >
-- >instance Routable V.AlbumsByVideo (ActionResponse (Items Album)) where
-- >  toRoute q = Route ["video.albumsByVideo"] (toURLParams q) "GET"
-- >
-- >instance Routable V.Comments (ActionResponse (Items V.Comment)) where
-- >  toRoute q = Route ["video.getComments"] (toURLParams q) "GET"
-- >
-- >instance Routable V.CreateComment (ActionResponse Int) where
-- >  toRoute q = Route ["video.createComment"] (toURLParams q) "POST"
-- >
-- >instance Routable V.DeleteComment (ActionResponse Int) where
-- >  toRoute q = Route ["video.deleteComment"] (toURLParams q) "POST"
-- >
-- >instance Routable V.RestoreComment (ActionResponse Int) where
-- >  toRoute q = Route ["video.restoreComment"] (toURLParams q) "POST"
-- >
-- >instance Routable V.EditComment (ActionResponse Int) where
-- >  toRoute q = Route ["video.editComment"] (toURLParams q) "POST"
-- >
-- >instance Routable V.GetTags (ActionResponse (Items V.Tag)) where
-- >  toRoute q = Route ["video.getTags"] (toURLParams q) "GET"
-- >
-- >instance Routable V.PutTag (ActionResponse Int) where
-- >  toRoute q = Route ["video.putTag"] (toURLParams q) "POST"
-- >
-- >instance Routable V.RemoveTag (ActionResponse Int) where
-- >  toRoute q = Route ["video.removeTag"] (toURLParams q) "POST"
-- >
-- >instance Routable V.NewTags (ActionResponse (Items V.NewTag)) where
-- >  toRoute q = Route ["video.getNewTags"] (toURLParams q) "GET"
-- >
-- >instance Routable V.Report (ActionResponse Int) where
-- >  toRoute q = Route ["video.report"] (toURLParams q) "POST"
-- >
-- >instance Routable V.ReportComment (ActionResponse Int) where
-- >  toRoute q = Route ["video.report"] (toURLParams q) "POST"
-- >
-- >instance Routable A.Get (ActionResponse (Items A.Audio)) where
-- >  toRoute q = Route ["audio.get"] (toURLParams q) "GET"
-- >
-- >instance Routable A.GetById (ActionResponse [A.Audio]) where
-- >  toRoute q = Route ["audio.getById"] (toURLParams q) "GET"
-- >
-- >instance Routable A.GetLyrics (ActionResponse A.Lyrics) where
-- >  toRoute q = Route ["audio.getLyrics"] (toURLParams q) "GET"
-- >
-- >instance Routable A.Search (ActionResponse (Items A.Audio)) where
-- >  toRoute q = Route ["audio.search"] (toURLParams q) "GET"
-- >
-- >instance Routable A.GetUploadServer (ActionResponse A.UploadServer) where
-- >  toRoute q = Route ["audio.getUploadServer"] (toURLParams q) "GET"
-- >
-- >instance Routable A.Save (ActionResponse A.SavedAudio) where
-- >  toRoute q = Route ["audio.save"] (toURLParams q) "POST"
-- >
-- >instance Routable A.Add (ActionResponse Int) where
-- >  toRoute q = Route ["audio.add"] (toURLParams q) "POST"
-- >
-- >instance Routable A.Delete (ActionResponse Int) where
-- >  toRoute q = Route ["audio.delete"] (toURLParams q) "POST"
-- >
-- >instance Routable A.Edit (ActionResponse Int) where
-- >  toRoute q = Route ["audio.edit"] (toURLParams q) "POST"
-- >
-- >instance Routable A.Reorder (ActionResponse Int) where
-- >  toRoute q = Route ["audio.reorder"] (toURLParams q) "POST"
-- >
-- >instance Routable A.Restore (ActionResponse A.Audio) where
-- >  toRoute q = Route ["audio.restore"] (toURLParams q) "POST"
-- >
-- >instance Routable A.Albums (ActionResponse (Items Album)) where
-- >  toRoute q = Route ["audio.getAlbums"] (toURLParams q) "GET"
-- >
-- >instance Routable A.AddAlbum (ActionResponse A.AlbumResponse) where
-- >  toRoute q = Route ["audio.addAlbum"] (toURLParams q) "POST"
-- >
-- >instance Routable A.EditAlbum (ActionResponse Int) where
-- >  toRoute q = Route ["audio.editAlbum"] (toURLParams q) "POST"
-- >
-- >instance Routable A.DeleteAlbum (ActionResponse Int) where
-- >  toRoute q = Route ["audio.deleteAlbum"] (toURLParams q) "POST"
-- >
-- >instance Routable A.MoveToAlbum (ActionResponse Int) where
-- >  toRoute q = Route ["audio.moveToAlbum"] (toURLParams q) "POST"
-- >
-- >instance Routable A.Broadcast (ActionResponse [Int]) where
-- >  toRoute q = Route ["audio.setBroadcast"] (toURLParams q) "POST"
-- >
-- >instance Routable A.BroadcastList (ActionResponse [A.BroadcastListResult]) where
-- >  toRoute q = Route ["audio.getBroadcastList"] (toURLParams q) "GET"
-- >
-- >instance Routable A.Recommendations (ActionResponse (Items A.Audio)) where
-- >  toRoute q = Route ["audio.getRecommendations"] (toURLParams q) "GET"
-- >
-- >instance Routable A.Popular (ActionResponse [A.Audio]) where
-- >  toRoute q = Route ["audio.getPopular"] (toURLParams q) "GET"
-- >
-- >instance Routable A.GetCount (ActionResponse Int) where
-- >  toRoute q = Route ["audio.getCount"] (toURLParams q) "GET"
-- >
-- >instance Routable U.Get (ActionResponse (Items User)) where
-- >  toRoute q = Route ["users.get"] (toURLParams q) "GET"
-- >
-- >instance Routable U.Search (ActionResponse (Items User)) where
-- >  toRoute q = Route ["users.get"] (toURLParams q) "GET"
-- >
-- >instance Routable U.IsAppUser (ActionResponse Int) where
-- >  toRoute q = Route ["users.isAppUser"] (toURLParams q) "GET"
-- >
-- >instance Routable U.Subscriptions (ActionResponse U.SubscriptionsResult) where
-- >  toRoute q = Route ["users.getSubscriptions"] (toURLParams q) "GET"
-- >
-- >instance Routable U.Followers (ActionResponse (Items User)) where
-- >  toRoute q = Route ["users.getFollowers"] (toURLParams q) "GET"
-- >
-- >instance Routable U.Report (ActionResponse Int) where
-- >  toRoute q = Route ["users.report"] (toURLParams q) "POST"
-- >
-- >instance Routable U.NearBy (ActionResponse (Items User)) where
-- >  toRoute q = Route ["users.getNearBy"] (toURLParams q) "GET"
