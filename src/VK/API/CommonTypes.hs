{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Common types for "Models" and "Actions".

module VK.API.CommonTypes where


import qualified Control.Applicative as A
import           Data.Aeson
import qualified Data.Text           as T
import           GHC.Generics        (Generic)
import           Network.API.Builder
import           Text.Parsec

import           VK.Internal.Orphans ()
import           VK.Internal.Utils

data OwnerId = GroupId !Int
             | UserId !Int
             deriving (Show, Eq)

instance ToQuery OwnerId where
  toQuery n oid = toQuery n $ ownerIdToInt oid

instance FromJSON OwnerId where
  parseJSON =
    withScientific "OwnerId" (pure . doParse)
    where
      doParse ns =
       case truncate ns of
       ni | ni >= 0 -> UserId ni
       ni -> GroupId $ abs ni

ownerIdToInt :: OwnerId -> Int
ownerIdToInt (GroupId v) = -v
ownerIdToInt (UserId v) = v

data Privacy = AllowAll
             | AllowFriends
             | AllowFriendsOfFriends
             | AllowFriendsOfFriendsOnly
             | AllowNobody
             | AllowOnlyMe
             | AllowList Int
             | AllowUser Int
             | DenyList Int
             | DenyUser Int
             deriving Show

instance FromJSON Privacy where
  parseJSON =
    withText "Privacy" doParse
    where
      doParse txt =
        case parse parser "" txt of
        Left _ -> mempty
        Right v -> pure v
      parser =
        try (string "friends_of_friends_only" >> return AllowFriendsOfFriendsOnly)
        <|> try (string "friends_of_friends" >> return AllowFriendsOfFriends)
        <|> (string "friends" >> return AllowFriends)
        <|> (string "nobody" >> return AllowNobody)
        <|> (string "only_me" >> return AllowOnlyMe)
        <|> (string "list" >> many1 digit >>= return . AllowList . read)
        <|> (many1 digit >>= return . AllowUser . read)
        <|> (string "all" >> return AllowAll)
        <|> (string "-" >>
             ((many1 digit >>= return . DenyUser . read)
              <|> (string "list" >> many1 digit >>= return . DenyList . read)))

instance ToQuery Privacy where
  toQuery n v = toQuery n $ encodeVal v
    where
      encodeVal :: Privacy -> T.Text
      encodeVal AllowAll = "all"
      encodeVal AllowFriends = "friends"
      encodeVal AllowFriendsOfFriends = "friends_of_friends"
      encodeVal AllowFriendsOfFriendsOnly = "friends_of_friends_only"
      encodeVal AllowNobody = "nobody"
      encodeVal AllowOnlyMe = "only_me"
      encodeVal (AllowList lid) = T.append "list" (T.pack $ show lid)
      encodeVal (AllowUser uid) = T.pack $ show uid
      encodeVal (DenyList lid) = T.append "-list" (T.pack $ show lid)
      encodeVal (DenyUser uid) = T.append "-" (T.pack $ show uid)

data Sex = SexUndefined
         | Male
         | Female
         deriving (Show, Generic, Bounded, Enum)

instance FromJSON Sex where
  parseJSON =
    withScientific "Sex"
    (\v ->
       case (round v :: Int) of
       idx | idx >= length sexes || idx < 0 ->
               mempty
       idx ->
         pure $ sexes !! idx
    )
    where
       sexes = [minBound ..]::[Sex]

instance ToQuery Sex where
  toQuery n v = toQuery n (fromEnum v)

data UserRelation = RelationUnknown
                  | NotMarried
                  | HaveFriend
                  | Contracted -- ^ помолвлен/помолвлена
                  | Married
                  | IsComplicated -- ^ всё сложно
                  | ActiveSearch
                  | InLove
                  deriving (Show, Generic, Enum, Bounded)

instance FromJSON UserRelation where
  parseJSON =
    intEnumFromJSON "UserRelation" ([minBound ..]::[UserRelation])

instance ToQuery UserRelation where
  toQuery n v = toQuery n (fromEnum v)

data AudioGenre = AdioGenreUnknown
                | Rock
                | Pop
                | RapAndHipHop
                | EasyListening
                | DanceAndHouse
                | Instrumental
                | Metal
                | Dubstep
                | JazzAndBlues
                | DrumAndBass
                | Trance
                | Chanson
                | Ethnic
                | AcousticAndVocal
                | Reggae
                | Classical
                | IndiePop
                | OtherGenre
                | Speech
                | OtherGenre1
                | Alternative
                | ElectropopAndDisco
                deriving (Show, Generic, Enum, Bounded)

instance FromJSON AudioGenre where
  parseJSON v =
    intEnumFromJSON "AudioGenre" ([minBound ..]::[AudioGenre]) v
    A.<|> pure OtherGenre

instance ToQuery AudioGenre where
  toQuery n v = toQuery n (fromEnum v)
