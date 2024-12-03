{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module MyLib where

import Control.Monad.Reader
import Data.Aeson
import Data.AesonBson
import Data.Maybe
import Data.Time.Calendar
import Database.MongoDB
import GHC.Generics
import Prelude.Compat
import Servant
import Prelude ()

{-
import Control.Monad.Except
import Control.Monad.Reader
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.String.Conversions
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant.Types.SourceT (source)
import System.Directory
import Text.Blaze
import qualified Text.Blaze.Html
import Text.Blaze.Html.Renderer.Utf8
-}

type AppW = ReaderT Pipe Handler

instance ToJSON User
instance FromJSON User where
  parseJSON = withObject "User" $ \v ->
    User
      <$> v
      .: "_id"
      <*> v
      .: "name"
      <*> v
      .: "age"
      <*> v
      .: "email"
      <*> v
      .: "birth_date"
      <*> v
      .:? "fingerprint"
      .!= []

data User = User
  { user_id :: String
  , name :: String
  , age :: Int
  , email :: String
  , birth_date :: Day
  , fingerprints :: [Int]
  }
  deriving (Eq, Show, Generic)

-- docToUser :: Document -> Maybe User
-- docToUser = aesonify

{-
users :: [User]
users = [User 0 "Dhanan" 20 "mirabala588@gmail.com" (fromGregorian 2004 1 26) [10101]]
-}

type GetAllUserAPI = "users" :> Get '[JSON] [User]

{-
type GetUserAPI = "users" :> Capture "user_id" Int :> Get '[JSON] User
type PostUserAPI = "users" :> ReqBody '[JSON] User :> Post '[JSON] User
type PutUserAPI = "users" :> ReqBody '[JSON] User :> Put '[JSON] User
type DeleteUserAPI = "users" :> Capture "user_id" Int :> DeleteNoContent
-}

type CombinedUserAPI = GetAllUserAPI -- :<|> GetUserAPI :<|> PostUserAPI :<|> DeleteUserAPI

accessDatabase :: (MonadIO m) => Pipe -> Action m a -> m a
accessDatabase pipe = access pipe master "Userbase"

getAllHandler :: AppW [User]
getAllHandler = do
  pipe <- ask
  liftIO $ do
    docs <- accessDatabase pipe $ find (select [] "Users") >>= rest
    return $ mapMaybe ((\case Success u -> Just u; Error _ -> Nothing) . (fromJSON . toJSON) . aesonify) docs

{-
getUserHandler :: Int -> Handler User
getUserHandler findId
  | findId < 0 = throwError err422{errBody = "Invalid User ID"}
  | otherwise = case filter (\user -> user_id user == findId) users of
      [] -> throwError err404{errBody = "User not found"}
      matchingUser -> return $ head matchingUser

postUserHandler :: User -> Handler User
postUserHandler newUser
  | user_id newUser < 0 = throwError err422{errBody = "Invalid User ID"}
  | otherwise = case filter (\user -> user_id newUser == user_id user) users of
      [] -> do
        let nu = newUser : users
        return newUser
      _ -> throwError err409{errBody = "User ID already exists"}

deleteUserHandler :: Int -> Handler NoContent
deleteUserHandler findId
  | findId < 0 = throwError err422{errBody = "Invalid User ID"}
  | otherwise = case filter (\user -> findId == user_id user) users of
      [] -> throwError err404{errBody = "User not found"}
      _ -> return NoContent
-}

server :: ServerT CombinedUserAPI AppW
server = getAllHandler -- :<|> getUserHandler :<|> postUserHandler :<|> deleteUserHandler

userAPI :: Proxy CombinedUserAPI
userAPI = Proxy

app :: Pipe -> Application
app pipe = serve userAPI $ hoistServer userAPI (`runReaderT` pipe) server
