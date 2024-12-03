{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- import MongoFFI
import Control.Exception
import Control.Monad
import Database.MongoDB
import MyLib
import Network.Socket
import Network.Wai.Handler.Warp
import Prelude.Compat
import Prelude ()

userName :: Username
userName = "firstTest"
userPass :: Password
userPass = "testPassword"
hostName :: HostName
hostName = "test.u83l8.mongodb.net"

main :: IO ()
main = do
  print ("setting mongodb" :: String)

  {-
    let host = Host "localhost" (PortNumber 27017)
    pipe <- connect host
  -}
  -- {-
  repSet <- openReplicaSetSRV hostName
  print ("1" :: String)
  pipe <- primary repSet
  print ("2" :: String)
  is_auth <- access pipe master "admin" $ auth userName userPass
  print ("3" :: String)
  unless is_auth (throwIO $ userError "Authentication failed :(")

  print ("finished connnecting mongodb" :: String)
  -- let host = Host "localhost" (PortNumber 27017)
  {-
  let connectionStr = "mongodb+srv://firstTest:testPassword@test.u83l8.mongodb.net/"
  case parseURI connectionStr of Just uri -> do
                                  pipe <- connect uri
                                  print ("starting server" :: String)
                                  run 8081 (app pipe)
  -}

  -- (((\case Just uri -> read (show uri) :: Host). parseURI) connectionStr)
  -- -}
  print ("starting server" :: String)
  run 8081 (app pipe)
