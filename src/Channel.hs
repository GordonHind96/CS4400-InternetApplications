{-# LANGUAGE RecordWildCards #-}

module Channel where

import           Control.Applicative
import           Control.Concurrent.STM
import           System.IO
import           Data.Map (Map)
import qualified Data.Map               as M
import           Data.Set               (Set)
import qualified Data.Set               as S

import Types
import Client

data Room = Room
  { roomName    :: RoomName
  , roomRef     :: Int
  , clients     :: TVar (Map Int Client)
  }

type RoomList = TVar (Map Int Room)

sendRoomMessage :: Message -> Room -> IO ()
sendRoomMessage msg room@Room{..} = atomically $ do
  clientList <- readTVar clients
  let roomClients = M.elems clientList
  mapM_ (\aClient -> sendMessage aClient msg) roomClients