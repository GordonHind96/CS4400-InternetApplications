{-# LANGUAGE RecordWildCards #-}

module Channel where

import           Control.Applicative
import           Control.Concurrent.STM
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

