{-# LANGUAGE RecordWildCards #-} 
module Client where

import Control.Concurrent.STM
import System.IO              (Handle, hPutStrLn)
import Data.Map (Map)
import qualified Data.Map as M
import Types

data Client = Client
  { clientName     :: ClientName
  , clientID       :: Int
  , clientHandle   :: Handle
  , clientSendChan :: TChan Message
  }

newClient :: ClientName -> Int -> Handle -> IO Client
newClient name id handle = do
  chan <- newTChanIO
  return Client { clientName     = name
                , clientID       = id
                , clientHandle   = handle
                , clientSendChan = chan
                }

sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} = writeTChan clientSendChan

