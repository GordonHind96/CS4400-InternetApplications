{-# LANGUAGE RecordWildCards #-} 
module Client where

import Control.Concurrent.STM
import System.IO              (Handle, hPutStrLn)
import Data.Map (Map)
import Types

data Client = Client
  { clientName     :: ClientName
  , clientID       :: Int
  , clientHandle   :: Handle
  , clientKicked   :: TVar (Maybe String)
  , clientSendChan :: TChan Message
  }
-- >>

-- <<newClient
newClient :: ClientName -> Int -> Handle -> STM Client
newClient name id handle = do
  c <- newTChan
  k <- newTVar Nothing
  return Client { clientName     = name
                , clientID       = id
                , clientHandle   = handle
                , clientSendChan = c
                , clientKicked   = k
                }
-- Send a message to a client.
sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} msg = writeTChan clientSendChan msg
