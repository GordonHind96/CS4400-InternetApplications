
{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Server where

import           Control.Applicative
import           Control.Concurrent.Async (race_)
import           Control.Concurrent.STM
import           Control.Exception        (finally, mask)
import           Control.Monad
import           Data.Map                 (Map)
import qualified Data.Map                 as M
import qualified Data.Set                 as S
import           System.IO
import           Text.Printf              (hPrintf, printf)

import Channel
import Client
import Types
{-
PROTOCOL:
	-JOIN
		- Client Sends
		 JOIN_CHATROOM: [chatroom name]
		 CLIENT_IP: [IP Address of client if UDP | 0 if TCP]
		 PORT: [port number of client if UDP | 0 if TCP]
		 CLIENT_NAME: [string Handle to identifier client user]

ie"JOIN_CHATROOM: GoTDiscussion\nClient_IP:  \nPort:  \nCLIENT_NAME: chaosladdah"
		--Server Sends
		  JOINED_CHATROOM: [chatroom name]
		  SERVER_IP: [IP address of chat room]
		  PORT: [port number of chat room]
		  ROOM_REF: [integer that uniquely identifies chat room on server]
		  JOIN_ID: [integer that uniquely identifies client joining]
ie"JOINED_CHATROOM: GoTDiscussion\nSERVER_IP: 127.0.0.1\nPORT: 1234\n
ROOM_REF: 101\nJOIN_ID: 71"
		-Server should also send message to chat room saying "x has joined room"
		-Failure
		  ERROR_CODE: [integer]
		  ERROR_DESCRIPTION: [string describing error]
	-LEAVING
		-Client Sends
		  LEAVE_CHATROOM: [ROOM_REF]
		  JOIN_ID: [integer previously provided by server on join]
		  CLIENT_NAME: [string Handle to identifier client user]
i.e "LEAVE_CHATROOM: 101\n JOIN_ID: 71\nCLIENT_NAME: chaosladda"
		-Server Sends
		 LEFT_CHATROOM: [ROOM_REF]
		 JOIN_ID: [integer previously provided by server on join]
ie "LEFT_CHATROOM: 101\nJOIN_ID: 71"
		-Server also sends message "X has left room"
On sending of this LEFT_CHATROOM message, the server should send no further 
CHAT messages to te client in respect of the chat room left. Note however,
that the server should complete the transmission of any chat message in 
progress before sendiing the LEFT_CHATROOM message. Thus, the LEFT_CHATROOM
message might not be the immediate response to a LEAVE_CHATROOM request
, but should interupt chat flow at the next available opportunity.
Note also, that if a client has joined more than one chat room, the
server should continue to send chat messages in respect of other chat rooms 
that the client remains connected to. Thus, a LEAVE_CHATROOM message should 
not cause the server to terminate the client/server socket connection.
	-DISCONNECTING
		-Client Sends
		  DISCONNECT: [IP address of client if UDP | 0 if TCP]
		  PORT: [port number of client it UDP | 0 id TCP]
		  CLIENT_NAME: [string handle to identify client user]
ie "DISCONNECT:  \nPORT:  \nCLIENT_NAME: chaosladdah"
	-MESSAGING
		-Client Seds
		 CHAT: [ROOM_REF]
		 JOIN_ID: [integer identifying client to server]
		 CLIENT_NAME: [string identifying client user]
		 MESSAGE: [string terminated with '\n\n']
ie "CHAT: 101\nJOIN_ID: 71\nCLIENT_NAME: chaosladdah\nMESSAGE: "hey\n\n""
		-Server should send to every client in room
		  CHAT: [ROOM_REF]
		  CLIENT_NAME: [string identifying client user]
		  MESSAGE: [string terminated with '\n\n']
ie "CHAR: 101\nCLIENT_NAME: chaosladdah\nMESSAGE: "hey\n\n""
-}
data Server = Server
    { serverChannels :: TVar (Map ChannelName Channel)
    , serverClients  :: TVar (Map ClientName Client)
    }

newServer :: IO Server
newServer = atomically $ do
    server <- Server <$> newTVar M.empty <*> newTVar M.empty
    addChannel server defaultChannelName
    return server

defaultChannelName :: ChannelName
defaultChannelName = "General"

-- Add a new channel. Assumes a channel with the same name does not already exist.
addChannel :: Server -> ChannelName -> STM ()
addChannel Server{..} name = newChannel name >>= modifyTVar serverChannels . M.insert name

-- Look up a channel on the server, by name.
lookupChannel :: Server -> ChannelName -> STM (Maybe Channel)
lookupChannel Server{..} name = M.lookup name <$> readTVar serverChannels

-- Look up or create a channel on the server and return it.
lookupOrCreateChannel :: Server -> ChannelName -> STM Channel
lookupOrCreateChannel server@Server{..} name = lookupChannel server name >>= \case
    Nothing -> do
        chan <- newChannel name
        modifyTVar serverChannels . M.insert name $ chan
        return chan
    Just chan -> return chan

-- Look up a client on the server, by name.
lookupClient :: Server -> ClientName -> STM (Maybe Client)
lookupClient Server{..} name = M.lookup name <$> readTVar serverClients

-- Remove a client from the server.
removeClient :: Server -> Client -> IO ()
removeClient Server{..} client@Client{..} = atomically $ do
    clientLeaveChannel LeaveReasonDisconnected client
    modifyTVar' serverClients $ M.delete clientName


-- Main entry point to communication with a client.
handleClient :: Handle -> Server -> IO ()
handleClient handle server@Server{..} = do
    -- Swallow carriage returns sent by telnet clients.
    hSetNewlineMode handle universalNewlineMode
    hSetBuffering handle LineBuffering
    readName
  where
    readName :: IO ()
    readName = do
        hPutStrLn handle "What is your name?"
        name <- hGetLine handle
        if null name
            then readName
            else mask $ \restore -> tryAddClient server name handle >>= \case
                Nothing -> restore $ do
                    _ <- hPrintf handle "The name '%s' is in use, please choose another." name
                    readName
                Just client ->
                    restore (runClient server client)
                        `finally` removeClient server client


-- Try to add a client to the server; fail if the requested name is taken.
tryAddClient :: Server -> ClientName -> Handle -> IO (Maybe Client)
tryAddClient server@Server{..} name handle = atomically $ do
    clients <- readTVar serverClients
    if M.member name clients
        then return Nothing
        else do
            client <- newClient name handle
            notify client welcomeMsg

            Just generalChat <- lookupChannel server defaultChannelName    -- Assume "General" chat always exists.
            _ <- clientJoinChannel JoinReasonConnected client generalChat  -- This will always be Nothing

            writeTVar serverClients $ M.insert name client clients
            return (Just client)

welcomeMsg :: String
welcomeMsg = unlines
    [ "Welcome to the server! Available commands:"
    , "/whisper name msg - whisper 'msg' to 'name'"
    , "/join    name     - join channel 'name'"
    , "/users            - list the users in the current channel"
    , "/whoami           - list your name and channel"
    , "/kick    name     - kick 'name'"
    , "/quit             - quit the server"
    ]

runClient :: Server -> Client -> IO ()
runClient server@Server{..} client@Client{..} = do
    race_ serverThread receiveThread
    return ()
  where
    receiveThread :: IO ()
    receiveThread = forever $ do
        msg <- hGetLine clientHandle
        atomically $ sendMessage client (Command msg)

    serverThread :: IO ()
    serverThread = join $ atomically $ do
        kicked <- readTVar clientKicked
        case kicked of
            Just (kicker,reason) -> return $
                sendBytes client $ "You have been kicked by " ++ kicker ++ ": " ++ reason
            Nothing -> do
                msg <- clientReadMessage client
                return $ do
                    continue <- handleMessage server client msg
                    when continue $ serverThread

handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage _ client (Notice msg)    = sendBytes client ("*** " ++ msg) >> return True
handleMessage _ client (Tell name msg) = sendBytes client (name ++ " whispers: " ++ msg) >> return True
handleMessage _ client (Broadcast chName clName msg)
    | clName == clientName client = return True  -- Ignore messages from self.
    | otherwise                 = sendBytes client (printf "[%s] %s: %s" chName clName msg) >> return True
handleMessage server client@Client{..} (Command msg) = case words msg of
    "/whisper":who:what -> whisper server client who (unwords what)           >> return True
    "/w"      :who:what -> whisper server client who (unwords what)           >> return True

    "/join":which:[]    -> joinChannel server client which                    >> return True
    "/j"   :which:[]    -> joinChannel server client which                    >> return True

    "/users":[]         -> users client                                       >> return True

    "/whoami":[]        -> whoami client                                      >> return True

    "/kick":who:why     -> kick server client who (unwords why)               >> return True
    "/k"   :who:why     -> kick server client who (unwords why)               >> return True

    "/quit":[]          ->                                                       return False
    "/q"   :[]          ->                                                       return False

    ('/':_):_           -> sendBytes client ("Unrecognized command: " ++ msg) >> return True
    _                   -> atomically (clientBroadcastToChannel client msg)   >> return True

whisper :: Server -> Client -> ClientName -> String -> IO ()
whisper server from@Client{..} to msg = join $ atomically $ lookupClient server to >>= maybe noTell yesTell
  where
    noTell :: STM (IO ())
    noTell = return $ sendBytes from (to ++ " is not connected.")

    yesTell :: Client -> STM (IO ())
    yesTell client = do
        sendMessage client (Tell clientName msg)
        return (return ())

-- Handle a client joining a channel. Possibly purge the client's old channel
-- from the server, if he/she was the last person in it.
joinChannel :: Server -> Client -> ChannelName -> IO ()
joinChannel server@Server{..} client name = atomically $
    lookupOrCreateChannel server name >>= clientJoinChannel JoinReasonJoined client >>= \case
        Nothing      -> return ()
        Just oldChan -> possiblyDeleteChannel oldChan
  where
    -- Delete a channel if it's empty, but not if it's the default chat channel.
    possiblyDeleteChannel :: Channel -> STM ()
    possiblyDeleteChannel Channel{..}
        | channelName == defaultChannelName = return ()
        | otherwise = do
            clients <- readTVar channelClients
            when (S.null clients) $
                modifyTVar serverChannels (M.delete channelName)

users :: Client -> IO ()
users client = atomically $ clientGetChan client >>= \case
    Nothing -> notify client "You aren't in a channel."
    Just chan -> readTVar (channelClients chan) >>= notify client . show . S.toAscList

whoami :: Client -> IO ()
whoami client@Client{..} = atomically $ do
    msgSuffix <- maybe (", not in any channel.")
                       ((" in channel " ++) . channelName)
                       <$> clientGetChan client
    notify client $ printf "You are %s%s" clientName msgSuffix

kick :: Server -> Client -> ClientName -> String -> IO ()
kick Server{..} kicker kickee reason = atomically $
    readTVar serverClients >>= maybe noKick yesKick . M.lookup kickee
  where
    noKick :: STM ()
    noKick = notify kicker (kickee ++ " is not connected")

    yesKick :: Client -> STM ()
    yesKick Client{..} = do
      writeTVar clientKicked $ Just (clientName, reason)
      notify kicker $ "You kicked " ++ kickee