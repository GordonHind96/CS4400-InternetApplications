
{-# LANGUAGE LambdaCase, RecordWildCards, TupleSections #-} 
module Main where
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import qualified Data.Map as Map
import Data.Map (Map)
import System.IO
import Control.Exception
import Network
import Control.Monad
import Text.Printf

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
--CLIENT CODE
type ClientName = String
data Client = Client
  { clientName     :: ClientName
  , clientHandle :: Handle
  , clientIP :: Maybe Int
  , clientPort :: Maybe Int
  , clientSendChan :: TVar (Maybe (Room, TChan Message))
}
newClient :: ClientName -> Handle -> Maybe Int->Maybe Int ->STM Client
newClient name handle ip port = do
  c <- newTChan
  return Client { clientName = name,
  clientHandle = handle,
  clientIP = ip,
  clientPort = port,
  clientSendChan = c
}

clientGetChan :: Client -> STM (Maybe Channel)
clientGetChan = (fmap . fmap) fst . readTVar . clientChan


clientGetTChan :: Client -> STM (Maybe (TChan Message))
clientGetTChan = (fmap . fmap) snd . readTVar . clientChan

clientReadMessage :: Client -> STM Message
clientReadMessage client =  readBroadcast
  where
    readBroadcast :: STM Message
    readBroadcast = clientGetTChan client >>= maybe retry readTChan

clientLeaveRoom :: LeaveReason -> CLient -> STM ()
clientLeaveRoom reason client@Client{..} = readTvar clientChan >>= \case
  Nothing -> notify client "You're not in a room"
  Just(curRoom, _) -> do
    notify client $ "TODO PUT IN PROTOCOL"
    roomRemoveClient reson curRoom clientName
    writeTVar clientSendChan Nothing

clientJoinRoom :: JoinReason -> Client -> Room -> STM (Maybe Channel)
clientJoinRoom reason client@Client{..} newChan = readTVar clientChan >>= \case
    -- Not in a channel - just join the new one.
    Nothing -> do
        clientJoinRoom'
        return Nothing
    -- In a channel - leave the current one. Old broadcast chan will get
    -- garbage collected.
    Just (curChan, _) 
        | roomName curRoom == roomName newRoom -> do
            notify client "You're already in that channel."
            return Nothing
        | otherwise -> do
            -- Join other channel first, because chanRemoveClient will broadcast a
            -- leave message. We don't want to see that message ourselves, because
            -- we know we've left the channel.
            clientJoinRoom'
            roomRemoveClient LeaveReasonLeft curRemove clientName
            return (Just curRemove)
  where
    clientJoinRoom' :: STM ()
    clientJoinRoom' = do
        notify client $ "TODO PROTOCL " ++ channelName newChan

        -- Make this call before duping the channel, so we don't see our own
        -- join message broadcast to the channel.
        roomAddClient reason newRoom clientName
        dupTChan (roomBroadcastChan newChan) >>=
            writeTVar clientChan . Just . (newChan,)

clientBroadcastToRoom :: Client -> String -> STM ()
clientBroadcastToRoom client@Client{..} msg = 
    clientGetChan client >>= maybe (return ()) (\c -> chanBroadcast c clientName msg)

notify :: Client -> String -> STM ()
notify client = sendMessage client . Notice

--ROOM CODE
type RoomName = String
type RoomRef = Int
data Room = Room
  { roomName :: RoomName
  , roomRef :: RoomRef
  , roomClients :: TVar (set ClientName)
  , rommBroadcastChan :: TChan Message
}
data LeaveReason
    = LeaveReasonLeft
    | LeaveReasonDisconnected

data JoinReason
    = JoinReasonJoined
    | JoinReasonConnected
newRoom :: RoomName -> STM Room
newRoom name = Room name <$> newTVar S.empty <*> newBroadcastTChan

roomRemoveClient  :: LeaveReason -> RoomRef -> RoomName -> STM()
roomRemoveClient LeaveReasonLeft = roomRemoveClient' roomNotifyHasLeft
roomRemoveClient LeaveReasonDisconnected = roomRemoveClient' roomNotifyHasDisconnected

roomRemoveClient' :: (Room -> ClientName -> STM ()) -> Room -> ClientName -> STM ()
roomRemoveClient' notifyAction rm@Room{..} name = do
    notifyAction rm name
    modifyTVar RoomClients . S.delete $ name

roomAddClient :: JoinReason -> Room -> ClientName -> STM ()
roomAddClient JoinReasonJoined    = roomAddClient' roomNotifyHasJoined
roomAddClient JoinReasonConnected = roomAddClient' roomNotifyHasConnected

roomAddClient' :: (Room -> ClientName -> STM ()) -> Room -> ClientName -> STM ()
roomAddClient' notifyAction rm@Room{..} name = do
    notifyAction rm name
    modifyTVar roomClients . S.insert $ name

roomNotifyHasLeft :: Room -> ClientName -> STM ()
roomNotifyHasLeft room name = roomNotify room (name ++ " has left the channel.")

-- Notify the channel a client has disconnected.
roomNotifyHasDisconnected :: Room -> ClientName -> STM ()
roomNotifyHasDisconnected room name = roomNotify room (name ++ " has disconnected.")

-- Notify the channel a client has joined.
roomNotifyHasJoined :: Room -> ClientName -> STM ()
roomNotifyHasJoined room name = roomNotify room (name ++ " has joined the channel.")

-- Notify the channel a client has connected.
roomNotifyHasConnected :: Room -> ClientName -> STM ()
roomNotifyHasConnected room name = roomNotify room (name ++ " has connected.")

-- Send a Message to the channel.
roomMessage :: Room -> Message -> STM ()
roomMessage = writeTChan . roomBroadcastChan

-- Send a Broadcast to the channel, from a client.
roomBroadcast :: Room -> ClientName -> String -> STM ()
roomBroadcast room@Room{..} clientName msg = roomMessage room (Broadcast roomRef clientName msg)

-- Send a Notice to the channel.
roomNotify :: Room -> String -> STM ()
roomNotify room = roomMessage room . Notice

--SERVER CODE
data Server = Server
    { serverRooms :: TVar (Map RoomName Rooom)
    , serverClients  :: TVar (Map ClientName Client)
    }
newServer :: IO Server
newServer = do 
  server <- Server <$> newTVar M.empty <*> newTVar M.empty
  return server

addRoom :: Server -> RoomName -> STM ()
addRoom Server{..} name = new Room name >>= modifyTVar serverRooms . M.insert name

removeClient :: Server -> Client -> IO ()
removeCLient Server{..} client@Client{..} = atomically $ do
  clientLeaveRoom LeaveReasonDisconnected client
  modifyTVar' serverClients $ M.delete clientName

lookupRoom :: Server -> RoomName -> STM (Maybe Room)
lookupRoom Server{..} name = M.lookup name <$> readTVar serverRooms

lookupOrCreateRoom :: Server -> RoomName -> STM Room
lookupOrCreateRoom server@Server{..} name = lookupRoom server name >>= \case
    Nothing -> do
        room <- newRoom name
        modifyTVar serverChannels . M.insert name $ room
        return room
    Just room -> return room

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

runClient :: Server -> Client -> IO ()
runClient server@Server{..} client@{..} = do
  race_ serverThread receiveThread
  return ()
where
  receiveThread :: IO ()
  receiveThread = forever $ do
    msg <- hGetLine clientHandle
    atomically $ sendMessage client (Command msg)

  serverThread :: IO ()
  serverThread = join $ atomiacally $ do
    msg <- clientReadMessage client
    return $ do
      continue <- handleMessage server clinet msg
      when continue $ serverThread

handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage _ client (Notice msg)    = sendBytes client ("*** " ++ msg) >> return True
handleMessage _ client (Broadcast chName clName msg)
    | clName == clientName client = return True  -- Ignore messages from self.
    | otherwise                 = sendBytes client (printf "[%s] %s: %s" chName clName msg) >> return True
handleMessage server client@Client{..} (Command msg) = case words msg of
    "/join":which:[]    -> joinChannel server client which                    >> return True
    "/j"   :which:[]    -> joinChannel server client which                    >> return True

    "/quit":[]          ->                                                       return False
    "/q"   :[]          ->                                                       return False

    ('/':_):_           -> sendBytes client ("Unrecognized command: " ++ msg) >> return True
    _                   -> atomically (clientBroadcastToChannel client msg)   >> return True

joinChannel :: Server -> Client -> RoomName -> IO ()
joinChannel server@Server{..} client name = atomically $
    lookupOrCreateRoom server name >>= clientJoinRoom JoinReasonJoined client
        

main :: IO ()
main = withSocketsDo $ do
    server <- newServer
    sock <- listenOn (PortNumber (fromIntegral port))
    _ <- printf "Listening on port %d\n" port

    forever $ do
        (handle, host, port') <- accept sock
        _ <- printf "Accepted connection from %s: %s\n" host (show port')
        forkFinally (handleClient handle server) (\_ -> hClose handle)

port :: Int
port = 44444