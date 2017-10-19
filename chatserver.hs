
{-# LANGUAGE RecordWildCards #-}
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
type ClientName = String
data Client = Client
  { clientName     :: ClientName
	, clientHandle :: Handle
	, clientIP :: Maybe Int
	, clientPort :: Maybe Int
	, clientSendChan :: TChan Message
}
data Message = Notice String
	| Tell ClientName String
	| Broadcast ClientName String
	| Command String

newClient :: ClientName -> Handle -> Maybe Int->Maybe Int ->STM Client
newClient name handle ip port = do
	c <- newTChan
	return Client { clientName = name,
	clientHandle = handle,
	clientIP = ip,
	clientPort = port,
	clientSendChan = c
}

data Server = Server
  { clients :: TVar (Map ClientName Client)
  }
newServer :: IO Server
newServer = do
	c <- newTVarIO Map.empty
	return Server {clients = c}

broadcast :: Server -> Message -> STM ()
broadcast Server{..} msg = do
  clientmap <- readTVar clients
  mapM_ (\client -> sendMessage client msg) (Map.elems clientmap)

sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} msg =
  writeTChan clientSendChan msg
-- >>

-- <<sendToName
sendToName :: Server -> ClientName -> Message -> STM Bool
sendToName server@Server{..} name msg = do
  clientmap <- readTVar clients
  case Map.lookup name clientmap of
    Nothing     -> return False
    Just client -> sendMessage client msg >> return True
-- >>

tell :: Server -> Client -> ClientName -> String -> IO ()
tell server@Server{..} Client{..} who msg = do
  ok <- atomically $ sendToName server who (Tell clientName msg)
  if ok
     then return ()
     else hPutStrLn clientHandle (who ++ " is not connected.")

main :: IO ()
main = withSocketsDo $ do
  server <- newServer
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  forever $ do
      (handle, host, port) <- accept sock
      printf "Accepted connection from %s: %s\n" host (show port)
      forkFinally (talk handle server) (\_ -> hClose handle)

port :: Int
port = 44444

checkAddClient :: Server -> ClientName -> Handle -> IO (Maybe Client)
checkAddClient server@Server{..} name handle = atomically $ do
  clientmap <- readTVar clients
  if Map.member name clientmap
    then return Nothing
    else do client <- newClient name handle Nothing Nothing 
            writeTVar clients $ Map.insert name client clientmap
            broadcast server  $ Notice (name ++ " has connected")
            return (Just client)

removeClient :: Server -> ClientName -> IO ()
removeClient server@Server{..} name = atomically $ do
  modifyTVar' clients $ Map.delete name
  broadcast server $ Notice (name ++ " has disconnected")

talk :: Handle -> Server -> IO ()
talk handle server@Server{..} = do
  hSetNewlineMode handle universalNewlineMode
      -- Swallow carriage returns sent by telnet clients
  hSetBuffering handle LineBuffering
  readName
 where
  readName = do
    hPutStrLn handle "What is your name?"
    name <- hGetLine handle
    if null name
      then readName
      else mask $ \restore -> do        -- 1
             ok <- checkAddClient server name handle
             case ok of
               Nothing -> restore $ do  -- 2
                  hPrintf handle
                     "The name %s is in use, please choose another\n" name
                  readName
               Just client ->
                  restore (runClient server client) -- 3
                      `finally` removeClient server name

runClient :: Server -> Client -> IO ()
runClient serv@Server{..} client@Client{..} = do
  race server receive
  return ()
 where
  receive = forever $ do
    msg <- hGetLine clientHandle
    atomically $ sendMessage client (Command msg)

  server = join $ atomically $ do
        msg <- readTChan clientSendChan
        return $ do
            continue <- handleMessage serv client msg
            when continue $ server

handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage server client@Client{..} message =
  case message of
     Notice msg         -> output $ "*** " ++ msg
     Tell name msg      -> output $ "*" ++ name ++ "*: " ++ msg
     Broadcast name msg -> output $ "<" ++ name ++ ">: " ++ msg
     Command msg ->
       case words msg of
           "/tell" : who : what -> do
               tell server client who (unwords what)
               return True
           ["/quit"] ->
               return False
           ('/':_):_ -> do
               hPutStrLn clientHandle $ "Unrecognized command: " ++ msg
               return True
           _ -> do
               atomically $ broadcast server $ Broadcast clientName msg
               return True
 where
   output s = do hPutStrLn clientHandle s; return True