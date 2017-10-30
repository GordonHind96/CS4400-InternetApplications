{-# LANGUAGE LambdaCase,RecordWildCards #-}

module Server where

import           Control.Applicative
import           Control.Concurrent.Async (race_)
import           Control.Concurrent.STM
import           Control.Exception        (finally, mask)
import           Control.Monad
import           Data.Map (Map)
import qualified Data.Map                 as M
import qualified Data.Set                 as S
import           Data.Hashable
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
port :: Int
port = 44444

data Server = Server
    { serverClients :: TVar (Map ClientName Client)
    , serverRooms :: RoomList 
}
 

joinArgs :: Int
joinArgs = 4

leaveArgs :: Int
leaveArgs = 3

disconnectArgs :: Int
disconnectArgs = 3

sendMsgArgs :: Int
sendMsgArgs = 4

newServer :: IO Server
newServer = do
  c <- newTVarIO M.empty
  r <- newTVarIO M.empty
  return Server { serverClients = c
                , serverRooms   = r
                }

newChannel :: Client -> String -> STM Room
newChannel joiner@Client{..} room = do
  clientList <- newTVar $ M.insert clientID joiner M.empty
  return Room { roomName = room
                    , roomRef  = (hashWithSalt 1979 room)
                    , clients  = clientList
            }  
broadcast :: Server -> Message -> STM ()
broadcast Server{..} msg = do
  clientmap <- readTVar serverClients
  mapM_ (\client -> sendMessage client msg) (M.elems clientmap)


                  
sendRoomMessage :: Server -> Int -> Message -> STM ()
sendRoomMessage server@Server{..} roomRef msg = do
    roomsList <- readTVar serverRooms
    case M.lookup roomRef roomsList of
        Nothing       -> return ()
        Just chatroom -> do
            clients <- readTVar (clients chatroom)
            mapM_ (\x -> sendMessage x msg) (M.elems clients) >> return ()

checkAddClient :: Server -> ClientName -> Handle -> IO(Maybe Client)
checkAddClient server@Server{..} name handle = atomically $ do
  clientmap <- readTVar serverClients
  if M.member name clientmap
    then return Nothing
    else do 
        newClientID <- 1 + (M.size clientmap)
        client <- newClient name newClientID handle
        writeTVar serverClients $ M.insert name client clientmap
        broadcast server  $ Notice (name ++ " has connected")
        return (Just client)

removeClient :: Server -> ClientName -> IO ()
removeClient server@Server{..} name = atomically $ do
  modifyTVar' serverClients $ M.delete name
  broadcast server $ Notice (name ++ " has disconnected")

runClient :: Server -> Client -> IO ()
runClient serv@Server{..} client@Client{..} = do
  race_ server receive
  return ()
 where
  receive = forever $ do
    msg <- hGetLine clientHandle
    case words msg of
      ["JOIN_CHATROOM:",roomName] -> do
        cmdLineArgs <- repeat $ (fromIntegral joinArgs)-1
        send cmdLineArgs roomName
      ["LEAVE_CHATROOM:",roomRef] -> do
        cmdLineArgs <- repeat $ (fromIntegral leaveArgs)-1
        send cmdLineArgs roomRef
      ["DISCONNECT:",ip] -> do
        cmdLineArgs <- repeat $ (fromIntegral disconnectArgs)-1
        send cmdLineArgs ip
      ["CHAT:",roomRef] -> do
        cmdLineArgs <- repeat $ (fromIntegral sendMsgArgs)-1
        send cmdLineArgs roomRef
      ["HELO",txt] -> do
        hello client
      _                             -> throwError
      where
        send :: [String] -> String -> IO ()
        send args initialArg = atomically $ do sendMessage client $ Command (map words args) initialArg
        throwError = atomically $ do sendMessage client $ Error "Error 1" "Unrecognised Command"
        repeat nTimes = replicateM nTimes $ hGetLine clientHandle
        hello client = atomically $ do sendMessage client $ Tell clientName $ "HELO text\nIP:0.0.0.0\nPort:" ++ (show port) ++ "\nStudentID:14311128\n\n"

  server = join $ atomically $ do
    k <- readTVar clientKicked
    case k of
      Just reason -> return $
        hPutStrLn clientHandle $ "You have been kicked: " ++ reason
      Nothing -> do
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
     Error heading body -> output $ "->" ++ heading ++ "<-\n" ++ body
     Command msg mainArg ->
       case msg of
           [["CLIENT_IP:",_],["PORT:",_],["CLIENT_NAME:",name]] -> do
               --Join room 'mainArg'
               return True
           [["JOIN_ID:",id],["CLIENT_NAME:",name]] -> do
               --leave room, 'mainArg' = room_ref
               return True
           [["PORT:",_],["CLIENT_NAME:",name]] ->
               --disconnect from server by returning false (see continue in runclient)
               return False
           [["JOIN_ID:",id],["CLIENT_NAME:",name],["MESSAGE:",msgToSend]] -> do
               --send this message, mainArg = room_ref
               return True
           _ -> do
               --Ignore unrecognised commands
               return True
 where
   output s = do hPutStrLn clientHandle s; return True

