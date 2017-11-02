
{-# LANGUAGE LambdaCase,RecordWildCards, OverloadedStrings #-}
module Main where

import 	         Control.Concurrent (forkFinally)
import           Control.Monad      (forever)
import           Network            (PortID(..), accept, listenOn, withSocketsDo)
import         	 System.IO          (hClose)
import           Text.Printf        (printf)
import           Control.Concurrent.Async (race)
import           Control.Concurrent.STM
import           Control.Exception        (finally, mask)
import           Control.Monad
import           Data.Map (Map)
import qualified Data.Map                 as M
import qualified Data.Set                 as S
import           Data.Hashable
import           System.IO
import           Text.Printf              (hPrintf, printf)
import           Network



import Channel
import Client
import Types
import Server  
{-
PROTOCOL:
    -JOIN
        - Client Sends
         JOIN_CHATROOM: [chatroom name]
         CLIENT_IP: [IP Address of client if UDP | 0 if TCP]
         PORT: [port number of client if UDP | 0 if TCP]
         CLIENT_NAME: [string Handle to identifier client user
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


joinArgs :: Int
joinArgs = 4

leaveArgs :: Int
leaveArgs = 3

disconnectArgs :: Int
disconnectArgs = 3

sendMsgArgs :: Int
sendMsgArgs = 4

newServer :: IO Server
newServer = newTVarIO M.empty

newChannel :: Client -> String -> STM Room
newChannel joiner@Client{..} room = do
  clientList <- newTVar $ M.insert clientID joiner M.empty
  return Room { roomName = room
                    , roomRef  = (hashWithSalt 1979 room)
                    , clients  = clientList
            } 

joinChannel :: Client -> Server -> String -> IO ()
joinChannel joiner@Client{..} serverRooms name = atomically $ do
    roomList <- readTVar serverRooms
    case M.lookup (hash name) roomList of
        
        Nothing -> do
            room <- newChannel joiner name

            let updatedRoomList = M.insert (roomRef room) room roomList
            writeTVar serverRooms updatedRoomList
        
        Just aRoom -> do
            clientList <- readTVar (clients aRoom)
            let newClientList = M.insert clientID joiner clientList
            writeTVar (clients aRoom) newClientList

leaveChannel :: String -> Server -> Int -> IO ()
leaveChannel clientName server roomRef = do
    roomList <- readTVar server
    case M.lookup roomRef roomList of
        Nothing    -> putStrLn "No room by that name" >> return ()
        Just room -> do
            clientList <- readTVar (clients room)
            let newList = M.delete (hash clientName) clientList
            writeTVar (clients room) newList
            putStrLn (clientName ++ " left " ++ (roomName room))

deleteChannel :: Server -> Int -> IO ()
deleteChannel server ref = do
    list <- readTVar server
    case M.lookup ref list of
        Nothing    -> return ()
        Just aRoom -> do
            let newList = M.delete ref list
            writeTVar server newList
            putStrLn ("room  " ++ show(ref) ++ " deleted") >> return ()

removeClient :: Server -> String -> IO ()
removeClient serv name = do
    rooms <- readTVar serv
    let roomNames = Prelude.map (\room -> roomName room) (M.elems rooms)
    mapM_ (\room -> kick $ hash room) roomNames
    where
        kick roomRef = do 
            leaveChannel name serv roomRef

runClient :: Server -> Client -> IO ()
runClient serv client@Client{..} = do
    race server receive
    removeClient serv clientName
    return ()
    where
        receive = forever $ do
            msg <- hGetLine clientHandle
            case words msg of
                ["JOIN_CHATROOM:",roomName] -> do
                    cmdLineArgs <- getArgs (joinArgs-1)
                    send cmdLineArgs roomName
                ["LEAVE_CHATROOM:",roomRef] -> do
                    cmdLineArgs <- getArgs (leaveArgs-1)
                    send cmdLineArgs roomRef
                ["DISCONNECT:",ip]          -> do
                    cmdLineArgs <- getArgs (disconnectArgs-1)
                    send cmdLineArgs ip
                ["CHAT:",roomRef]           -> do
                    cmdLineArgs <- getArgs (sendMsgArgs-1)
                    send cmdLineArgs roomRef
                _                           -> throwError
            where
                send :: [String] -> String -> IO ()
                send args initialArg = atomically   $ sendMessage client $ Command (map words args) initialArg
                throwError           = atomically   $ sendMessage client $ Error "Error" "Unrecognised Command"
                getArgs n            = replicateM n $ hGetLine clientHandle
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


handleClient :: Handle -> Server -> IO ()
handleClient handle server = do
    hSetNewlineMode handle universalNewlineMode
    hSetBuffering handle NoBuffering
    readOp
    return ()
   where
    readOp = do
        op <- hGetLine handle
        case words op of
            ["HELO",_] -> do
                output $ "HELO text\nIP: 0\nPort: " ++ (show port) ++ "\nStudentID: 14311128\n"
                readOp
            ["KILL_SERVICE"] -> output "RIP" >> return ()
            ["JOIN_CHATROOM:",roomName] -> do
                arguments <- getArgs (joinArgs-1)
                --output roomName
                case map words arguments of
                    [["CLIENT_IP:",_],["PORT:",_],["CLIENT_NAME:",name]] -> do

                        client <- newClient name (hash name) handle
                        joinChannel client server roomName
                        hPutStrLn handle $ "***Welcome, "++name++"***"
                        hPutStrLn handle $ name++" entered " ++ roomName
                        runClient server client `finally` (removeClient server name >> return ())
                    _ -> output "Unreconized command" >> readOp
            _ -> do
                readOp
            where
                output = hPutStrLn handle 
                getArgs n = replicateM n $ hGetLine handle

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
