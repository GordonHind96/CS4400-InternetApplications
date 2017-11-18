
{-# LANGUAGE LambdaCase,RecordWildCards, OverloadedStrings #-}
module Main where

import Control.Concurrent (forkFinally)
import Control.Monad      (forever,replicateM,when,join)
import Network            (PortID(..), accept, listenOn, withSocketsDo)
import System.IO          (hClose)
import Text.Printf        (printf)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM
import Control.Exception        (finally, mask)
import Data.Map (Map)
import qualified Data.Map                 as M
import qualified Data.Set                 as S
import Data.Hashable
import System.IO
import System.Environment
import Data.String
import Text.Printf              (hPrintf, printf)



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
ie "CHAT: 101\nCLIENT_NAME: chaosladdah\nMESSAGE: "hey\n\n""
-}


joinArgs :: Int
joinArgs = 4

leaveArgs :: Int
leaveArgs = 3

disconnectArgs :: Int
disconnectArgs = 3

sendMsgArgs :: Int
sendMsgArgs = 5

endService :: String
endService = "KILL"

newServer :: IO Server
newServer = newTVarIO M.empty

newChannel :: Client -> String -> STM Room
newChannel cli@Client{..} room = do
  clientList <- newTVar $ M.insert clientID cli M.empty
  return Room { roomName = room
                ,roomRef  = (hash room)
                ,clients  = clientList
            } 
retrieveChannel::RoomRef -> Server -> STM (Maybe Room)
retrieveChannel roomRef server = do
	rooms <- readTVar server
	case M.loopup roomRef rooms of
		Nothing -> return Nothing
		Just r -> return $ Just r

joinChannel :: Client -> Server -> String -> IO ()
joinChannel cli@Client{..} serverRooms name = atomically $ do
    roomList <- readTVar serverRooms
    case M.lookup (hash name) roomList of

        Nothing -> do
            room <- newChannel cli name
            let updatedRoomList = M.insert (roomRef room) room roomList
            writeTVar serverRooms updatedRoomList
            response (roomRef room) (roomName room)  
        Just room -> do
            clientList <- readTVar (clients room)
            let newClientList = M.insert clientID cli clientList
            writeTVar (clients room) newClientList
            response (roomRef room) (roomName room)
       where
       	response ref name = sendMessage cli (Response $ "JOINED_CHATROOM:"++name++"\nSERVER_IP:0.0.0.0\nPORT:"++show (fromIntegral port) ++ "\nROOM_REF:" ++ show ref ++"\nJOIN_ID:" ++ show (ref+clientID))

leaveChannel :: Client -> Server -> Int -> IO ()
leaveChannel client@Client{..} server roomRef = leaver client server roomRef (roomRef+clientID)

leaver :: Client -> Server -> RoomRef -> Int -> IO ()
leaver cli@Client{..} server roomRef jonRef = do
	roomList <- atomically $ readTVar server
	case M.lookup roomRef roomList of
		Nothing -> putStrLn "No room with that reference"
		Just room -> do
			atomically $ sendMessage cli (Response $ "LEFT_CHATROOM:"++ show roomRef ++ "\nJOIN_ID:" ++ show joinRef)
			remover
	   where
	   	remover = atomcally $ do
	   		clients <- readTvar (members room)
	   		let roomMembers = M.elems clientSendChan
	   		mapM_ (\client -> sendMessage client notification) roomMembers
	   		let newList = M.delete(hash clientName) clients
	   		writeTvar (members room) newList
	   	notification = (Broadcast $ "CHAT:" ++ (show roomRef)++"\nCLIENT_NAME:" ++ clietName ++ "\nMESSAGE:" ++ clientName ++ "has left the room.\n\n")


deleteChannel :: Server -> Int -> IO ()
deleteChannel server ref = atomically $ do
    list <- readTVar server
    case M.lookup ref list of
        Nothing    -> return ()
        Just room -> do
            let newList = M.delete ref list
            writeTVar server newList

removeClient :: Server -> Client -> IO ()
removeClient serv cli@Client{..} = do
	rooms <- atomically $ readTVar serv
	let roomNames = Prelude.map (\room -> roomName room) (M.elems rooms)
	mapM_ (\room -> kick room) roomNames
   where
   	kick room = do 
   		leaveChannel cli serv (hash room) >> putStrLn (clientName ++ " removed from " ++ room)

runClient :: Server -> Client -> IO ()
runClient serv client@Client{..} = do
    race server receive
    return ()
   where
   	receive = forever $ do
   		msg <- hGetLine clientHandle
   		case words msg of
   			["KILL_SERVICE"] -> do
   				send [endService] endService
   			["JOIN_CHATROOM:",roomName] -> do
   				cmdLineArgs <- getArgs (joinArgs-1)
   				send cmdLineArgs roomName
   			["LEAVE_CHATROOM:",roomRef] -> do
   				cmdLineArgs <- getArgs (leaveArgs-1)
   				send cmdLineArgs roomRef
   			["DISCONNECT:",ip] -> do
   				cmdLineArgs <- getArgs (disconnectArgs-1)
   				send cmdLineArgs ip
   			["CHAT:",roomRef] -> do
   				cmdLineArgs <- getArgs (sendMsgArgs-1)
   				send cmdLineArgs roomRef
   			_  -> throwError
       	   where
       	   	send :: [String] -> String -> IO ()
       	   	send args initialArg = atomically   $ sendMessage client $ Command (map words args) initialArg
       	   	throwError           = atomically   $ sendMessage client $ Error "Error" "Unrecognized Command"
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
     Response msg -> output msg
     Broadcast name msg -> output $ "<" ++ name ++ ">: " ++ msg
     Error heading body -> output $ "->" ++ heading ++ "<-\n" ++ body
     Command msg mainArg -> case msg of
     	[["CLIENT_IP:",_],["PORT:",_],["CLIENT_NAME:",name]] -> do
     		let msgs = "CHAT:"++(show $ (hash mainArg))++"\nCLIENT_NAME:"++clientName++"\nMESSAGE:"++clientName ++ " has entered the room.\n"
     		joinChannel client server mainArg >> notifyRoom (hash mainArg) (broadCast msgs)
     		putStrLn "joined"

     	[["JOIN_ID:",id],["CLIENT_NAME:",name]] -> do
     		leaver client server (read mainArg :: Int) (read id:: Int)
     		return True
     	[["PORT:",_],["CLIENT_NAME:",name]] -> removeClient server client >> return False
     	[["JOIN_ID:",id],["CLIENT_NAME:",name],("MESSAGE:":msgToSend),[]] -> do
     		notifyRoom (read mainArg :: Int) $ Broadcast ("CHAT: " ++ mainArg) ("CLIENT_NAME: " ++ name ++ "\nMESSAGE: "++(unwords msgToSend)++"\n\n")
     		return True
     	[["KILL"]] -> do
     		if mainArg == endService then return False
     		else return True
     	_ -> do
     		atomically   $ sendMessage client $ Error "Error 1" "Unrecognised Args"
     		mapM_ putStrLn $ map unwords msg
     		return True
       where
       	reply replyMsg = atomically $ sendMessage client replyMsg
       	notifyRoom roomRef msg = do
       		roomsList <- atomically $ readTVar server
       		let maybeRoom = M.lookup roomRef roomsList
       		case maybeRoom of
       			Nothing    -> return True
       			Just aRoom -> sendRoomMessage msg aRoom >> return True
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
                output $ "HELO text\nIP: 134.226.214.252" ++"\nPort: " ++ (show port) ++ "\nStudentID: 14311128\n"
                readOp
            ["KILL_SERVICE"] -> output "RIP" >> return ()
            ["JOIN_CHATROOM:",roomName] -> do 
                arguments <- getArgs (joinArgs-1)
                
                case map words arguments of
                    [["CLIENT_IP:",_],["PORT:",_],["CLIENT_NAME:",name]] -> do
                        client <- newClient name (hash name) handle
                        joinChannel client server roomName
                        hPutStrLn handle $ "**Welcome, "++name++"**"
                        hPutStrLn handle $ name++" entered " ++ roomName
                        let msg = "CHAT:"++ (show $ hash roomName) ++ "\nCLIENT_NAME"++ name++"\nMESSAGE:"++ name++"has entered the room.\n\n"
						notifyRoom (hash roomName) $ Broadcast msg
                        runClient server client >> endClient client
                    _ -> output "Unrecognized command" >> readOp
                    where
                    	notifyRoom roomRef msg = do
                    		rooms <- atomically $ readTVar server
                    		let mRoom = M.lookup roomRef rooms
                    		case mRoom of
                    			Nothing -> putStrLn ("No room with ref " ++ (show roomRef)) >> return True
                    			Just room -? sendRoomMessage msg room >> return True
                    	endClient = do
                    		return ()
            _ -> do
                output "Unreconized command" >> readOp
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