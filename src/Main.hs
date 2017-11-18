{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.STM
import Control.Concurrent
import GHC.Conc
import Control.Concurrent.Async
import qualified Data.Map as M
import qualified Data.List as List
import Data.Hashable
import System.IO
import Control.Exception
import Network
import Control.Monad
import Text.Printf

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

newServer :: IO Server
newServer = newTVarIO M.empty


newRoom :: Client -> String -> STM Room
newRoom cli@Client{..} room = do
  clientList <- newTVar $ M.insert clientID cli M.empty
  return Room { roomName = room
                , roomRef  = hash room
                , clients  = clientList
              }

getRoom :: Int -> Server -> STM (Maybe Room)
getRoom roomRef serv = do
  roomlist <- readTVar serv
  case M.lookup roomRef roomlist of
   Nothing -> return Nothing
   Just r  -> return $ Just r

joinRoom :: Client -> Server -> String -> IO ()
joinRoom cli@Client{..} rooms name = atomically $ do
  roomList <- readTVar rooms
  case M.lookup (hash name) roomList of
    Nothing -> do
      room <- newRoom cli name
      let updatedRoomList = M.insert (roomRef room) room roomList
      writeTVar rooms updatedRoomList
      sendResponse (roomRef room) (roomName room)
    Just aRoom -> do
      clientList <- readTVar (clients aRoom)
      let newClientList = M.insert clientID cli clientList
      writeTVar (clients aRoom) newClientList
      sendResponse (roomRef aRoom) (roomName aRoom)
    where
     sendResponse ref name = sendMessage cli (Response $ "JOINED_CHATROOM:"++name++"\nSERVER_IP:0.0.0.0\nPORT:"++show (fromIntegral port) ++ "\nROOM_REF:" ++ show ref ++"\nJOIN_ID:" ++ show (ref+clientID))

leaveRoom :: Client -> Server -> Int -> IO ()
leaveRoom client@Client{..} server roomRef = leaver client server roomRef (roomRef+clientID)

leaver :: Client -> Server -> Int -> Int -> IO ()
leaver client@Client{..} server roomRef joinRef = do
  roomList <- atomically $ readTVar server
  case M.lookup roomRef roomList of
    Nothing    -> putStrLn "No room with that reference" 
    Just aRoom -> do
      atomically $ sendMessage client (Response $ "LEFT_CHATROOM:" ++ show roomRef ++ "\nJOIN_ID:" ++ show joinRef)
      remover
      putStrLn $ clientName++" left " ++ (roomName aRoom)
      where
       remover = atomically $ do
         clientList <- readTVar (clients aRoom)
         let roomClients = M.elems clientList
         mapM_ (\aClient -> sendMessage aClient notification) roomClients
         let newList = M.delete (hash clientName) clientList
         writeTVar (clients aRoom) newList
       notification = (Broadcast $ "CHAT:" ++ (show roomRef) ++ "\nCLIENT_NAME:" ++ clientName ++ "\nMESSAGE:" ++ clientName ++ " has left the room.\n")

deleteRoom :: Server -> Int -> IO ()
deleteRoom serv ref = atomically $ do 
  list <- readTVar serv
  case M.lookup ref list of
    Nothing    -> return ()
    Just aRoom -> do
      let newList = M.delete ref list
      writeTVar serv newList

port :: Int
port = 44444

joinArgs :: Int
joinArgs = 4

leaveArgs :: Int
leaveArgs = 3

disconnectArgs :: Int
disconnectArgs = 3

sendMsgArgs :: Int
sendMsgArgs = 5

killService :: String
killService = "KILL"


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
       ["HELO","BASE_TEST"] -> do
         echo $ "HELO text\nIP:134.226.60.34\nPort:" ++ (show port) ++ "\nStudentID:14311128\n"
         readOp
       ["KILL_SERVICE"] -> output "RIP" >> return ()
       ["JOIN_CHATROOM:",roomName] -> do
         arguments <- getArgs (joinArgs-1)
         case map words arguments of
           [["CLIENT_IP:",_],["PORT:",_],["CLIENT_NAME:",name]] -> do
             client <- newClient name (hash name) handle
             joinRoom client server roomName
             putStrLn $ "***Welcome, "++name++"***"
             let msgLines = "CHAT:"++(show $ (hash roomName))++"\nCLIENT_NAME:"++name++"\nMESSAGE:"++name ++ " has joined this Room.\n"
             notifyRoom (hash roomName) $ Broadcast msgLines
             runClient server client >> endClient client
           _ -> output "Unrecognized command" >> readOp
           where
            notifyRoom roomRef msg = do
              roomsList <- atomically $ readTVar server
              let maybeRoom = M.lookup roomRef roomsList
              case maybeRoom of
               Nothing    -> putStrLn ("No room with reference " ++ (show roomRef)) >> return True
               Just aRoom -> sendRoomMessage msg aRoom >> return True
            endClient client = do
              return ()
       _ -> output "Unreconized command" >> readOp
       where
        output = hPutStrLn handle 
        getArgs n = replicateM n $ hGetLine handle
        echo s = do
                  putStrLn $ s ++ " being echoed"
                  output s
                  input <- hGetLine handle
                  echo input

runClient :: Server -> Client -> IO ()
runClient serv client@Client{..} = do
  race server receive
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
         mapM_ putStrLn cmdLineArgs
         send cmdLineArgs roomRef
       ["DISCONNECT:",ip]          -> do
         cmdLineArgs <- getArgs (disconnectArgs-1)
         putStrLn "disconnect command"
         send cmdLineArgs ip
       ["CHAT:",roomRef]           -> do
         cmdLineArgs <- getArgs (sendMsgArgs-1)
         send cmdLineArgs roomRef
       ["KILL_SERVICE"]            -> do
         send [killService] killService
       _                           -> throwError
       where
        send :: [String] -> String -> IO ()
        send args initialArg = atomically   $ sendMessage client $ Command (map words args) initialArg
        throwError           = atomically   $ sendMessage client $ Error "Error 1" "Unrecognised Command"
        getArgs n            = replicateM n $ hGetLine clientHandle
   server = join $ atomically $ do
     msg <- readTChan clientSendChan
     return $ do 
       continue <- handleMessage serv client msg
       when continue $ server

handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage server client@Client{..} message =
  case message of
    Notice    msg       -> output $ msg
    Response  msg       -> output $ msg
    Broadcast msg       -> output $ msg
    Error heading body  -> output $ "->" ++ heading ++ "<-\n" ++ body
    Command msg mainArg -> case msg of


      [["CLIENT_IP:",_],["PORT:",_],["CLIENT_NAME:",name]] -> do
        putStrLn ("joining joinRef = " ++ show (clientID + (hash mainArg)))
        let msgLines = "CHAT:"++(show $ (hash mainArg))++"\nCLIENT_NAME:"++clientName++"\nMESSAGE:"++clientName ++ " has joined this Room.\n"
        joinRoom client server mainArg >> notifyRoom (hash mainArg) (Broadcast msgLines)

      [["JOIN_ID:",id],["CLIENT_NAME:",name]] -> do
        putStrLn ("leave room joinref = " ++ id)
        leaver client server (read mainArg :: Int) (read id :: Int)
        return True

      [["PORT:",_],["CLIENT_NAME:",name]] -> removeClient server client >> return False

      [["JOIN_ID:",id],["CLIENT_NAME:",name],("MESSAGE:":msgToSend),[]] -> do
        notifyRoom (read mainArg :: Int) $ Broadcast ("CHAT: " ++ mainArg ++ "\nCLIENT_NAME: " ++ name ++ "\nMESSAGE: "++(unwords msgToSend)++"\n")

      [["KILL"]]                         -> do
        if mainArg == killService then return False
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
           Nothing    -> putStrLn ("No room with that reference " ++ (show roomRef)) >> return True
           Just aRoom -> sendRoomMessage msg aRoom >> return True
  where
   output s = do putStrLn (clientName ++ " receiving\\/\n" ++ s) >> hPutStrLn clientHandle s; return True

removeClient :: Server -> Client -> IO ()
removeClient serv cli@Client{..} = do
  rooms <- atomically $ readTVar serv
  let roomNames = Prelude.map (\room -> roomName room) (M.elems rooms)
  putStrLn $ show roomNames
  mapM_ (\room -> kickFrom room) roomNames
  where
   kickFrom room = do 
     leaveRoom cli serv (hash room) >> putStrLn (clientName ++ " removed from " ++ room)

main :: IO ()
main = withSocketsDo $ do 
 server <- newServer
 sock <- listenOn (PortNumber (fromIntegral port))
 printf "Listening on port %d\n" port
 forever $ do
   (handle, host, port) <- accept sock
   printf "Accepted connection from %s: %s\n" host (show port)
   forkFinally (handleClient handle server) (\_ -> hClose handle)

