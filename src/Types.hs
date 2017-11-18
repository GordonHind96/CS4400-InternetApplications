module Types where


type ClientName = String
type RoomName = String
type RoomRef = Int

type ErrorHeading = String
type ErrorBody = String
type CmdArgs = [[String]]

data Message = Notice String
             | Response String
             | Broadcast String
             | Command [[String]] String
             | Error String String
             deriving Show
