module Types where

type ClientName = String
type RoomName = String


type ErrorHeading = String
type ErrorBody = String
type CmdArgs = [[String]]

data Message = Notice String
             | Tell ClientName String
             | Broadcast ClientName String
             | Command CmdArgs String
             | Error ErrorHeading ErrorBody