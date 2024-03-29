{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Protocol(module Protocol) where

import           Data.Word
import           TimeProtocol

data MemberType = Gateway
                | Database
                | Controller
                | Device Device
                deriving (Eq, Ord, Show, Read)

data Device = Temp
            | Motion
            | Door
            | Presence
            | Bulb
            | Outlet
            deriving (Eq, Ord, Show, Read)

data Mode   = Home
            | Away
            deriving (Eq, Ord, Show, Read)

data OnOff  = On
            | Off
            deriving (Eq, Ord, Show, Read)

data State  = DegreesCelsius Int
            | MotionDetected Bool
            | DoorOpen Bool
            | Power OnOff
            deriving (Eq, Ord, Show, Read)

newtype ID = ID Int deriving (Eq, Ord, Show, Read, Num)

invalidID :: ID
invalidID = -1

gatewayID :: ID
gatewayID = 0

--------------------------------------------------------------------------------
-- Database datatypes.

data DBEntry = DBEntry ID Timestamp DBEvent deriving (Show, Read)

data DBEvent = RegisterEvent MemberType
             | LeaveEvent
             | BroadcastEvent Broadcast
             deriving (Eq, Ord, Show, Read)

-- A `Request` is an RPC, which the gateway will either forward to a device (if
-- it contains an `ID`) or handle directly (if it does not contain an `ID`).

data Request = Register MemberType
             | QueryState
             | ChangeState OnOff
             | ReportTime ClockTime
             | LeaderOK
             | DBInsert DBEntry
             | DBQuery [(ID, DBEvent)]
             deriving (Show, Read)

-- A `Response` is the return value of an RPC. Some `Response`s are valid return
-- values, while others (the ones starting with `No` or `Not`) are error
-- responses.

data Response = Success
              | RegisteredAs ID
              | HasState State
              | AdjustTime ClockOffset
              | DBResultSet [Maybe Timestamp]
              | NotFound ID
              | NotSupported MemberType Request
              deriving (Show, Read)

-- A `Broadcast` is a message sent to all currently connected controllers.
-- Broadcasts are used for push-style updates.

data Broadcast = ReportState State
               | ChangeMode Mode
               | TextMessage String
               | Present
               | QueryTime
               | Election
               | IWon
               deriving (Eq, Ord, Show, Read)

-- A `Message` is anything that can be sent over a network, plus a few special
-- cases. This includes `Request`s, `Response`s, and `Broadcast`s, but it also
-- includes command-line user input (to make CLI event loops simpler) and
-- a special case for unparseable input.

data Message = Request Conversation Request
             | Response Conversation Response
             | Broadcast ID Broadcast
             | UserInput String
             | Unknown String
             deriving (Show, Read)

data Conversation = Conversation { requester      :: ID
                                 , responder      :: ID
                                 , conversationID :: Word32
                                 } deriving (Show, Read)

