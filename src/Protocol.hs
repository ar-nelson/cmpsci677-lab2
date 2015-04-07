{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Protocol( Device(..)
               , Mode(..)
               , OnOff(..)
               , State(..)
               , ID(..)
               , MsgID(..)
               , Request(..)
               , Response(..)
               , Broadcast(..)
               , Message(..)
) where

import           System.Random

data Device = Temp
            | Motion
            | Door
            | Bulb
            | Outlet
            deriving (Eq, Show, Read)

data Mode   = Home
            | Away
            deriving (Eq, Show, Read)

data OnOff  = On
            | Off
            deriving (Eq, Show, Read)

data State  = DegreesCelsius Int
            | MotionDetected Bool
            | DoorOpen Bool
            | Power OnOff
            deriving (Eq, Show, Read)

newtype ID = ID Int deriving (Eq, Ord, Show, Read)

-- A `Request` is an RPC, which the gateway will either forward to a device (if
-- it contains an `ID`) or handle directly (if it does not contain an `ID`).

data Request = Register Device
             | Subscribe
             | QueryState ID
             | ChangeState ID OnOff
             deriving (Show, Read)

-- A `Response` is the return value of an RPC. Some `Response`s are valid return
-- values, while others (the ones starting with `No` or `Not`) are error
-- responses.

data Response = Success
              | RegisteredAs ID
              | HasState State
              | NoDevice ID
              | NotSupported Device Request
              deriving (Show, Read)

-- A `Broadcast` is a message sent to all currently connected controllers.
-- Broadcasts are used for push-style updates.

data Broadcast = ReportState ID State
               | ChangeMode Mode
               | TextMessage String
               deriving (Show, Read)

-- A `Message` is anything that can be sent over a network, plus a few special
-- cases. This includes `Request`s, `Response`s, and `Broadcast`s, but it also
-- includes command-line user input (to make CLI event loops simpler) and
-- a special case for unparseable input.

data Message = Req MsgID Request
             | Rsp MsgID Response
             | Brc Broadcast
             | UserInput String
             | Unknown String
             deriving (Show, Read)

-- `Request`s and `Response`s have an attached message ID, which is
-- a randomly-assigned integer used by the gateway to match responses to
-- requests.

newtype MsgID = MsgID Int deriving (Eq, Ord, Show, Read, Random)

