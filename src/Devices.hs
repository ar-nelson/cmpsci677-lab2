module Devices(startDevice) where

import           Control.Concurrent.Lifted
import           Control.Monad.Except
import           Network.Socket            hiding (Broadcast, recv, send)
import           Safe                      (readMay)

import           Communication
import           Protocol
import           TimeProtocol
import           TimeServer

--------------------------------------------------------------------------------
-- All devices present a console interface that allows a user to query and/or
-- update the device's state directly.

startDevice :: Device -> HostName -> String -> Bool -> Timed ()
startDevice dev host port silent =
  do (send, recv, i) <- connectWithTimeServer (Device dev) host port silent
     println (nameOf dev ++ " console interface")
     println (instructions dev)
     _ <- fork $ do why <- bg dev send recv i (lift . println)
                    println ("Background thread died: " ++ why)
     console send recv silent
     killChanM send "Console interface closed."
  where println = liftIO . unless silent . putStrLn

nameOf :: Device -> String
nameOf Temp   = "Temperature Sensor"
nameOf Motion = "Motion Sensor"
nameOf Door   = "Door Sensor"
nameOf dev    = "Smart " ++ show dev

instructions :: Device -> String
instructions Temp = "Enter an integer, 'state', or 'exit'."
instructions _    = "Enter 'on', 'off', 'state', or 'exit'."

--------------------------------------------------------------------------------
-- A background thread runs concurrently with the CLI, handling both network and
-- user input. In the event of an error, this thread will stop and an error
-- message will display, but the CLI will not immediately close.

bg :: Device                      -- Device type
   -> MessageChan                 -- send channel
   -> MessageChan                 -- recv channel
   -> ID                          -- Device ID
   -> (String -> HandlerState ()) -- println function
   -> Timed String                -- Return value: Error message

--------------------------------------------------------------------------------
-- The temperature sensor has a local state that can be queried remotely, but
-- can only be set locally via the console interface.
--
-- Entering an integer at the console interface will set the current
-- temperature, and entering `state` will print the current temperature.

bg Temp send recv _ println = messageLoop recv handle (DegreesCelsius 0)

  where handle :: MessageHandler State

        handle (DegreesCelsius c) (UserInput "state") =
          println (show c ++ "\0176C") >> return (DegreesCelsius c)

        handle st (UserInput s) =
          case readMay s :: Maybe Int of
            Just c  -> println ("Set temp to " ++ show c ++ "\0176C.")
                       >> return (DegreesCelsius c)
            Nothing -> println "Invalid input." >> return st

        handle st (Request conv QueryState) =
          sendRsp conv (HasState st) send >> return st

        handle st (Request conv req) =
          sendRsp conv (NotSupported (Device Temp) req) send >> return st

        handle st _ = return st

--------------------------------------------------------------------------------
-- The motion sensor pushes `ReportState` broadcast messages when its state is
-- changed, and can also be queried for its state.
--
-- The console inputs `on` and `off` simulate the detector seeing motion/no
-- motion, respectively.

bg Motion send recv myID println = messageLoop recv handle(MotionDetected False)

  where handle :: MessageHandler State

        handle _  (UserInput "on")    = setState True
        handle _  (UserInput "off")   = setState False
        handle st (UserInput "state") = println (show st) >> return st
        handle st (UserInput _)       = println "Invalid input." >> return st

        handle st (Request conv QueryState) =
          sendRsp conv (HasState st) send >> return st

        handle st (Request conv req) =
          sendRsp conv (NotSupported (Device Motion) req) send >> return st

        handle st _ = return st

        setState v = do println ("State changed to " ++ show v ++ ".")
                        writeChanM send (Broadcast myID (ReportState st))
                        return st
                     where st = MotionDetected v
--------------------------------------------------------------------------------
-- The door sensor pushes `ReportState` broadcast messages when its state is
-- changed, and can also be queried for its state.
--
-- The console inputs `on` and `off` open and close the door, respectively.

bg Door send recv myID println = messageLoop recv handle (DoorOpen False)

  where handle :: MessageHandler State

        handle _  (UserInput "on")    = setState True
        handle _  (UserInput "off")   = setState False
        handle st (UserInput "state") = println (show st) >> return st
        handle st (UserInput _)       = println "Invalid input." >> return st

        handle st (Request conv QueryState) =
          sendRsp conv (HasState st) send >> return st

        handle st (Request conv req) =
          sendRsp conv (NotSupported (Device Door) req) send >> return st

        handle st _ = return st

        setState v = do println ("State changed to " ++ show v ++ ".")
                        writeChanM send (Broadcast myID (ReportState st))
                        return st
                     where st = DoorOpen v

--------------------------------------------------------------------------------
-- Both the Smart Light Bulb and Smart Outlet behave identically, so they use
-- the same code. The only functionality they provide is an on/off state.
--
-- The console interface allows a user or script to set the state with `on` or
-- `off`, and query the state with `state`.

bg dev send recv _ println = messageLoop recv handle (Power Off)

  where handle :: MessageHandler State

        handle _  (UserInput "on")    = setState On
        handle _  (UserInput "off")   = setState Off
        handle st (UserInput "state") = println (show st) >> return st
        handle st (UserInput _)       = println "Invalid input." >> return st

        handle st (Request conv QueryState) =
          sendRsp conv (HasState st) send >> return st

        handle _ (Request conv (ChangeState o)) =
          sendRsp conv Success send >> setState o

        handle st (Request conv req) =
          sendRsp conv (NotSupported (Device dev) req) send >> return st

        handle st _ = return st

        setState o = do println ("State changed to " ++ show o ++ ".")
                        return (Power o)
