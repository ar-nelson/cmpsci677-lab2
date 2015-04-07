module Devices(startDevice) where

import           Control.Concurrent
import           Control.Monad      (unless)
import           Network.Socket     hiding (recv, send)
import           Safe               (readMay)
import           System.IO

import           Communication
import           Protocol

--------------------------------------------------------------------------------
-- All devices present a console interface that allows a user to query and/or
-- update the device's state directly.

startDevice :: Device -> HostName -> String -> Bool -> IO ()
startDevice dev host port silent =
  do (send, recv, i) <- connectAndRegister dev host port silent
     let -- If the `silent` command-line argument was provided, this interface
         -- will not output anything except the device's ID.
         println = unless silent . putStrLn
         -- Every line from standard input is a command, sent as a `UserInput`
         -- message as though it were received over a socket. The command `exit`
         -- bypasses this and shuts down the application.
         console = do unless silent $ do putStr "> "
                                         hFlush stdout
                      input <- getLine
                      if input == "exit"
                        then println "Goodbye!"
                        else do writeChan recv $ Right (UserInput input)
                                unless silent $ threadDelay 1000
                                console
     println $ nameOf dev ++ " console interface"
     println $ instructions dev
     _ <- forkIO $ do why <- bg dev send recv i println
                      println $ "Background thread died: " ++ why
     console
     writeChan send $ Left "Console interface closed."

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

bg :: Device            -- Device type
   -> MessageChan       -- send channel
   -> MessageChan       -- recv channel
   -> ID                -- Device ID
   -> (String -> IO ()) -- println function
   -> IO String         -- Return value: Error message

recur :: a -> IO (Either String a)
recur = return . Right

--------------------------------------------------------------------------------
-- The temperature sensor has a local state that can be queried remotely, but
-- can only be set locally via the console interface.
--
-- Entering an integer at the console interface will set the current
-- temperature, and entering `state` will print the current temperature.

bg Temp send recv _ println = messageLoop recv handle (DegreesCelsius 0)
  where handle :: MessageHandler State
        handle st (UserInput "state") =
          do case st of DegreesCelsius c -> println $ show c ++ "\0176C"
             recur st
        handle st (UserInput s) =
          case readMay s :: Maybe Int of
            Just c -> do println $ "Set temp to " ++ show c ++ "\0176C."
                         recur $ DegreesCelsius c
            Nothing -> println "Invalid input." >> recur st
        handle st (Req mid (QueryState _)) =
          do sendRsp mid (HasState st) send
             recur st
        handle st (Req mid req) =
          do sendRsp mid (NotSupported Temp req) send
             recur st
        handle st (Unknown s) =
          do println $ "Unparseable message: '" ++ s ++ "'"
             recur st
        handle st _ = recur st

--------------------------------------------------------------------------------
-- The motion sensor pushes `ReportState` broadcast messages when its state is
-- changed, and can also be queried for its state.
--
-- The console inputs `on` and `off` simulate the detector seeing motion/no
-- motion, respectively.

bg Motion send recv i println =
  messageLoop recv handle (MotionDetected False)
  where handle :: MessageHandler State
        setState v = do println $ "State changed to " ++ show v ++ "."
                        let st = MotionDetected v
                        writeChan send $ Right (Brc (ReportState i st))
                        recur st
        handle _  (UserInput "on")  = setState True
        handle _  (UserInput "off") = setState False
        handle st (UserInput "state") = println (show st) >> recur st
        handle st (UserInput _) = println "Invalid input." >> recur st
        handle st (Req mid (QueryState _)) =
          sendRsp mid (HasState st) send >> recur st
        handle st (Req mid req) =
          sendRsp mid (NotSupported Motion req) send >> recur st
        handle st (Unknown s) =
          println ("Unparseable message: '" ++ s ++ "'") >> recur st
        handle st _ = recur st

--------------------------------------------------------------------------------
-- The door sensor pushes `ReportState` broadcast messages when its state is
-- changed, and can also be queried for its state.
--
-- The console inputs `on` and `off` open and close the door, respectively.

bg Door send recv i println = messageLoop recv handle (DoorOpen False)
  where handle :: MessageHandler State
        setState v = do println $ "State changed to " ++ show v ++ "."
                        let st = DoorOpen v
                        writeChan send $ Right (Brc (ReportState i st))
                        recur st
        handle _  (UserInput "on")  = setState True
        handle _  (UserInput "off") = setState False
        handle st (UserInput "state") = println (show st) >> recur st
        handle st (UserInput _) = println "Invalid input." >> recur st
        handle st (Req mid (QueryState _)) =
          sendRsp mid (HasState st) send >> recur st
        handle st (Req mid req) =
          sendRsp mid (NotSupported Door req) send >> recur st
        handle st (Unknown s) =
          println ("Unparseable message: '" ++ s ++ "'") >> recur st
        handle st _ = recur st

--------------------------------------------------------------------------------
-- Both the Smart Light Bulb and Smart Outlet behave identically, so they use
-- the same code. The only functionality they provide is an on/off state.
--
-- The console interface allows a user or script to set the state with `on` or
-- `off`, and query the state with `state`.

bg dev send recv _ println = messageLoop recv handle (Power Off)
  where handle :: MessageHandler State
        setState o = do println $ "State changed to " ++ show o ++ "."
                        recur (Power o)
        handle _  (UserInput "on")  = setState On
        handle _  (UserInput "off") = setState Off
        handle st (UserInput "state") = println (show st) >> recur st
        handle st (UserInput _) = println "Invalid input." >> recur st
        handle st (Req mid (QueryState _)) =
          sendRsp mid (HasState st) send >> recur st
        handle _ (Req mid (ChangeState _ o)) =
          sendRsp mid Success send >> setState o
        handle st (Req mid req) =
          sendRsp mid (NotSupported dev req) send >> recur st
        handle st (Unknown s) =
          println ("Unparseable message: '" ++ s ++ "'") >> recur st
        handle st _ = recur st

--------------------------------------------------------------------------------
-- Each device, when it starts, contacts the gateway and attempts to
--
-- * establish a TCP connection, and
-- * send a `Register` command and acquire a device ID.
--
-- If either step fails, an exception will be thrown and the application will
-- close.

connectAndRegister :: Device
                   -> HostName
                   -> String
                   -> Bool
                   -> IO (MessageChan, MessageChan, ID)

connectAndRegister dev host port silent =
  do send <- newChan :: IO MessageChan
     recv <- newChan :: IO MessageChan
     connectToGateway host port send recv silent
     mid <- sendReq (Register dev) send
     rspMsg <- readChan recv
     case rspMsg of Right (Rsp mid' rsp) ->
                      if mid == mid' then handleRsp rsp send recv
                                     else wrongRsp
                    Right _ -> wrongRsp
                    Left s -> error s
  where handleRsp (RegisteredAs (ID i)) send recv =
          do putStrLn $ if silent then show i
                                  else "Connected with ID " ++ show i
             hFlush stdout
             return (send, recv, ID i)
        handleRsp rsp _ _ = error $
          "Invalid response to Register: " ++ show rsp
        wrongRsp = error "Got something other than response to Register."

