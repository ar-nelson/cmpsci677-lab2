-- Controllers are programs which register themselves with the gateway like
-- devices, receive push updates from the gateway, and send commands to other
-- devices (which must be specified by ID when the controller starts).

module Controllers(startController, Controller(..)) where

import           Control.Concurrent
import           Control.Concurrent.Suspend.Lifted (mDelay)
import           Control.Concurrent.Timer
import           Control.Monad
import           Safe                              (readMay)
import           System.IO

import           Communication
import           Protocol

data Controller = Heater | Light | UserInterface | TestLogger

askForDeviceID :: String -> Bool -> IO ID
askForDeviceID name silent = do unless silent $ do putStr (name ++ " ID: ")
                                                   hFlush stdout
                                n <- readLn :: IO Int
                                return (ID n)

connectAndSubscribe :: String -> String -> Bool -> IO (MessageChan, MessageChan)
connectAndSubscribe host port silent =
  do send <- newChan :: IO MessageChan
     recv <- newChan :: IO MessageChan
     connectToGateway host port send recv silent
     mid <- sendReq Subscribe send
     rspMsg <- readChan recv
     case rspMsg of Right (Rsp mid' rsp) ->
                      if mid == mid' then handleRsp rsp send recv
                                     else wrongRsp
                    Right _ -> wrongRsp
                    Left s -> error s
  where handleRsp Success send recv =
          do unless silent $
               putStrLn "Subscribed to broadcasts from gateway."
             return (send, recv)
        handleRsp rsp _ _ = error $
          "Invalid response to Subscribe: " ++ show rsp
        wrongRsp = error "Got something other than response to Subscribe."

recur = return . Right
die   = return . Left

startController :: Controller -> String -> String -> Bool -> IO ()

--------------------------------------------------------------------------------
-- Task 1: Preventing Water Pipe Bursts
--
-- The heater controller checks periodically (every second) for temperature
-- changes from a temperature sensor, then changes the state of a smart outlet
-- if the temperature passes certain thresholds.

heatOnThreshold  = 1
heatOffThreshold = 2
tempCheckIntervalMicros = 1000000

startController Heater host port silent =
  do (send, recv) <- connectAndSubscribe host port silent
     let println = unless silent . putStrLn
     tempID   <- askForDeviceID "Temperature Sensor" silent
     outletID <- askForDeviceID "Smart Outlet" silent
     println "Running controller..."
     let handle :: MessageHandler MsgID
         handle mid' (Rsp mid rsp)
           | mid == mid' =
               case rsp of HasState (DegreesCelsius c) ->
                             do when (c <= heatOnThreshold) $
                                  do sendReq (ChangeState outletID On) send
                                     println "Heat on."
                                when (c >= heatOffThreshold) $
                                  do sendReq (ChangeState outletID Off) send
                                     println "Heat off."
                                threadDelay tempCheckIntervalMicros
                                mid'' <- sendReq (QueryState tempID) send
                                recur mid''
                           _ -> die $ "Unexpected " ++ show rsp
           | otherwise = recur mid'
         handle st r = println (show r) >> recur st
     mid <- sendReq (QueryState tempID) send
     why <- messageLoop recv handle mid
     println $ "Controller died: " ++ why

--------------------------------------------------------------------------------
-- Task 2: Preparing for Spring Break
--
-- The light controller detects motion and turns a smart light bulb on and off
-- in response. It also detects the `ChangeMode` broadcast message and adjusts
-- its behavior accordingly: if the user is away, it will not turn the light on,
-- but will send a `TextMessage` broadcast when motion is detected.

startController Light host port silent =
  do (send, recv) <- connectAndSubscribe host port silent
     let println = unless silent . putStrLn
     motionID <- askForDeviceID "Motion Sensor" silent
     bulbID   <- askForDeviceID "Smart Light Bulb" silent
     let turnOff = do sendReq (ChangeState bulbID Off) send
                      println "Turning light off."
         delay   = mDelay 5 -- 5 minutes
     timer <- oneShotTimer turnOff delay
     let resetTimer = do itWorked <- oneShotStart timer turnOff delay
                         unless itWorked resetTimer
     println "Running controller..."
     let awayMsg = TextMessage "Motion detected while user is away!"
         handle :: MessageHandler Mode
         handle _ (Brc (ChangeMode st')) =
           do println $ "Set user mode to " ++ show st'
              recur st'
         handle Home (Brc (ReportState i (MotionDetected True))) =
           do when (i == motionID) $ do sendReq (ChangeState bulbID On) send
                                        println "Turning light on."
                                        stopTimer timer
              recur Home
         handle Home (Brc (ReportState i (MotionDetected False))) =
           do when (i == motionID) $ do println "Setting light timer."
                                        resetTimer
              recur Home
         handle Away (Brc (ReportState i (MotionDetected True))) =
           do when (i == motionID) $ writeChan send $ Right (Brc awayMsg)
              recur Away
         handle st (Rsp _ rsp) = println (show rsp) >> recur st
         handle st _ = recur st
     why <- messageLoop recv handle Home
     println $ "Controller died: " ++ why

--------------------------------------------------------------------------------
-- The user interface is a special kind of controller: it takes console input in
-- the form of Haskell commands, and sends it directly to the gateway. It also
-- reports responses and broadcast messages to the console.
--
-- A background thread that handles both user and network input. All
-- network input is printed to the screen, even in silent mode (although silent
-- mode uses less pretty printing).

startController UserInterface host port silent =
  do (send, recv) <- connectAndSubscribe host port silent
     let println = unless silent . putStrLn
         console = do unless silent $ do putStr "> "
                                         hFlush stdout
                      input <- getLine
                      if input == "exit"
                        then println "Goodbye!"
                        else do writeChan recv $ Right (UserInput input)
                                unless silent $ threadDelay 100000
                                console
         handle :: MessageHandler ()
         handle _ (Req _ req) = do putStrLn $ "REQUEST: " ++ show req
                                   recur ()
         handle _ (Rsp _ rsp) =
           do putStrLn $ "RESPONSE: " ++ if silent then show rsp
                                                   else showRsp rsp
              recur ()
         handle _ (Brc brc)   = do putStrLn $ "BROADCAST: " ++ show brc
                                   recur ()
         handle _ (Unknown s) = do putStrLn $ "UNPARSEABLE: " ++ s
                                   recur ()
         handle _ (UserInput s) =
           do case readMay s :: Maybe Request of
                Just req -> void (sendReq req send)
                Nothing -> case readMay s :: Maybe Broadcast of
                             Just brc -> writeChan send $ Right (Brc brc)
                             Nothing -> println "Invalid input."
              recur ()
     println $ "Connected to host " ++ host ++ ":" ++ port ++ "."
     println "\nEnter commands in Haskell expression form."
     println "Examples: \"QueryState (ID 1)\""
     println "          \"ChangeState (ID 2) On\""
     println "          \"ChangeMode Home\""
     println "          \"ChangeMode Away\""

     bgThread <- forkIO $ do why <- messageLoop recv handle ()
                             println $ "Background thread died: " ++ why
     console
     writeChan send $ Left "Console interface closed."
  where showRsp :: Response -> String
        showRsp Success = "Success!"
        showRsp (RegisteredAs i) = "Registered with " ++ show i
        showRsp (HasState (DegreesCelsius c)) = show c ++ "\0176C"
        showRsp (HasState (MotionDetected True)) = "Motion detected!"
        showRsp (HasState (MotionDetected False)) = "No motion detected."
        showRsp (HasState (DoorOpen True)) = "Door is open."
        showRsp (HasState (DoorOpen False)) = "Door is closed."
        showRsp (HasState (Power p)) = show p
        showRsp (NoDevice id) = "Error: No device with " ++ show id
        showRsp (NotSupported dev req) =
          "Error: " ++ show dev ++ " does not support request " ++ show req

startController TestLogger _ _ _ = undefined

