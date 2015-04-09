-- Controllers are programs which register themselves with the gateway like
-- devices, receive push updates from the gateway, and send commands to other
-- devices (which must be specified by ID when the controller starts).

module Controllers(startController, Controller(..)) where

import           Control.Concurrent.Killable
import           Control.Concurrent.Lifted
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Control (liftBaseDiscard)
import           Data.List.Split
import           Safe                        (readMay)
import           System.IO
import           System.Timer.Updatable

import           Communication
import           Protocol
import           TimeProtocol

data Controller = Heater | Light | UserInterface | TestLogger

heatOnThreshold :: Int
heatOnThreshold  = 1

heatOffThreshold :: Int
heatOffThreshold = 2

tempCheckIntervalMicros :: Int
tempCheckIntervalMicros = 1000000 -- 1 second

lightDelayMicros :: Delay
lightDelayMicros = 5 * 60 * 1000000 -- 5 minutes

askForDeviceID :: String -> Bool -> Timed ID
askForDeviceID name silent = liftIO $ liftM ID $
  unless silent (putStr (name ++ " ID: ") >> hFlush stdout) >> readLn

startController :: Controller -> String -> String -> Bool -> Timed ()

--------------------------------------------------------------------------------
-- Task 1: Preventing Water Pipe Bursts
--
-- The heater controller checks periodically (every second) for temperature
-- changes from a temperature sensor, then changes the state of a smart outlet
-- if the temperature passes certain thresholds.

startController Heater host port silent =
  do (send, recv, myID) <- connectAndRegister Controller host port silent
     let println = liftIO . unless silent . putStrLn
     tempID   <- askForDeviceID "Temperature Sensor" silent
     outletID <- askForDeviceID "Smart Outlet" silent
     println "Running controller..."
     let handle :: MessageHandler Int
         handle lastCID (Response conv rsp)
           | conversationID conv == lastCID = case rsp of
               HasState (DegreesCelsius c) ->
                 do when (c <= heatOnThreshold) $ lift $
                      do conv' <- myID `to` outletID
                         sendReq conv' (ChangeState On) send
                         println "Heat on."
                    when (c >= heatOffThreshold) $ lift $
                      do conv' <- myID `to` outletID
                         sendReq conv' (ChangeState Off) send
                         println "Heat off."
                    liftIO $ threadDelay tempCheckIntervalMicros
                    conv' <- myID `to` tempID
                    sendReq conv' QueryState send
                    return (conversationID conv')
               _ -> fail $ "Unexpected " ++ show rsp
           | otherwise = return lastCID
         handle st r = lift (println (show r)) >> return st
     conv <- myID `to` tempID
     sendReq conv QueryState send
     why <- messageLoop recv handle (conversationID conv)
     println $ "Controller died: " ++ why

--------------------------------------------------------------------------------
-- Task 2: Preparing for Spring Break
--
-- The light controller detects motion and turns a smart light bulb on and off
-- in response. It also detects the `ChangeMode` broadcast message and adjusts
-- its behavior accordingly: if the user is away, it will not turn the light on,
-- but will send a `TextMessage` broadcast when motion is detected.

startController Light host port silent =
  do (send, recv, myID) <- connectAndRegister Controller host port silent
     let println = liftIO . unless silent . putStrLn
     motionID <- askForDeviceID "Motion Sensor" silent
     bulbID   <- askForDeviceID "Smart Light Bulb" silent
     let turnOff = do conv <- myID `to` bulbID
                      sendReq conv (ChangeState Off) send
                      println "Turning light off."
     timer <- liftBaseDiscard (`replacer` lightDelayMicros) turnOff
     let resetTimer = liftIO (renewIO timer lightDelayMicros)
     println "Running controller..."
     let awayMsg = TextMessage "Motion detected while user is away!"
         handle :: MessageHandler Mode
         handle _ (Broadcast _ (ChangeMode st')) =
           lift (println ("Set user mode to " ++ show st')) >> return st'
         handle Home (Broadcast i (ReportState (MotionDetected True))) =
           do when (i == motionID) $ lift $
                do conv <- myID `to` bulbID
                   sendReq conv (ChangeState On) send
                   println "Turning light on."
                   liftIO $ kill timer
              return Home
         handle Home (Broadcast i (ReportState (MotionDetected False))) =
           do when (i == motionID) $ lift $
                println "Setting light timer." >> resetTimer
              return Home
         handle Away (Broadcast i (ReportState (MotionDetected True))) =
           do when (i == motionID) $ lift $
                writeChanM send $ Broadcast myID awayMsg
              return Away
         handle st (Response _ rsp) = lift (println (show rsp)) >> return st
         handle st _ = return st
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
  do (send, recv, myID) <- connectAndRegister Controller host port silent
     let println = liftIO . unless silent . putStrLn
         console = do liftIO $ unless silent $ do putStr "> "
                                                  hFlush stdout
                      input <- liftIO getLine
                      if input == "exit"
                        then println "Goodbye!"
                        else do writeChanM recv $ UserInput input
                                unless silent $ liftIO $ threadDelay 100000
                                console
         handle :: MessageHandler ()
         handle _ (UserInput s) =
           lift $ case splitOn "->" s of
             [ids, reqs] ->
                maybe (println "Invalid input.")
                      (\(i, r) -> myID `to` i >>= \c -> sendReq c r send) $
                      do i <- readMay ids
                         r <- readMay reqs
                         return (ID i, r)
             [brcs] -> maybe (println "Invalid input.")
                             (\brc -> sendBrc myID brc send)
                             (readMay brcs)
             _ -> println "Invalid input: Only one -> is allowed."
         handle _ (Response _ rsp) =
           void $ liftIO $ do unless silent (putStrLn "")
                              putStr "RESPONSE: "
                              putStrLn $ if silent then show rsp
                                                   else showRsp rsp
                              unless silent (putStr "> ")
                              hFlush stdout
         handle _ msg = void $ liftIO $ do unless silent (putStrLn "")
                                           print msg
                                           unless silent (putStr "> ")
                                           hFlush stdout
     println $ "Connected to host " ++ host ++ ":" ++ port ++ "."
     println "\nEnter requests in the form 'ID -> Request'."
     println "\nMessages without an 'ID ->' prefix are broadcasts."
     println "Examples: \"1 -> QueryState\""
     println "          \"2 -> ChangeState On\""
     println "          \"ChangeMode Home\""
     println "          \"ChangeMode Away\""

     _ <- fork $ do why <- messageLoop recv handle ()
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
        showRsp (NotFound i) = "Error: No device with " ++ show i
        showRsp (NotSupported mt req) =
          "Error: " ++ show mt ++ " does not support request " ++ show req
        showRsp rsp = show rsp

startController TestLogger _ _ _ = undefined

