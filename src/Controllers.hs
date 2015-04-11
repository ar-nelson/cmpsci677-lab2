-- Controllers are programs which register themselves with the gateway like
-- devices, receive push updates from the gateway, and send commands to other
-- devices (which must be specified by ID when the controller starts).

module Controllers(startController, Controller(..)) where

import           Control.Concurrent.Lifted
import           Control.Monad
import           Control.Monad.Trans
import           Data.List.Split
import           Data.Time
import           Data.Word
import           Network.Socket            (HostName)
import           Prelude                   hiding (log)
import           Safe                      (readMay)
import           System.IO

import           Communication
import           Protocol
import           TimeProtocol
import           TimeServer

data Controller = Heater | Light | Security | UserInterface | TestLogger

askForDeviceID :: String -> Bool -> Timed ID
askForDeviceID name silent = liftIO $ liftM ID $
  unless silent (putStr (name ++ " ID: ") >> hFlush stdout) >> readLn

startController :: Controller -> HostName -> String -> Bool -> Timed ()

--------------------------------------------------------------------------------
-- Task 1: Preventing Water Pipe Bursts
--
-- The heater controller checks periodically (every second) for temperature
-- changes from a temperature sensor, then changes the state of a smart outlet
-- if the temperature passes certain thresholds.

startController Heater host port silent =
  do (send, recv, myID) <- connectWithTimeServer Controller host port silent

     tempID   <- askForDeviceID "Temperature Sensor" silent
     outletID <- askForDeviceID "Smart Outlet" silent
     println "Running controller..."

     let handle :: MessageHandler Word32
         handle lastCID (Response conv rsp)
           | conversationID conv == lastCID =
             case rsp of
               HasState (DegreesCelsius c) ->
                 do when (c <= heatOnThreshold)  (setState On)
                    when (c >= heatOffThreshold) (setState Off)
                    threadDelay tempCheckIntervalMicros
                    conv' <- myID `to` tempID
                    sendReq conv' QueryState send
                    return (conversationID conv')
               _ -> fail ("Unexpected " ++ show rsp)
           | otherwise = return lastCID
           where setState o = lift $ do conv' <- myID `to` outletID
                                        sendReq conv' (ChangeState o) send
                                        println ("Heat " ++ show o ++ ".")
         handle st r = lift (println (show r)) >> return st

     conv <- myID `to` tempID
     sendReq conv QueryState send
     why <- messageLoop recv handle (conversationID conv)
     println ("Controller died: " ++ why)

   where println = liftIO . unless silent . putStrLn
         heatOnThreshold  = 1
         heatOffThreshold = 2
         tempCheckIntervalMicros = 1000000 -- 1 second

--------------------------------------------------------------------------------
-- Task 2: Preparing for Spring Break
--
-- The light controller detects motion and turns a smart light bulb on and off
-- in response. It also detects the `ChangeMode` broadcast message and adjusts
-- its behavior accordingly: if the user is away, it will not turn the light on,
-- but will send a `TextMessage` broadcast when motion is detected.

startController Light host port silent =
  do (send, recv, myID) <- connectWithTimeServer Controller host port silent

     motionID <- askForDeviceID "Motion Sensor" silent
     bulbID   <- askForDeviceID "Smart Light Bulb" silent
     println "Running controller..."

     timer <- newEmptyMVar :: Timed (MVar ThreadId)
     let timerAction = do threadDelay lightDelayMicros
                          println "Turning light off."
                          conv <- myID `to` bulbID
                          sendReq conv (ChangeState Off) send
         resetTimer = fork timerAction >>= swapMVar timer >>= killThread
     putMVar timer =<< fork timerAction

     let handle :: MessageHandler Mode

         handle _ (Broadcast _ (ChangeMode st')) =
           lift (println ("Set user mode to " ++ show st')) >> return st'

         handle Home (Broadcast i (ReportState (MotionDetected True))) =
           do when (i == motionID) $ lift $
                do conv <- myID `to` bulbID
                   sendReq conv (ChangeState On) send
                   println "Turning light on."
                   readMVar timer >>= killThread
              return Home

         handle Home (Broadcast i (ReportState (MotionDetected False))) =
           do when (i == motionID) $ lift $
                println "Setting light timer." >> resetTimer
              return Home

         handle Away (Broadcast i (ReportState (MotionDetected True))) =
           do when (i == motionID) $ lift $
                writeChanM send (Broadcast myID awayMsg)
              return Away

         handle st (Response _ rsp) = lift (println (show rsp)) >> return st
         handle st _ = return st

     why <- messageLoop recv handle Home
     println ("Controller died: " ++ why)

   where println = liftIO . unless silent . putStrLn
         awayMsg = TextMessage "Motion detected while user is away!"
         lightDelayMicros = 5 * 60 * 1000000 -- 5 minutes

--------------------------------------------------------------------------------
-- Task 3: Event Ordering
--
-- The security controller changes the user's status between `Home` and `Away`
-- based on input from a door sensor, a motion sensor, and a presence sensor.
-- It confirms the order of these events using a database. If a person enters
-- the home (door open followed by motion detected), then the controller will
-- send a text message broadcast.

startController Security host port silent =
  do (send, recv, myID) <- connectWithTimeServer Controller host port silent

     dbID       <- askForDeviceID "Database"        silent
     motionID   <- askForDeviceID "Motion Sensor"   silent
     doorID     <- askForDeviceID "Door Sensor"     silent
     presenceID <- askForDeviceID "Presence Sensor" silent
     println "Running controller..."

     let handle :: MessageHandler (Maybe Word32)

         handle st (Broadcast bid (ReportState (MotionDetected b)))
           | bid /= motionID = return st
           | otherwise = do log $ if b then "Motion detected."
                                       else "No motion detected."
                            sendQuery

         handle st (Broadcast bid (ReportState (DoorOpen b)))
           | bid /= doorID = return st
           | otherwise = do log $ if b then "Door open."
                                       else "Door closed."
                            sendQuery

         handle st (Broadcast bid Present) =
           when (bid == presenceID) (log "Push event from presence sensor.")
           >> return st

         handle (Just cid) (Response Conversation { conversationID = cid' }
                                     (DBResultSet [ Just doorOpenTime
                                                  , Just doorCloseTime
                                                  , Just motionTrueTime
                                                  , maybeMotionFalseTime
                                                  , maybePresenceTime
                                                  ])) =
           do when (cid == cid') $ lift $
                if incomingOrder
                   then if presenceDetected
                             then do log "Owner came home."
                                     sendBrc myID (ChangeMode Home) send
                             else do log "Intruder detected!"
                                     sendBrc myID intruderMsg send
                   else when outgoingOrder $
                          do log "Owner left home."
                             sendBrc myID (ChangeMode Away) send
              return Nothing

           where outgoingOrder = case maybeMotionFalseTime of
                   Just motionFalseTime ->
                     within motionTimeout doorOpenTime motionFalseTime
                     && doorOpenTime   `before` doorCloseTime
                     && motionTrueTime `before` doorOpenTime
                     && motionTrueTime `before` motionFalseTime
                   Nothing -> False

                 incomingOrder =
                   within motionTimeout doorOpenTime motionTrueTime
                   && doorOpenTime `before` motionTrueTime
                   && doorCloseTime `before` doorOpenTime -- Prevents duplicates
                   && maybe True (`before` motionTrueTime) maybeMotionFalseTime

                 presenceDetected = case maybePresenceTime of
                   Just presenceTime ->
                     within presenceTimeout motionTrueTime presenceTime
                   Nothing -> False

         handle (Just cid) (Response Conversation { conversationID = cid' }
                                     DBResultSet {})
           | cid /= cid' = return (Just cid)
           | otherwise   = return Nothing

         handle (Just cid) (Response Conversation { conversationID = cid' } rsp)
           | cid /= cid' = return (Just cid)
           | otherwise   = do lift . println $
                                "Invalid database response: " ++ show rsp
                              return Nothing

         handle st _ = return st

         sendQuery =
           do conv <- myID `to` dbID
              sendReq conv (DBQuery query) send
              return (Just (conversationID conv))
           where { query =
             [ (doorID,     BroadcastEvent (ReportState (DoorOpen True)))
             , (doorID,     BroadcastEvent (ReportState (DoorOpen False)))
             , (motionID,   BroadcastEvent (ReportState (MotionDetected True)))
             , (motionID,   BroadcastEvent (ReportState (MotionDetected False)))
             , (presenceID, BroadcastEvent Present)
             ] }

     why <- messageLoop recv handle Nothing
     println ("Controller died: " ++ why)

  where println         = liftIO . unless silent . putStrLn
        log message     = liftIO $
          do time <- getCurrentTime
             putStrLn (formatTime defaultTimeLocale "%T > " time ++ message)
        intruderMsg     = TextMessage "Intruder detected!"
        a `before` b    = lamportTime a < lamportTime b
        within time a b = abs (clockTime a `timeDiff` clockTime b) <= time
        motionTimeout   = 60 -- 1 minute
        presenceTimeout = 60 -- 1 minute

--------------------------------------------------------------------------------
-- The user interface is a special kind of controller: it takes console input in
-- the form of Haskell commands, and sends it directly to the gateway. It also
-- reports responses and broadcast messages to the console.
--
-- A background thread that handles both user and network input. All
-- network input is printed to the screen, even in silent mode (although silent
-- mode uses less pretty printing).

startController UserInterface host port silent =
  do (send, recv, myID) <- connectWithTimeServer Controller host port silent

     println $ unlines
       [ "Connected to host " ++ host ++ ":" ++ port ++ "."
       , ""
       , "Enter requests in the form 'ID -> Request'."
       , "Messages without an 'ID ->' prefix are broadcasts."
       , ""
       , "Examples: \"1 -> QueryState\""
       , "          \"2 -> ChangeState On\""
       , "          \"ChangeMode Home\""
       , "          \"ChangeMode Away\""
       ]

     let handle :: MessageHandler ()
         handle _ (UserInput s) =
           lift $ case splitOn "->" s of
             [ids, reqs] ->
                maybe (println "Invalid input.")
                      (\(i, r) -> myID `to` i >>= \c -> sendReq c r send)
                      $ do i <- readMay ids
                           r <- readMay reqs
                           return (ID i, r)
             [brcs] -> maybe (println "Invalid input.")
                             (\brc -> sendBrc myID brc send)
                             (readMay brcs)
             _ -> println "Invalid input: Only one -> is allowed."
         handle _ (Response _ rsp) =
           void $ liftIO $ do putStr "RESPONSE: "
                              putStrLn $ if silent then show rsp
                                                   else showRsp rsp
                              hFlush stdout
         handle _ msg = void $ liftIO (print msg)

     _ <- fork $ do why <- messageLoop recv handle ()
                    println $ "Background thread died: " ++ why
     console send recv silent
     killChanM send "Console interface closed."

  where println = liftIO . unless silent . putStrLn
        showRsp rsp = case rsp of
          Success                         -> "Success!"
          RegisteredAs i                  -> "Registered with " ++ show i
          HasState (DegreesCelsius c)     -> show c ++ "\0176C"
          HasState (MotionDetected True)  -> "Motion detected!"
          HasState (MotionDetected False) -> "No motion detected."
          HasState (DoorOpen True)        -> "Door is open."
          HasState (DoorOpen False)       -> "Door is closed."
          HasState (Power p)              -> show p
          NotFound i          -> "Error: No device with " ++ show i
          NotSupported mt req -> "Error: " ++ show mt
            ++ " does not support request " ++ show req
          _ -> show rsp

startController TestLogger _ _ _ = undefined

