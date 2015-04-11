module TimeServer(connectWithTimeServer, runTimeServer) where

import           Control.Concurrent.Lifted
import           Control.Concurrent.STM
import           Control.Exception.Lifted  hiding (handle)
import           Control.Monad
import           Control.Monad.Except
import           Data.Foldable             (for_, traverse_)
import           Data.Map                  (Map, (!))
import qualified Data.Map                  as Map
import           Network.Socket            (HostName)

import           Communication
import           Protocol
import           TimeProtocol

--------------------------------------------------------------------------------
-- A combination of `connectAndRegister` and `runTimeServer`.

connectWithTimeServer :: MemberType
                      -> HostName
                      -> String
                      -> Bool
                      -> Timed (MessageChan, MessageChan, ID)

connectWithTimeServer mt host port silent =
  do (send, upstreamRecv, myID) <- connectAndRegister mt host port silent
     downstreamRecv <- runTimeServer send upstreamRecv myID silent
     return (send, downstreamRecv, myID)

--------------------------------------------------------------------------------
-- Runs `timeServer` in a new thread, and returns its downstream recv channel.

runTimeServer :: MessageChan
              -> MessageChan
              -> ID
              -> Bool
              -> Timed MessageChan

runTimeServer send recv myID silent =
  do downstreamRecv <- newChan
     _ <- fork $ do why <- timeServer send recv downstreamRecv myID
                             (\_ -> return ())
                    liftIO $ println $ "Time server thread died: " ++ why
                    killChanM downstreamRecv why
     return downstreamRecv
  where println = unless silent . putStrLn


--------------------------------------------------------------------------------
-- The time server intercepts messages on its recv channel, processes relevant
-- messages, and forwards all others to the downstreamRecv channel.
--
-- It uses the bully algorithm to elect a leader from among all of the time
-- servers in the cluster. The leader polls the other time servers for their
-- current time every 30 seconds, and adjusts everyone's time offset
-- accordingly. If a time server does not receive a query for 1 minute,
-- it starts a new leader election.

timeServer :: MessageChan
           -> MessageChan
           -> MessageChan
           -> ID
           -> (String -> IO ())
           -> Timed String

timeServer send recv downstreamRecv myID printFn =
  do timerAction <- newEmptyMVar :: Timed (MVar (Timed ()))
     timer       <- newEmptyMVar :: Timed (MVar ThreadId)
     times <- stm $ newTVar Map.empty :: Timed (TVar (Map ID ClockTime))
     convs <- stm $ newTVar Map.empty :: Timed (TVar (Map ID Conversation))
     _ <- fork . forever . join $ takeMVar timerAction

     let -- Cancels the current timer and sets a new one, which will run
         -- `action` after `delay` seconds.
         setTimer action delay =
           do tryTakeMVar timer >>= traverse_ killThread
              t <- fork $ do threadDelay (delay * seconds)
                             putMVar timerAction action
              putMVar timer t

         -- Becomes the leader, and sends an 'IWon' to everyone.
         becomeLeader = do sendBrc myID IWon send
                           debug "Became leader."

         -- Starts a new election and waits for a `LeaderOK`. Becomes the leader
         -- in 5 seconds if one is not received.
         beginElection = do sendBrc myID Election send
                            setTimer becomeLeader becomeLeaderDelay
                            debug "Starting election..."

         -- Queries all other time servers for their current time.
         queryTime = do time <- fmap clockTime newTimestamp
                        stm $ do writeTVar times (Map.fromList [(myID, time)])
                                 writeTVar convs Map.empty
                        sendBrc myID QueryTime send
                        setTimer updateTime updateTimeDelay

         -- Averages all received clock times from all other time servers,
         -- then sends out adjustments.
         updateTime = do debug "Updating time..."
                         tmap <- stm $ readTVar times
                         let myTime = tmap ! myID
                             omap   = Map.map (timeDiff myTime) tmap
                             avg    = Map.foldl (+) 0 omap /
                                      fromIntegral (Map.size omap)
                         updateClockOffset avg
                         debug $ "Adjusted clock by " ++ show avg
                         for_ (Map.toList (Map.delete myID omap)) $ \(i, off) ->
                           do conv <- stm $ do m <- readTVar convs
                                               return (m ! i)
                              sendRsp conv (AdjustTime (off - avg)) send
                         setTimer queryTime (queryTimeDelay - updateTimeDelay)

         -- Handles messages received over the `recv` channel.
         -- Its state value (of type `ID`) is the current leader's ID.
         handle :: ID -> Message -> Timed ID
         handle leader (Request conv (ReportTime t)) =
           do when (leader == myID) $
                stm $ do modifyTVar times (Map.insert (requester conv) t)
                         modifyTVar convs (Map.insert (requester conv) conv)
              return leader
         handle leader (Response conv (AdjustTime off)) =
           do when (leader == responder conv) $
                do updateClockOffset off
                   debug $ "Adjusted clock by " ++ show off
              return leader
         handle leader (Request conv LeaderOK) =
           do setTimer beginElection beginElectionDelay
              debug $ "Got OK from " ++ show (requester conv)
              return leader
         handle leader (Broadcast sender QueryTime) =
           do when (sender == leader && sender /= myID) $
                do setTimer beginElection beginElectionDelay
                   conv <- myID `to` leader
                   time <- fmap clockTime newTimestamp
                   sendReq conv (ReportTime time) send
              return leader
         handle leader (Broadcast otherID Election) =
           do debug $ "Got Election from " ++ show otherID
              when (myID > otherID) $
                do conv <- myID `to` otherID
                   sendReq conv LeaderOK send
                   beginElection
              when (myID < otherID) $ setTimer beginElection beginElectionDelay
              return leader
         handle _ (Broadcast leader IWon)
           | leader < myID  = beginElection >> return leader
           | leader == myID = queryTime >> return leader
           | otherwise = do setTimer beginElection beginElectionDelay
                            debug $ "LEADER IS NOW " ++ show leader
                            return leader
         handle leader msg = writeChanM downstreamRecv msg >> return leader

     beginElection
     finally (messageLoop recv ((lift .) . handle) myID)
             (tryTakeMVar timer >>= traverse_ killThread)

  where becomeLeaderDelay  = 5
        beginElectionDelay = 60
        queryTimeDelay     = 30
        updateTimeDelay    = 10
        debug :: (MonadIO m) => String -> m ()
        debug = liftIO . printFn

seconds :: Int
seconds = 1000000

