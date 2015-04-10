module Gateway(startGateway) where

import           Control.Concurrent.Lifted
import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.Except
import           Data.Foldable             (for_, traverse_)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Network.Socket            hiding (Broadcast, recv, send)
import           Prelude                   hiding (catch)

import           Communication
import           Protocol
import           TimeProtocol
import           TimeServer

--------------------------------------------------------------------------------

-- The gateway's core functionality is a TCP server that receives connections
-- from other devices, then routes messages between the devices.

startGateway :: String -> Timed ()
startGateway port =
  do st       <- liftIO initGatewayState
     timeSend <- newChan
     timeRecv <- newChan

     sock <- liftIO . withSocketsDo $
               do putStrLn $ "Starting gateway on port " ++ port ++ "..."
                  addrinfos <- getAddrInfo
                               (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                               Nothing (Just port)
                  let serveraddr = head addrinfos
                  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
                  bindSocket sock (addrAddress serveraddr)
                  putStrLn "Listening for connections."
                  listen sock 5
                  return sock

     gwRecv    <- runTimeServer timeSend timeRecv gatewayID False
     let gwSend = timeSend

     stm $ addMember MemberEntry { memberID = gatewayID
                                 , memberType = Gateway
                                 , memberSendChan = timeRecv
                                 } st
     void . fork . forever $
       readChanM gwRecv >>= \m -> case m of
          Request conv req ->
            stm (getMember (requester conv) st) >>= traverse_
              (sendRsp conv (NotSupported Gateway req) . memberSendChan)
          _ -> return ()
     void . fork $ routeMessages st "Local Time Server" gwRecv gwSend
                   (Just gatewayID)

     forever $ do (connsock, clientaddr) <- liftIO (withSocketsDo (accept sock))
                  send <- newChan
                  recv <- newChan
                  socketToChannels connsock send recv False
                  stm $ addChannel clientaddr send st
                  void . fork $ finally
                    (routeMessages st (show clientaddr) send recv Nothing)
                    (stm $ removeChannel clientaddr st)

-- The gateway takes all messages received from connected clients (whether they
-- are registered as devices or not), and either handles them itself or routes
-- them to their destination.
--
-- Most messages are forwarded; `Request`s are forwarded to a target device (if
-- it exists), and `Response`s are forwarded to the sender of the relevant
-- `Request` (determined by the message's `MsgID`, which should be stored in the
-- gateway's global state). `Broadcast`s are forwarded to all subscribed
-- controllers.

routeMessages :: GatewayState
              -> String
              -> MessageChan
              -> MessageChan
              -> Maybe ID
              -> Timed ()

routeMessages st addr send recv myID =
  catch (runExceptT $ do smsg <- ExceptT (readChan recv)
                         msg  <- unstamp smsg
                         printDebug msg
                         lift (route (timestampOf smsg) msg)
        ) (return . Left . (show :: SomeException -> String))
    >>= either err (routeMessages st addr send recv)
  where
    route :: Timestamp -> Message -> Timed (Maybe ID)

    route timestamp (Request conv req)
      | responder conv == gatewayID = handleLocally timestamp conv req
      | otherwise = stm (getMember (responder conv) st) >>= fwdTo >> return myID
      where fwdTo (Just e) = writeChanM (memberSendChan e) $ Request conv req
            fwdTo Nothing  = writeChanM send $
              Response conv (NotFound (responder conv))

    route _ (Response conv rsp)
      | requester conv == gatewayID =
          do Just MemberEntry{memberSendChan=ch} <- stm $ getMember gatewayID st
             sendRsp conv rsp ch
             return myID
      | otherwise = stm (getMember (requester conv) st) >>= fwdTo >> return myID
      where fwdTo (Just e) = sendRsp conv rsp (memberSendChan e)
            fwdTo Nothing  = liftIO . putStrLn $ "Failed to deliver response "
                               ++ show rsp ++ " to " ++ show (requester conv)
                               ++ ": no member with this ID."

    route time (Broadcast i brc) =
      do ms  <- stm $ listMembers st
         for_ ms $ \MemberEntry{memberID=mi,memberType=mt,memberSendChan=mch} ->
           sendBrc i brc mch
           >> when (mt == Database) (gatewayID `to` mi >>= \c ->
                sendReq c (DBInsert (DBEntry i time (BroadcastEvent brc))) mch)
         return myID

    route _ msg = do liftIO $ putStrLn ("Cannot route message " ++ show msg)
                     return myID

    -- The `Register` request is handled by the gateway directly, and stores
    -- a known device ID in the gateway's state.

    handleLocally time conv (Register m) =
      do i <- stm register
         liftIO $ putStrLn ("Registered " ++ show m ++ " with " ++ show i)
         sendRsp conv (RegisteredAs i) send
         dbs <- stm $ dbMembers st
         for_ dbs $ \MemberEntry { memberID = dbi, memberSendChan = dbch } ->
           do conv' <- gatewayID `to` dbi
              sendReq conv' (DBInsert (DBEntry i time (RegisterEvent m))) dbch
         return (Just i)
      where register = do for_ myID $ flip removeMember st
                          ni <- nextID st
                          addMember MemberEntry { memberID       = ni
                                                , memberType     = m
                                                , memberSendChan = send
                                                } st
                          return ni

    handleLocally _ conv req =
      do Just MemberEntry {memberSendChan = ch} <- stm $ getMember gatewayID st
         sendReq conv req ch
         return myID

    --printDebug msg = liftIO . putStrLn $ show myID ++ " sent " ++ show msg
    printDebug _ = return ()

    err :: String -> Timed ()
    err e = do liftIO $ putStrLn ("Connection to " ++ addr ++ " closed: " ++ e)
               for_ myID $ \i ->
                 do stm (removeMember i st)
                    liftIO $ putStrLn ("Removed device with " ++ show i)
                    dbs  <- stm $ dbMembers st
                    time <- newTimestamp
                    for_ dbs $ \MemberEntry{memberID=dbi,memberSendChan=dbch} ->
                      gatewayID `to` dbi >>= \c ->
                        sendReq c (DBInsert (DBEntry i time LeaveEvent)) dbch

--------------------------------------------------------------------------------

data GatewayState = GatewayState {
  sendChannels :: TVar (Map SockAddr MessageChan),
  members      :: TVar (Map ID MemberEntry),
  idCounter    :: TVar ID
}

initGatewayState :: IO GatewayState
initGatewayState = atomically $
  do sc <- newTVar (Map.empty :: Map SockAddr MessageChan)
     m  <- newTVar (Map.empty :: Map ID MemberEntry)
     ic <- newTVar (gatewayID + 1)
     return GatewayState { sendChannels = sc
                         , members      = m
                         , idCounter    = ic
                         }

data MemberEntry = MemberEntry {
  memberID       :: ID,
  memberType     :: MemberType,
  memberSendChan :: MessageChan
}

addChannel    :: SockAddr -> MessageChan -> GatewayState -> STM ()
removeChannel :: SockAddr -> GatewayState -> STM ()
getMember     :: ID -> GatewayState -> STM (Maybe MemberEntry)
addMember     :: MemberEntry -> GatewayState -> STM ()
removeMember  :: ID -> GatewayState -> STM ()
listMembers   :: GatewayState -> STM [MemberEntry]
dbMembers     :: GatewayState -> STM [MemberEntry]
nextID        :: GatewayState -> STM ID

addChannel addr ch st = modifyTVar (sendChannels st) (Map.insert addr ch)
removeChannel addr st = modifyTVar (sendChannels st) (Map.delete addr)
getMember i st        = fmap (Map.lookup i) (readTVar (members st))
addMember e st        = modifyTVar (members st) (Map.insert (memberID e) e)
removeMember i st     = modifyTVar (members st) (Map.delete i)
listMembers st        = fmap (fmap snd . Map.toList) (readTVar (members st))
dbMembers st          = filter ((==Database) . memberType) `fmap` listMembers st
nextID GatewayState{idCounter = var} = readTVar var >>= swapTVar var . (+ 1)

