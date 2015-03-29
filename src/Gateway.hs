module Gateway(startGateway) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception      (finally)
import           Data.Foldable          (for_)
import           Data.List              (delete)
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Maybe             (mapMaybe)
import           Network.Socket         hiding (recv, send)

import           Communication
import           Protocol

--------------------------------------------------------------------------------

-- The gateway's core functionality is a TCP server that receives connections
-- from other devices, then routes messages between the devices.

startGateway :: String -> IO ()
startGateway port = withSocketsDo $
  do putStrLn $ "Starting gateway on port " ++ port ++ "..."
     addrinfos <- getAddrInfo
                  (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                  Nothing (Just port)
     let serveraddr = head addrinfos
     sock <- socket (addrFamily serveraddr) Stream defaultProtocol
     bindSocket sock (addrAddress serveraddr)
     putStrLn "Listening for connections."
     listen sock 5
     st <- initGatewayState
     procConnections st sock
  where procConnections :: GatewayState -> Socket -> IO ()
        procConnections st mastersock =
          do (connsock, clientaddr) <- accept mastersock
             send <- newChan :: IO MessageChan
             recv <- newChan :: IO MessageChan
             socketToChannels connsock send recv False
             atomically (addChannel clientaddr send st)
             forkIO $ finally (routeMessages st clientaddr send recv Nothing)
                              (atomically (removeChannel clientaddr st))
             procConnections st mastersock

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
              -> SockAddr
              -> MessageChan
              -> MessageChan
              -> Maybe ID
              -> IO ()

routeMessages st addr send recv myID =
  do next <- readChan recv
     case next of
       Right (Req mid req) -> handleReq mid req >>= recur
       Right (Rsp mid _) ->
         let fwdTo (Just chan) = writeChan chan next
             fwdTo Nothing = putStrLn $
               "Failed to deliver response to message #" ++ show mid
         in (atomically (getSenderChannel mid st) >>= fwdTo) >> recur myID
       Right (Brc brc) ->
         do chans <- atomically (getSubscriberChannels st)
            mapM_ (`sendMsg` Brc brc) chans
            recur myID
       Right (UserInput s) -> putStrLn $ "Cannot route UserInput: '" ++ s ++ "'"
       Right (Unknown s) -> putStrLn $ "Unparseable message: '" ++ s ++ "'"
       Left err ->
         do putStrLn $ "Connection to " ++ show addr ++ " closed: " ++ err
            for_ myID $ \i -> do atomically $ removeDevice i st
                                 putStrLn $ "Removed device with " ++ show i
  where
    recur = routeMessages st addr send recv
    sendMsg chan = writeChan chan . Right
    handleReq :: MsgID -> Request -> IO (Maybe ID)

    -- The `Register` request is handled by the gateway directly, and stores a known
    -- device ID in the gateway's global state.
    handleReq mid (Register d) =
      do i <- atomically register
         putStrLn $ "Registered " ++ show d ++ " with " ++ show i
         sendMsg send $ Rsp mid (RegisteredAs i)
         return (Just i)
      where register = do for_ myID $ flip removeDevice st
                          ni <- nextID st
                          let entry = DeviceEntry { deviceID       = ni
                                                  , deviceAddr     = addr
                                                  , deviceType     = d
                                                  , deviceSendChan = send }
                          addDevice entry st
                          return ni

    -- The `Subscribe` request is also handled by the gateway, and adds the sender to
    -- the list of subscribed controllers.
    handleReq mid Subscribe =
      do atomically (addSubscriber addr st)
         putStrLn $ "Subscribed controller at " ++ show addr
         sendMsg send $ Rsp mid Success
         return myID

    -- All other requests are forwarded to a specificed device, if it exists.
    handleReq mid req =
      fwdReq mid req devID >> return myID
      where devID = case req of QueryState  i   -> i
                                ChangeState i _ -> i
    fwdReq mid req i =
      atomically (getDevice i st) >>= fwdTo
      where fwdTo (Just e) = do atomically (addSender mid addr st)
                                sendMsg (deviceSendChan e) $ Req mid req
            fwdTo Nothing = sendMsg send $ Rsp mid (NoDevice i)

--------------------------------------------------------------------------------

data GatewayState = GatewayState {
  sendChannels :: TVar (Map SockAddr MessageChan),
  senders      :: TVar (Map MsgID SockAddr),
  devices      :: TVar (Map ID DeviceEntry),
  idCounter    :: TVar ID,
  subscribers  :: TVar [SockAddr]
}

initGatewayState :: IO GatewayState
initGatewayState = atomically $
  do sc <- newTVar (Map.empty :: Map SockAddr MessageChan)
     s  <- newTVar (Map.empty :: Map MsgID SockAddr)
     d  <- newTVar (Map.empty :: Map ID DeviceEntry)
     ic <- newTVar (ID 1)
     sb <- newTVar ([] :: [SockAddr])
     return GatewayState { sendChannels = sc
                         , devices      = d
                         , senders      = s
                         , idCounter    = ic
                         , subscribers  = sb }

data DeviceEntry = DeviceEntry {
  deviceID       :: ID,
  deviceAddr     :: SockAddr,
  deviceType     :: Device,
  deviceSendChan :: MessageChan
}

addChannel       :: SockAddr -> MessageChan -> GatewayState -> STM ()
removeChannel    :: SockAddr -> GatewayState -> STM ()
getDevice        :: ID -> GatewayState -> STM (Maybe DeviceEntry)
addDevice        :: DeviceEntry -> GatewayState -> STM ()
removeDevice     :: ID -> GatewayState -> STM ()
addSender        :: MsgID -> SockAddr -> GatewayState -> STM ()
removeSender     :: MsgID -> GatewayState -> STM ()
getSenderChannel :: MsgID -> GatewayState -> STM (Maybe MessageChan)
nextID           :: GatewayState -> STM ID
addSubscriber    :: SockAddr -> GatewayState -> STM ()
removeSubscriber :: SockAddr -> GatewayState -> STM ()
getSubscriberChannels :: GatewayState -> STM [MessageChan]

addChannel addr ch st = modifyTVar (sendChannels st) (Map.insert addr ch)
removeChannel addr st = modifyTVar (sendChannels st) (Map.delete addr)
getDevice i st        = fmap (Map.lookup i) (readTVar (devices st))
addDevice e st        = modifyTVar (devices st) (Map.insert (deviceID e) e)
removeDevice i st     = modifyTVar (devices st) (Map.delete i)
addSender i addr st   = modifyTVar (senders st) (Map.insert i addr)
removeSender i st     = modifyTVar (senders st) (Map.delete i)
addSubscriber a st    = modifyTVar (subscribers st) $
  \as -> if a `elem` as then as else a : as
removeSubscriber a st = modifyTVar (subscribers st) (delete a)
getSubscriberChannels st = do sb <- readTVar (subscribers st)
                              sc <- readTVar (sendChannels st)
                              return $ mapMaybe (`Map.lookup` sc) sb
getSenderChannel i st =
  do s  <- readTVar (senders st)
     sc <- readTVar (sendChannels st)
     removeSender i st
     return $ Map.lookup i s >>= flip Map.lookup sc
nextID GatewayState{idCounter = var} =
  readTVar var >>= swapTVar var . \(ID n) -> ID (n + 1)

