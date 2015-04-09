module Gateway(startGateway) where

import           Control.Concurrent.Lifted
import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Monad.Trans
import           Data.Foldable             (for_)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Network.Socket            hiding (Broadcast, recv, send)
import           Prelude                   hiding (catch)

import           Communication
import           Protocol
import           TimeProtocol

--------------------------------------------------------------------------------

-- The gateway's core functionality is a TCP server that receives connections
-- from other devices, then routes messages between the devices.

startGateway :: String -> Timed ()
startGateway port =
  do sock <- liftIO $ withSocketsDo $
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
     st   <- liftIO initGatewayState
     procConnections st sock
  where procConnections :: GatewayState -> Socket -> Timed ()
        procConnections st mastersock =
          do (connsock, clientaddr) <- liftIO (accept mastersock)
             send <- newChan :: Timed MessageChan
             recv <- newChan :: Timed MessageChan
             socketToChannels connsock send recv False
             stm (addChannel clientaddr send st)
             fork $ finally (routeMessages st clientaddr send recv Nothing)
                            (stm (removeChannel clientaddr st))
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
              -> Timed ()

routeMessages st addr send recv myID =
  catch (fmap Right (readChanM recv >>= route)) (return . Left)
    >>= either err (routeMessages st addr send recv)
  where
    route :: Message -> Timed (Maybe ID)
    route (Request conv req)
      | responder conv == gatewayID = handleLocally conv req
      | otherwise = stm (getMember (responder conv) st) >>= fwdTo >> return myID
      where fwdTo (Just e) = writeChanM (memberSendChan e) $ Request conv req
            fwdTo Nothing  = writeChanM send $
              Response conv (NotFound (responder conv))
    route (Response conv rsp)
      | requester conv == gatewayID = return myID -- Ignore responses.
      | otherwise = stm (getMember (requester conv) st) >>= fwdTo >> return myID
      where fwdTo (Just e) = writeChanM (memberSendChan e) $ Response conv rsp
            fwdTo Nothing  = liftIO $ putStrLn $ "Failed to deliver response "
                               ++ show rsp ++ " to " ++ show (requester conv)
                               ++ ": no member with this ID."
    route (Broadcast i brc) =
      do ms <- stm (listMembers st)
         for_ ms $ \m -> writeChanM (memberSendChan m) (Broadcast i brc)
         return myID
    route msg = do liftIO $ putStrLn $ "Cannot route message " ++ show msg
                   return myID

    -- The `Register` request is handled by the gateway directly, and stores
    -- a known device ID in the gateway's state.

    handleLocally conv (Register m) =
      do i <- stm register
         liftIO $ putStrLn $ "Registered " ++ show m ++ " with " ++ show i
         writeChanM send $ Response conv (RegisteredAs i)
         return (Just i)
      where register = do for_ myID $ flip removeMember st
                          ni <- nextID st
                          let entry = MemberEntry { memberID       = ni
                                                  , memberAddr     = addr
                                                  , memberType     = m
                                                  , memberSendChan = send }
                          addMember entry st
                          return ni
    handleLocally conv req =
      writeChanM send (Response conv (NotSupported Gateway req)) >> return myID

    err :: SomeException -> Timed ()
    err e = liftIO $ do putStrLn $
                          "Connection to " ++ show addr ++ " closed: " ++ show e
                        for_ myID $ \i -> atomically (removeMember i st)
                          >> putStrLn ("Removed device with " ++ show i)

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
                         , idCounter    = ic }

data MemberEntry = MemberEntry {
  memberID       :: ID,
  memberAddr     :: SockAddr,
  memberType     :: MemberType,
  memberSendChan :: MessageChan
}

addChannel    :: SockAddr -> MessageChan -> GatewayState -> STM ()
removeChannel :: SockAddr -> GatewayState -> STM ()
getMember     :: ID -> GatewayState -> STM (Maybe MemberEntry)
addMember     :: MemberEntry -> GatewayState -> STM ()
removeMember  :: ID -> GatewayState -> STM ()
listMembers   :: GatewayState -> STM [MemberEntry]
nextID        :: GatewayState -> STM ID

addChannel addr ch st = modifyTVar (sendChannels st) (Map.insert addr ch)
removeChannel addr st = modifyTVar (sendChannels st) (Map.delete addr)
getMember i st        = fmap (Map.lookup i) (readTVar (members st))
addMember e st        = modifyTVar (members st) (Map.insert (memberID e) e)
removeMember i st     = modifyTVar (members st) (Map.delete i)
listMembers st        = fmap (fmap snd . Map.toList) (readTVar (members st))
nextID GatewayState{idCounter = var} = readTVar var >>= swapTVar var . (+ 1)

