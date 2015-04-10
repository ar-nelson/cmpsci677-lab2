{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Communication( connectToGateway
                    , registerWithGateway
                    , connectAndRegister
                    , messageLoop
                    , socketToChannels
                    , killChanM
                    , readChanM
                    , writeChanM
                    , sendReq
                    , sendRsp
                    , sendBrc
                    , to
                    , console
                    , MessageM
                    , MessageChan
                    , MessageHandler
                    , HandlerState
) where

import           Control.Concurrent.Lifted
import           Control.Exception.Lifted
import           Control.Monad.Base
import           Control.Monad.Except
import           Control.Monad.Reader
import           Network.Socket            hiding (Broadcast, recv, send)
import           Prelude                   hiding (catch)
import           Safe                      (readMay)
import           System.IO
import           System.Random

import           Protocol
import           TimeProtocol

--------------------------------------------------------------------------------
-- Each device, when it starts, contacts the gateway and attempts to
--
-- * establish a TCP connection, and
-- * send a `Register` command and acquire a device ID.
--
-- If either step fails, an exception will be thrown and the application will
-- close.

connectToGateway :: HostName
                 -> String
                 -> MessageChan
                 -> MessageChan
                 -> Bool
                 -> Timed ()

connectToGateway host port send recv silent =
  catch openSocketChannels onError
  where openSocketChannels =
          liftIO (withSocketsDo $
            do addrinfos <- getAddrInfo Nothing (Just host) (Just port)
               let serveraddr = head addrinfos
               sock <- socket (addrFamily serveraddr) Stream defaultProtocol
               setSocketOption sock KeepAlive 1
               connect sock (addrAddress serveraddr)
               return sock
          ) >>= \sock -> socketToChannels sock send recv silent
        onError = killChanM recv . show :: SomeException -> Timed ()

registerWithGateway :: MemberType
                    -> MessageChan
                    -> MessageChan
                    -> Bool
                    -> Timed ID

registerWithGateway mt send recv silent =
  do conv <- invalidID `to` gatewayID
     sendReq conv (Register mt) send
     handleLoop (conversationID conv)
  where handleLoop cid = readChanM recv >>= \m -> case m of
          Response Conversation {conversationID = cid'} (RegisteredAs (ID i)) ->
            if cid == cid'
               then liftIO $ do unless silent $ putStr "Connected with ID "
                                print i
                                hFlush stdout
                                return (ID i)
               else handleLoop cid
          Response Conversation {conversationID = cid'} rsp ->
            if cid == cid'
               then error $ "Invalid response to Register: " ++ show rsp
               else handleLoop cid
          _ -> handleLoop cid

connectAndRegister :: MemberType
                   -> HostName
                   -> String
                   -> Bool
                   -> Timed (MessageChan, MessageChan, ID)

connectAndRegister mt host port silent =
  do send <- newChan
     recv <- newChan
     connectToGateway host port send recv silent
     myID <- registerWithGateway mt send recv silent
     return (send, recv, myID)

--------------------------------------------------------------------------------

type MessageM    = Either String (Timestamped Message)
type MessageChan = Chan MessageM

killChanM :: MonadBase IO m => MessageChan -> String -> m ()
killChanM c = writeChan c . fail

readChanM :: (MonadBase IO m, MonadIO m, MonadReader TimeState m)
          => MessageChan -> m Message
readChanM c = runExceptT (ExceptT (readChan c) >>= unstamp)
                >>= either error return

writeChanM :: (MonadBase IO m, MonadIO m, MonadReader TimeState m)
           => MessageChan -> Message -> m ()
writeChanM c m = stamp m >>= writeChan c . return

-- Creating a connection spawns two new threads (one for send, one for recv),
-- each of which uses the provided channels. If the connection is closed for any
-- reason, the threads will terminate.
--
-- An error value on the send channel will cause the connection to close, while
-- an error value on the recv channel indicates that something external closed
-- the connection.

socketToChannels :: Socket
                 -> MessageChan
                 -> MessageChan
                 -> Bool
                 -> Timed ()

socketToChannels sock send recv silent =
  do h <- liftIO (socketToHandle sock ReadWriteMode)
     let recvFail :: SomeException -> Timed ()
         recvFail e = do println $ "Recv thread died: " ++ show e
                         killChanM recv $ show e
                         killChanM send $ show e
         defMsg = do killChanM recv "Connection closed."
                     killChanM send "Connection closed."
     recvThread <- fork $ catch (recvLoop h recv >> defMsg) recvFail
     let sendFail :: SomeException -> Timed ()
         sendFail e = do println $ "Send thread died: " ++ show e
                         throwTo recvThread e
     _ <- fork $ finally (catch (sendLoop h send) sendFail) (liftIO (hClose h))
     return ()
  where println = liftIO . unless silent . putStrLn

-- Messages are sent over the wire as strings, using Haskell's default
-- `read`/`show` serialization. They are separated by newlines; this works as
-- long as all machines in the network use the same line endings, but **there
-- may be problems** if you try to communicate between, say, a Linux machine and
-- a Windows machine.

sendLoop :: Handle -> MessageChan -> Timed ()
sendLoop h chan = readChan chan >>= either error (liftIO . hPrint h)
                                >>  sendLoop h chan

recvLoop :: Handle -> MessageChan -> Timed ()
recvLoop h chan = liftIO (hGetLine h) >>= parse >>= toChan >> recvLoop h chan
  where parse s = maybe (stamp $ Unknown s) return (readMay s)
        toChan  = writeChan chan . return

--------------------------------------------------------------------------------

-- The message handler is a function that takes a current state and a message,
-- performs some I/O action(s), then returns either
--
-- * `Right st`, a new state for the next message loop iteration, or
-- * `Left String`, an error message which ends the loop.

type HandlerState   st = ExceptT String Timed st
type MessageHandler st = st -> Message -> HandlerState st

messageLoop :: MessageChan -> MessageHandler st -> st -> Timed String
messageLoop chan handler st =
  runExceptT (ExceptT (readChan chan) >>= unstamp >>= handler st)
    >>= either return (messageLoop chan handler)

sendReq :: (MonadBase IO m, MonadIO m, MonadReader TimeState m)
        => Conversation -> Request -> MessageChan -> m ()
sendReq c req chan = writeChanM chan $ Request c req

sendRsp :: (MonadBase IO m, MonadIO m, MonadReader TimeState m)
        => Conversation -> Response -> MessageChan -> m ()
sendRsp c rsp chan = writeChanM chan $ Response c rsp

sendBrc :: (MonadBase IO m, MonadIO m, MonadReader TimeState m)
        => ID -> Broadcast -> MessageChan -> m ()
sendBrc i brc chan = writeChanM chan $ Broadcast i brc

to :: MonadIO m => ID -> ID -> m Conversation
req `to` rsp = do cid <- liftIO randomIO
                  return Conversation { requester = req
                                      , responder = rsp
                                      , conversationID = cid
                                      }

--------------------------------------------------------------------------------

console :: MessageChan -> MessageChan -> Bool -> Timed ()

console send recv silent =
  do unless silent $ liftIO (putStr "> " >> hFlush stdout)
     input <- liftIO getLine
     if input == "exit" then exit else eval input
  where exit       = unless silent $ liftIO (putStrLn "Goodbye!")
        eval input = do writeChanM recv (UserInput input)
                        unless silent $ threadDelay 1000000
                        console send recv silent

