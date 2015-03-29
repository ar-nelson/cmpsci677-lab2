module Communication( connectToGateway
                    , messageLoop
                    , socketToChannels
                    , sendReq
                    , sendRsp
                    , MessageChan
                    , MessageHandler
) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad      (unless)
import           Data.Maybe         (fromMaybe)
import           Network.Socket     hiding (recv, send)
import           Prelude            hiding (catch)
import           Safe               (readMay)
import           System.IO
import           System.Random      (randomIO)

import           Protocol

--------------------------------------------------------------------------------

connectToGateway :: HostName
                 -> String
                 -> MessageChan
                 -> MessageChan
                 -> Bool
                 -> IO ()

connectToGateway host port send recv silent =
  catch openSocketChannels onError
  where openSocketChannels = withSocketsDo $
          do addrinfos <- getAddrInfo Nothing (Just host) (Just port)
             let serveraddr = head addrinfos
             sock <- socket (addrFamily serveraddr) Stream defaultProtocol
             setSocketOption sock KeepAlive 1
             connect sock (addrAddress serveraddr)
             socketToChannels sock send recv silent
        onError = writeChan recv . Left . show :: SomeException -> IO ()

--------------------------------------------------------------------------------

type MessageChan = Chan (Either String Message)

-- Creating a connection spawns two new threads (one for send, one for recv),
-- each of which uses the provided channels. If the connection is closed for any
-- reason, the threads will terminate.
--
-- An error value on the send channel will cause the connection to close, while
-- an error value on the recv channel indicates that something external closed
-- the connection.

sendLoop :: Handle -> MessageChan -> IO ()
recvLoop :: Handle -> MessageChan -> IO ()

--                  socket    send channel   recv channel   silent
socketToChannels :: Socket -> MessageChan -> MessageChan -> Bool -> IO ()
socketToChannels sock send recv silent =
  do h <- socketToHandle sock ReadWriteMode
     let println = unless silent . putStrLn
         killChan c = writeChan c . Left
         recvFail :: SomeException -> IO ()
         recvFail e = do println $ "Recv thread died: " ++ show e
                         killChan recv $ show e
                         killChan send $ show e
         defMsg = do killChan recv "Connection closed."
                     killChan send "Connection closed."
     recvThread <- forkIO $ catch (recvLoop h recv >> defMsg) recvFail

     let sendFail :: SomeException -> IO ()
         sendFail e = do println $ "Send thread died: " ++ show e
                         throwTo recvThread e
     _ <- forkIO $ catch (sendLoop h send) sendFail >> hClose h
     return ()

-- Messages are sent over the wire as strings, using Haskell's default
-- `read`/`show` serialization. They are separated by newlines; this works as
-- long as all machines in the network use the same line endings, but **there
-- may be problems** if you try to communicate between, say, a Linux machine and
-- a Windows machine.

sendLoop h chan =
  do next <- readChan chan
     case next of Right message -> hPrint h message >> sendLoop h chan
                  Left err -> error err

recvLoop h chan =
  do message <- fmap parse (hGetLine h)
     writeChan chan $ Right message
     recvLoop h chan
  where parse s = fromMaybe (Unknown s) (readMay s :: Maybe Message)

--------------------------------------------------------------------------------

-- The message handler is a function that takes a current state and a message,
-- performs some I/O action(s), then returns either
--
-- * `Right st`, a new state for the next message loop iteration, or
-- * `Left String`, an error message which ends the loop.

type MessageHandler st = st -> Message -> IO (Either String st)

messageLoop :: MessageChan -> MessageHandler st -> st -> IO String
messageLoop c h s = mapRight (mapRight (messageLoop c h) . h s) (readChan c)
mapRight fn = (>>= \v -> case v of {Right r -> fn r; Left s -> return s})

sendReq :: Request -> MessageChan -> IO MsgID
sendReq req chan = do mid <- randomIO :: IO MsgID
                      writeChan chan . Right $ Req mid req
                      return mid

sendRsp :: MsgID -> Response -> MessageChan -> IO ()
sendRsp mid rsp chan = writeChan chan . Right $ Rsp mid rsp

