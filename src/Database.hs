module Database(startDatabase, dbFilename) where

import           Control.Exception.Lifted (catch)
import           Control.Monad
import           Control.Monad.Trans
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Maybe               (mapMaybe)
import           Network.Socket           (HostName)
import           Prelude                  hiding (catch, id, lookup)
import           Safe
import           System.IO
import           System.IO.Error          hiding (catch)

import           Communication
import           Protocol
import           TimeProtocol
import           TimeServer

dbFilename :: FilePath
dbFilename = "db.txt"

type DB = Map (ID, DBEvent) DBEntry

startDatabase :: HostName -> String -> Bool -> Timed ()
startDatabase host port silent =
  do initialDB <- reloadDatabase
     dbFile    <- liftIO $ openFile dbFilename AppendMode

     (send, recv, _) <- connectWithTimeServer Database host port silent
     println "Starting database..."

     let handle :: DB -> Message -> Timed DB

         handle db (Request conv (DBInsert entry)) =
           do sendRsp conv Success send
              writeEntry entry
              return (db `addEntry` entry)

         handle db (Request conv (DBQuery queries)) =
           sendRsp conv (DBResultSet (map lookup queries)) send >> return db
           where lookup = fmap (\(DBEntry _ ts _) -> ts) . flip Map.lookup db

         handle db (Request conv req) =
           sendRsp conv (NotSupported Database req) send >> return db

         handle db _ = return db

         writeEntry :: DBEntry -> Timed ()
         writeEntry entry = do println (show entry)
                               liftIO $ hPrint dbFile entry
                               liftIO $ hFlush dbFile

     why <- messageLoop recv ((lift .) . handle) initialDB
     println ("Database died: " ++ why)
     liftIO $ hClose dbFile

  where println = liftIO . unless silent . putStrLn

reloadDatabase :: Timed DB
reloadDatabase = catch (liftIO (readFile dbFilename) >>= reload) err
  where initDB = Map.empty :: DB
        reload contents = foldM addAndUpdate initDB entries
          where entries = mapMaybe readMay (lines contents)
                addAndUpdate db e = do updateLamport (lamportTime time)
                                       return (addEntry db e)
                                    where DBEntry _ time _ = e
        err e | isDoesNotExistError e = return initDB
              | otherwise             = liftIO $ ioError e

addEntry :: DB -> DBEntry -> DB
addEntry db (DBEntry id newTime event) =
  case Map.lookup (id, event) db of
    Just (DBEntry _ oldTime _) ->
      if lamportTime newTime >= lamportTime oldTime then update else db
    Nothing -> update
  where update = Map.insert (id, event) (DBEntry id newTime event) db

