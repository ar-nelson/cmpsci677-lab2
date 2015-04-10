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
           where lookup (id, QueryRegister)    = Map.lookup id (dbRegister db)
                 lookup (id, QueryLeave)       = Map.lookup id (dbLeave db)
                 lookup (id, QueryReportState) = Map.lookup id (dbState db)
                 lookup (_,  QueryChangeMode)  = dbMode db
                 lookup (id, QueryTextMessage) = Map.lookup id (dbTextMsg db)
                 lookup (id, QueryPresent)     = Map.lookup id (dbPresent db)

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

data DB = DB { dbRegister :: Map ID DBEntry
             , dbLeave    :: Map ID DBEntry
             , dbState    :: Map ID DBEntry
             , dbMode     :: Maybe DBEntry
             , dbTextMsg  :: Map ID DBEntry
             , dbPresent  :: Map ID DBEntry
             }

reloadDatabase :: Timed DB
reloadDatabase =
  catch (do contents <- liftIO $ readFile dbFilename
            let entries = mapMaybe readMay (lines contents)
                addAndUpdate db e =
                  updateLamport (lamportTime ts) >> return (addEntry db e)
                  where DBEntry _ ts _ = e
            foldM addAndUpdate initDB entries
        ) $ \e -> if isDoesNotExistError e then return initDB
                                           else liftIO $ ioError e
  where initDB = DB Map.empty Map.empty Map.empty Nothing Map.empty Map.empty

addEntry :: DB -> DBEntry -> DB
addEntry db entry =
  case event of
    RegisterEvent{} -> db { dbRegister = addTo (dbRegister db) }
    LeaveEvent{}    -> db { dbLeave    = addTo (dbLeave db) }
    BroadcastEvent ChangeMode{}  -> db { dbMode = Just entry }
    BroadcastEvent ReportState{} ->
      db { dbState   = addTo (dbState db) }
    BroadcastEvent TextMessage{} ->
      db { dbTextMsg = addTo (dbTextMsg db) }
    BroadcastEvent Present ->
      db { dbPresent = addTo (dbPresent db) }
    _ -> db
  where DBEntry eid _ event = entry
        addTo = Map.insert eid entry

