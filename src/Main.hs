module Main where

import           Control.Monad.Trans (liftIO)
import           System.Environment

import           Controllers
import           Database
import           Devices
import           Gateway
import           Protocol
import           TimeProtocol

usage :: Timed ()
usage = liftIO $ getProgName >>= \prog -> putStrLn $ unlines
  [ "Usage: " ++ prog ++ " <command> <host> <port> [silent]"
  , "  <command> is the subprogram to run. It may be one of:"
  , "    - temp"
  , "    - motion"
  , "    - door"
  , "    - presence"
  , "    - bulb"
  , "    - outlet"
  , "    - gateway"
  , "    - database"
  , "    - control heater"
  , "    - control light"
  , "    - control user"
  , "    - control testlog"
  , "  <host> is the gateway hostname (0.0.0.0 for the gateway itself)."
  , "  <port> is the gateway TCP port."
  , "  'silent' is optional, and not valid with 'gateway'. If provided,"
  , "    1. console output will be minimal, and"
  , "    2. devices will output only their device ID on a line by itself."
  ]

main :: IO ()
main =
  do time <- initTimeState
     args <- getArgs
     flip runTimed time $ case args of
       ["control", c, host, port, "silent"] -> control c host port True
       ["control", c, host, port]           -> control c host port False
       [command, host, port, "silent"]      -> start command host port True
       [command, host, port]                -> start command host port False
       _ -> usage

start :: String -> String -> String -> Bool -> Timed ()
start "temp"     h p    s = startDevice Temp h p s
start "motion"   h p    s = startDevice Motion h p s
start "door"     h p    s = startDevice Door h p s
start "presence" h p    s = startDevice Presence h p s
start "bulb"     h p    s = startDevice Bulb h p s
start "outlet"   h p    s = startDevice Outlet h p s
start "gateway"  _ port _ = startGateway port
start "database" h p    s = startDatabase h p s
start _ _ _ _ = usage

control :: String -> String -> String -> Bool -> Timed ()
control "heater"  h p s = startController Heater h p s
control "light"   h p s = startController Light h p s
control "user"    h p s = startController UserInterface h p s
control "testlog" h p s = startController TestLogger h p s
control _ _ _ _ = usage

