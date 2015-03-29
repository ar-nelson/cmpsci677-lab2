module Main where

import           System.Environment

import           Controllers
import           Devices
import           Gateway
import           Protocol

usage :: IO ()
usage = do
  prog <- getProgName
  putStrLn $ unlines [
    "Usage: " ++ prog ++ " <command> <host> <port> [silent]",
    "  <command> is one of [temp, motion, bulb, outlet, gateway, control <x>],",
    "    where <x> is one of [heater, light, user, testlog].",
    "  <host> is the gateway hostname (0.0.0.0 for the gateway itself).",
    "  <port> is the gateway TCP port.",
    "  'silent' is optional, and not valid with 'gateway'. If provided,",
    "    1. console output will be minimal, and",
    "    2. devices will output only their device ID on a line by itself."]

main :: IO ()
main =
  do args <- getArgs
     case args of
       ["control", c, host, port, "silent"] -> control c host port True
       ["control", c, host, port]           -> control c host port False
       [command, host, port, "silent"]      -> start command host port True
       [command, host, port]                -> start command host port False
       _ -> usage

start :: String -> String -> String -> Bool -> IO ()
start "temp"    h p    s = startDevice Temp h p s
start "motion"  h p    s = startDevice Motion h p s
start "bulb"    h p    s = startDevice Bulb h p s
start "outlet"  h p    s = startDevice Outlet h p s
start "gateway" _ port _ = startGateway port
start _ _ _ _ = usage

control :: String -> String -> String -> Bool -> IO ()
control "heater"  h p s = startController Heater h p s
control "light"   h p s = startController Light h p s
control "user"    h p s = startController UserInterface h p s
control "testlog" h p s = startController TestLogger h p s
control _ _ _ _ = usage

