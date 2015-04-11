#!/bin/bash

X=./smarthome
PORT=8281

# Make some named pipes.
# ---------------------------------------------------------

rm -rf ./tmp
mkdir ./tmp

IN_MOTION=./tmp/motion-input
IN_DOOR=./tmp/door-input
IN_PRESENCE=./tmp/presence-input
IN_SECURITY=./tmp/security-input
OUT_DB=./tmp/database-output
OUT_MOTION=./tmp/motion-output
OUT_DOOR=./tmp/door-output
OUT_PRESENCE=./tmp/presence-output

mkfifo $IN_MOTION $IN_DOOR $IN_PRESENCE $IN_SECURITY
mkfifo $OUT_DB $OUT_MOTION $OUT_DOOR $OUT_PRESENCE

# Start the devices.
# ---------------------------------------------------------

echo "Starting gateway..."
$X gateway 0.0.0.0 $PORT > /dev/null &
PID_GATEWAY=$!
sleep 1

echo "Starting database..."
rm -f db.txt
$X database 127.0.0.1 $PORT silent > $OUT_DB &
PID_DB=$!
ID_DB=`head -n 1 $OUT_DB`

echo "Starting devices and controller..."

tail -f $IN_MOTION | $X motion 127.0.0.1 $PORT silent > $OUT_MOTION &
ID_MOTION=`head -n 1 $OUT_MOTION`

tail -f $IN_DOOR | $X door 127.0.0.1 $PORT silent > $OUT_DOOR &
ID_DOOR=`head -n 1 $OUT_DOOR`

tail -f $IN_PRESENCE | $X presence 127.0.0.1 $PORT silent > $OUT_PRESENCE &
ID_PRESENCE=`head -n 1 $OUT_PRESENCE`

tail -f $IN_SECURITY | $X control security 127.0.0.1 $PORT silent > test-output.txt &
PID_SECURITY=$!
echo $ID_DB > $IN_SECURITY
echo $ID_MOTION > $IN_SECURITY
echo $ID_DOOR > $IN_SECURITY
echo $ID_PRESENCE > $IN_SECURITY

# Send commands to the pipes.
# ---------------------------------------------------------

echo "Allowing time servers to sync (takes 30s)."

sleep 31

echo "TEST 1: Leaving home."

echo "on" > $IN_MOTION
sleep 0.5
echo "on" > $IN_DOOR
sleep 0.5
echo "off" > $IN_DOOR
sleep 0.5
echo "off" > $IN_MOTION
sleep 0.5

echo "TEST 2: Intruder arrives."

echo "on" > $IN_DOOR
sleep 0.5
echo "on" > $IN_MOTION
sleep 0.5
echo "off" > $IN_DOOR
sleep 0.5
echo "off" > $IN_MOTION
sleep 0.5

echo "TEST 3: Owner arrives."

echo "present" > $IN_PRESENCE
sleep 0.5
echo "on" > $IN_DOOR
sleep 0.5
echo "on" > $IN_MOTION
sleep 0.5
echo "off" > $IN_DOOR
sleep 0.5

echo "TEST 4: Owner leaves again."

echo "on" > $IN_DOOR
sleep 0.5
echo "off" > $IN_MOTION
sleep 0.5
echo "off" > $IN_DOOR
sleep 0.5

# Kill everything.
# ---------------------------------------------------------

echo "Done; killing processes."

echo "exit" > $IN_MOTION
echo "exit" > $IN_DOOR
echo "exit" > $IN_PRESENCE
sleep 1
fuser -k $IN_MOTION
fuser -k $IN_DOOR
fuser -k $IN_PRESENCE
fuser -k $IN_SECURITY
kill $PID_SECURITY
kill $PID_DB
kill $PID_GATEWAY
rm -rf ./tmp

