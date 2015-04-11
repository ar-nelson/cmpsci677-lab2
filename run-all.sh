#!/bin/bash
echo "Creating named pipes..."
rm -rf ./tmp
mkdir ./tmp

X=./smarthome
PORT=9191

# ------------------------------------------------------------------------------

IN_TEMP=./tmp/temp-input
IN_MOTION=./tmp/motion-input
IN_DOOR=./tmp/door-input
IN_PRESENCE=./tmp/presence-input
IN_OUTLET=./tmp/outlet-input
IN_BULB=./tmp/bulb-input
IN_HEATER=./tmp/heater-input
IN_LIGHT=./tmp/light-input
IN_SECURITY=./tmp/security-input
OUT_DB=./tmp/database-output
OUT_TEMP=./tmp/temp-output
OUT_MOTION=./tmp/motion-output
OUT_DOOR=./tmp/door-output
OUT_PRESENCE=./tmp/presence-output
OUT_OUTLET=./tmp/outlet-output
OUT_BULB=./tmp/bulb-output

mkfifo $IN_TEMP $IN_MOTION $IN_DOOR $IN_PRESENCE $IN_OUTLET $IN_BULB $IN_HEATER $IN_LIGHT $IN_SECURITY
mkfifo $OUT_DB $OUT_TEMP $OUT_MOTION $OUT_DOOR $OUT_PRESENCE $OUT_OUTLET $OUT_BULB

# ------------------------------------------------------------------------------
echo "Starting gateway..."
$X gateway 0.0.0.0 $PORT > /dev/null &
PID_GATEWAY=$!
sleep 1

# ------------------------------------------------------------------------------
echo "Starting database..."
$X database 127.0.0.1 $PORT silent > $OUT_DB &
PID_DB=$!
ID_DB=`head -n 1 $OUT_DB`

# ------------------------------------------------------------------------------
echo "Starting devices..."

tail -f $IN_TEMP | $X temp 127.0.0.1 $PORT silent > $OUT_TEMP &
ID_TEMP=`head -n 1 $OUT_TEMP`

tail -f $IN_MOTION | $X motion 127.0.0.1 $PORT silent > $OUT_MOTION &
ID_MOTION=`head -n 1 $OUT_MOTION`

tail -f $IN_DOOR | $X door 127.0.0.1 $PORT silent > $OUT_DOOR &
ID_DOOR=`head -n 1 $OUT_DOOR`

tail -f $IN_PRESENCE | $X presence 127.0.0.1 $PORT silent > $OUT_PRESENCE &
ID_PRESENCE=`head -n 1 $OUT_PRESENCE`

tail -f $IN_OUTLET | $X outlet 127.0.0.1 $PORT silent > $OUT_OUTLET &
ID_OUTLET=`head -n 1 $OUT_OUTLET`

tail -f $IN_BULB | $X bulb 127.0.0.1 $PORT silent > $OUT_BULB &
ID_BULB=`head -n 1 $OUT_BULB`

# ------------------------------------------------------------------------------
echo "Starting controllers..."

tail -f $IN_HEATER | $X control heater 127.0.0.1 $PORT silent > /dev/null &
PID_HEATER=$!
echo $ID_TEMP > $IN_HEATER
echo $ID_OUTLET > $IN_HEATER

tail -f $IN_LIGHT | $X control light 127.0.0.1 $PORT silent > /dev/null &
PID_LIGHT=$!
echo $ID_MOTION > $IN_LIGHT
echo $ID_BULB > $IN_LIGHT

tail -f $IN_SECURITY | $X control security 127.0.0.1 $PORT silent > /dev/null &
PID_SECURITY=$!
echo $ID_DB > $IN_SECURITY
echo $ID_MOTION > $IN_SECURITY
echo $ID_DOOR > $IN_SECURITY
echo $ID_PRESENCE > $IN_SECURITY

# ------------------------------------------------------------------------------
echo "-------------------------"
echo "  MEMBER IDs"
echo "-------------------------"
echo "  Database:           $ID_DB"
echo "  Temperature sensor: $ID_TEMP"
echo "  Motion sensor:      $ID_MOTION"
echo "  Door sensor:        $ID_DOOR"
echo "  Presence sensor:    $ID_PRESENCE"
echo "  Smart outlet:       $ID_OUTLET"
echo "  Smart light bulb:   $ID_BULB"
echo "-------------------------"

$X control user 127.0.0.1 $PORT

# ------------------------------------------------------------------------------
echo "-------------------------"
echo "Killing all programs..."
echo "-------------------------"

kill $PID_LIGHT
kill $PID_HEATER
kill $PID_SECURITY
fuser -k $IN_LIGHT
fuser -k $IN_HEATER
fuser -k $IN_SECURITY

echo "exit" > $IN_BULB
echo "exit" > $IN_OUTLET
echo "exit" > $IN_PRESENCE
echo "exit" > $IN_DOOR
echo "exit" > $IN_MOTION
echo "exit" > $IN_TEMP
sleep 1
fuser -k $IN_BULB
fuser -k $IN_OUTLET
fuser -k $IN_PRESENCE
fuser -k $IN_DOOR
fuser -k $IN_MOTION
fuser -k $IN_TEMP
kill $PID_DB
kill $PID_GATEWAY
rm -rf ./tmp

