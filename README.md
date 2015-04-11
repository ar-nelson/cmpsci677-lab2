% CMPSCI 677 Lab 2: Internet of Things - Smarter Home Edition
% Adam R. Nelson
% April 10, 2015

Project Description
===================

Changes from Lab 1
------------------

Due to time constraints and the significantly-increased complexity of this
project, Lab 2 was *not* written as a literate program. The Haskell source
files can be found under `src/`.

Although the basic organization and functionality remains the same as Lab
1, there are some notable changes:

* All members of the cluster (not just devices) now have IDs.

* There are four new subcommands: `database`, `door`, `presence`, and `control
  security`.

* Instead of the gateway routing messages by random message IDs, it now
  attaches a `Conversation` to each `Request` and `Response`, which specifies
  the requester and responder in each request/response exchange.

If you have not read Lab 1's design document, you should read it before reading
this document or the source code. This document assumes that the reader is
familiar with my work from Lab 1.

The Time Server
---------------

The time server is defined in `TimeServer.hs`. It spawns a new thread that
intercepts messages from the current subprogram's `recv` channel, and forwards
any messages that it doesn't use. Every subprogram runs a time server in the
background.

Clock time is synchronized between all cluster members. A leader is elected
using the bully algorithm; because each new cluster member will automatically
have the highest ID, every new cluster member will immediately initiate an
election that it automatically wins.

Logical time is maintained using Lamport clocks. Writing the project in Haskell
allowed me to thread Lamport clock ordering throughout the entire program, in
the form of the `Timed` monad. `Timed` is a monad transformer on top of `IO`,
which can create and read `Timestamped Message`s. It maintains a Lamport clock
which:

1. Increments itself every time a `Timestamped Message` is created, and
2. Is always greater than the Lamport clock of the most-recently-read
   `Timestamped Message`.

As long as all message sending is performed within `Timed`, the type system
guarantees causal ordering of Lamport clock values.

The Database
------------

The database is a new type of subprogram. Whenever a database connects to the
gateway, the gateway begins sending it timestamped `DBInsert` messages for each
of the following events:

* Members joining the cluster
* Members leaving the cluster
* Broadcast messages (push notifications)

The database stored each received entry in the file `db.txt`, and, for each
cluster member, keeps the most recent entry of each type in memory.

The database can be queried with `DBQuery` messages, which contain a list of
(ID, event) pairs. The database responds to a `DBQuery` with a `DBResultSet`,
which contains a list of `Timestamps` (containing both clock and Lamport time),
one for each (ID, event) pair, representing the most recent entry for that
pair.

For example, a `DBQuery [(ID 1, BroadcastEvent (ReportState (DoorOpen True)))]`
message would cause the database to return a result set with a single entry:
the timestamp of the most recent `DoorOpen True` state push notification from
the device with ID 1.

When the database starts, it reads an existing `db.txt` (if present) and
replays all of the events it contains. This also sets the database's Lamport
clock to the highest value in `db.txt`, which will propogate to the gateway and
result in subsequent database entries having correct Lamport clock values.

Usage
=====

Compiling
---------

Provided that the [Haskell platform][hs] is installed, the project can be
compiled by running `make`. This should install all necessary `cabal` packages,
compile the Haskell program, and copy it to the current directory with the name
`smarthome`.

GHC >= 4.5 is required. If you recently installed the Haskell platform, you may
need to update your package database with `cabal update` before running `make`.

[hs]: https://www.haskell.org/platform/ 

Executing
---------

The executable requires at least 3 arguments: the _subprogram name_, the
_hostname_, and the _port_. For everything except the gateway, the hostname and
port are the gateway's hostname and port. The gateway ignores the hostname.

Here are some examples of valid ways to execute the program, demonstrating most
of the subprogram names:

```bash
    ./smarthome gateway 0.0.0.0 9100
    ./smarthome database 0.0.0.0 9100
    ./smarthome door 127.0.0.1 9100
    ./smarthome motion 127.0.0.1 9100
    ./smarthome bulb 127.0.0.1 9100
    ./smarthome outlet 127.0.0.1 9100
    ./smarthome control security 127.0.0.1 9100
    ./smarthome control light 127.0.0.1 9100
    ./smarthome control user 127.0.0.1 9100
```

`silent` can be added as an extra argument to all of these to limit console
output; this is used by the test scripts.

The test scripts `run-all.sh` and `run-test-case.sh` can be used to run the
program, but _only after it has been compiled with `make`_.

Test Cases
----------

`run-test-case.sh` is probably what you're interested in. It runs a test of the
Security Controller (detecting when a user comes home/leaves home, and
detecting intruders). The test output is printed to `test-output.txt`. My
output from a previous run is available in `test-output-cached.txt`, and the
database file from this run (showing time server activity) is `db.cached.txt`.

