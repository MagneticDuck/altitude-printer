# altitude-printer

This utility facilitates the reading of [altitude](http://altitudegame.com) server logs by human administrators.

See also: [http://altitudegame.com/forums/showthread.php?t=7393](http://altitudegame.com/forums/showthread.php?t=7393) for a user-friendly C# app with client-side features.

## features

- Player join and quit notifications
- Chat with nicknames
- Formatted timestamps
- Map switches
- Explicit notification of time-gaps between meaningful events larger than 30 minutes.

## usage

You can either build it from source with the Haskell build system Cabal:

    $ cabal sanbox init
    $ cabal install
    $ cat log.txt | ./dist/dist-sandbox-b4f4b4f1/build/altitude-printer/altitude-printer > output

Or download the already built binary from the releases tab.
