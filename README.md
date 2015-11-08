# altitude-printer

This is a small, hastily written utility I wrote to facilitate the reading of altitude server logs by human administrators. Build it with cabal, the haskell build system:

    cabal install

And run the created 'altitude-printer' executible, passing an entire raw server log into its stdin; it will print a human-readable version to its stdout, with all kinds of helpful features and information. You can find examples in the /tests/ directory.
