# v2.0
- Major code rewrite.
- Add command-line flags:
  - `-n`: is an alias for `--next`
  - `-nn`: is an alias for `--next-only`
  - `--next`: Include information about the next queued song in the
    output JSON.
  - `--next-only`: Print only the next queued song's information,
    replacing the `tags` object.

# v1.5
- Add json keys
  - `volume`: Integer for volume percentage
  - `crossfade`: Integer seconds of crossfase
  - `mixramp_db`: Decibels for MixRamp, can use float (decimals) number
  - `mixramp_delay`: Seconds of delay for MixRamp, can use float number
  - `updating_db`: Returns `true` when updading, not present otherwise
- More code refactoring, prepping for v2 for more abstractions :p

# v1.4.0
- Add "`next_filename`" for getting next song file URI relative to the
  music library.

# v1.3.2
- Add "`next_position`", "`id`" and "`next_id`" keys to `playlist`.

# v1.3.1
- Move helper function `objectJson` to lib

# v1.3
- Add `filename` key.
- Add `playlist` key and move existing keys to it.
- Customize ordering of displayed output JSON.
- Add cabal tested-with GHC versions

# v1.2.0.0
- Move literate Org Mode code to LITERATE.org file
- Move functions from executable source Main.hs to their own library
- Bump dependency versions for `aeson` and `bytestring`
- Changed status.state from "play" to "playing" and "pause" to
  "paused".
  The reason why it was "play" and "pause" before was because
  that was the socket answer string.

# v1.1.0.2
[comment]: # (2023-10-23)
- Fixed cabal `build-depends` version bounds for Arch Linux dynamic
  building.

# v1.1.0.1
[comment]: # (2023-10-17)
- Added haddock comments
- Addressed `cabal check` warnings;
- setup for uploading as a Hackage package.

# v1.1.0.0
[comment]: # (2023-06-11)
- Remove `-h` from `--help` and use `-h` for `--host`
- Make `--help` option hidden in the help message

# v1.0.0.0
[comment]: # (2023-06-08)
Initial working version
- Added conditional tags printing, only non-empty values are printed
- Accept host, port and password
- Nested json objects for `status` and `tags`
- Added `elapsed_percent` key shortcut for `elapsed / duration * 100`

# v0.0.1.0
[comment]: # (2023-06-01)
- initial connection and parsing values
- First version. Released on an unsuspecting world.
