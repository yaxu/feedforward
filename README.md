Feedforward, an experimental editor for TidalCycles
(c) Alex McLean 2018
Released under the terms of the GNU Public Licence version 3

Install (into ~/.cabal/bin) with `cabal install`

At the time of writing..

* Some emacs keys work for navigation.
* F2 replays a previous session (experimental)
* F10 quits

To switch between Classic and SuperDirt, edit Feedback/Edit.hs and look for `dirt = `... Set it to either `dirt = Classic` or `dirt = Super`.

The stuff in `Drum/` is a server for central recording of keystrokes
and for taking coordinated snapshots.
