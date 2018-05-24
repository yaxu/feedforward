Feedforward, an experimental editor for TidalCycles
(c) Alex McLean 2018
Released under the terms of the GNU Public Licence version 3

https://github.com/yaxu/feedforward

## Installation

You'll need the ncurses library installed first. For example under
debian linux, you can do this with `apt-get install
libncurses5-dev`. Please let me (alex@slab.org) know if you get it
installed under other systems (particularly windows/mac) and how. (It
probably will be particularly difficult to install under windows)

Install (into ~/.cabal/bin) with:

```
git clone http://github.com/yaxu/feedforward
cd feedforward
cabal install
```

## VU meters

To get in-text VU meters on patterns, switch on RMS sending from SuperDirt.

Before starting SuperDirt, do this: `s.options.maxLogins = 8;`

After starting SuperDirt (e.g. with `SuperDirt.start`), do this:
`~dirt.startSendRMS;`.

Do all this *before* starting feedforward.

The VU meters will also work if you use the latest develop version of
superdirt.

## Usage

At the moment it sends a lot of info to stderr, which you'll need to
direct somewhere, e.g. by running like this:

`feedforward 2> err.txt`

If .cabal/bin isn't in your path, you could run it with:

`~/.cabal/bin/feedforward 2> err.txt`

At the time of writing..

* Some emacs keys work for navigation. (^n, ^p, ^a, ^e, ^b, ^f, ..)
* Alt-Enter to evaluate all the code in the buffer
* F2 replays a previous session (experimental)
* F10 quits
* ctrl-h hushes everything
* ctrl-l redraws the screen (in case something gets corrupted)
* Different patterns automatically get sent to different orbits
* There is no cut and paste yet
* Mouse clicks position the cursor on some terminals, but it's a bit
  clunky

By default, feedforward will work with SuperDirt, running on the same
computer. To switch between Classic dirt and SuperDirt, edit
Feedback/Edit.hs and look for `dirt = `... Set it to either `dirt =
Classic` or `dirt = Super`.

The stuff in `Drum/` is a server for laptop ensembles, for centralised
recording of keystrokes and for taking coordinated snapshots. Probably
not interesting to you just yet.

## Contributions

This is very early stage software, but I'm nonetheless happy to hear
bug reports:

https://github.com/yaxu/feedforward/issues
