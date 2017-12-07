# Settlers of Clarktan

## Overview

Settlers of Clarktan is an OCaml implementation of the popular German board game Settlers of Catan. Players assume the roles of settlers, each attempting to build and develop holdings while trading and acquiring resources. Players are awarded points as their settlements grow; the first to reach 10 victory points is the winner.

In addition to building roads, settlements and cities using your resources, you can also buy and play development cards as well as trade wisely with the other players and the bank. The game is single player, and you compete against 3 other AI players.

## Installation

As this is the project for CS 3110 at Cornell University, we will assume that you already have ocaml and opam installed. If not, please follow the [instructions here] (http://www.cs.cornell.edu/courses/cs3110/2016fa/install.html).

Additionally, you will need to install X11/XQuartz for Graphics support. If you installed ocaml with homebrew, it can be done by running
<dl>
  <dd> brew install Caskroom/cask/xquartz </dd>
  <dd> brew reinstall ocaml --with-x11 </dd>
</dl>

Then, map opam to use the system installation instead of the currently bound one: opam switch sys. Then run eval `opam config env` as instructed.

You might also need to install the Graphics module and the CamlImages. It can be done by running
<dl>
  <dd> opam install graphics </dd>
  <dd> opam install camlimages </dd>
</dl>

## Gameplay

After successfully installing the external libraries, you will be able to run
<dl>
  <dd> make check to compile our code </dd>
</dl>
and
<dl>
  <dd> make play to have fun with our game! </dd>
</dl>
Note that if running natively on macOS, it might take a few seconds to power up XQuartz. The game can also be manually launched by running ./main.byte.

Please make sure that both the Terminal and the main game Window is visible on your screen to see instructions and updates as you play.

Additional details about the gameplay can be found in the design document.

If you have any trouble during installation or running of our game, the development team will gladly assist you in resolving them. Please send an email to ys393@cornell.edu, ejj35@cornell.edu, yz923@cornell.edu and we will get back to you ASAP.
