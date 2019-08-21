# README
--------

Here, you will find the main library.
You are probably looking for the main readme [here](https://github.com/turion/essence-of-live-coding).

--------

essence-of-live-coding is a general purpose and type safe live coding framework in Haskell.
You can run programs in it, and edit, recompile and reload them _while_ they're running.
Internally, the state of the live program is automatically migrated when performing hot code swap.

The library also offers an easy to use FRP interface.
It is parametrized by its side effects,
separates data flow cleanly from control flow,
and allows to develop live programs from reusable, modular components.
There are also useful utilities for debugging and quickchecking.

