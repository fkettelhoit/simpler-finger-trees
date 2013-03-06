# Simpler Finger Trees in Clojure

Finger Trees are awesome. But they could be simpler and even more awesome and versatile.

## Finger Trees in Clojure right now

The [current implementation](https://github.com/clojure/data.finger-tree) of finger trees in Clojure is a faithful re-implementation of the [original paper](http://www.soi.city.ac.uk/~ross/papers/FingerTree.html) by Paterson and Hinze. But is the structure of finger trees described there really the optimum?

## Simpler Finger Trees

This repository is a work-in-progress effort to develop a data structure that follows the basic idea behind finger trees and obeys the same Big-O properties for all operations, but is *simpler* and *more general*:

* Use fewer cases (Seed/Tree instead of Empty/Single/Deep) to allow easier definition in OO languages *(done)*
* Decouple Finger Trees from 2-3 Nodes and allow arbitrary container structures *(partially done)*
* Allow "Monoid-mixing" to create new Finger Trees from pre-defined blocks

## Usage

This is only a research project right now. Do not use it. (It's a miracle it even compiles...)

## License

Copyright © 2013 Frederic Kettelhoit

Distributed under the Eclipse Public License, the same as Clojure.
