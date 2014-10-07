
= What is it?

Welcome to the Zomp project. Zomp is a small but powerful programming
language. It combines C like low level access to the machine with a Lisp style
macro system. Together this allows writing high performance applications and
gives library developers a lot of power.

It is heavy work-in-progress but already usable to write small
applications. Currently there is not a lot of documentation. You can browse the
testsuite and the examples to get an idea what you can do.

Zomp's website is at http://zompc.net.

= History, Goals

It was created because all other programming languages suck. It's as simple as
that. The requirement was to have a language that at the same time delivers the
same performance and low level control as C and allows more productive
development. This goal is approached by providing a minimal base language
extendable by macros and an interactive shell which supports modifying the code
of a running application for fast prototyping. It does not attempt to protect
against memory errors and other low level conditions.

TODO: C++ rant, dynamic languages, ocaml/haskell

= Getting started

At this time there is no proper process to get started. You will need to clone
the Zomp repository and provide a few required tools and libraries.

External dependencies (list might be incomplete and out of date):
- OS X 10.9 (older versions might work)
- OCaml 4.02. Install using opam and make sure ocamlfind is present
- Menhir parser generator, version 20070520
- LLVM and clang 2.9 (see source/build/tools.mk)

To build just call make all from the root directory. To compile zomp files use
"zompc -c foo.zomp", to use the zomp shell call zompsh. The accompanying
Emacs mode in zomp.el is recommended. The Zomp shell is designed to make it easy
to write integrations for other editors. Start it and enter !help to see what it
can do.

= How to contribute / licence

At this time this is a closed source project but you can send contributions and
feedback to Jan Rehders at cmdkeen@gmx.de

