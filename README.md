clunk
=====

A clunk interpreter. 

A more detailed explanation of the language can be found in [here](./language.md). 

Road map
--------
- [x] lex
- [x] parse
- [ ] interpret
  - [X] basic expressions
  - [X] bound names
  - [ ] functions, scopes and closures
  - [X] pattern matching
  - [ ] complex data types (tuples and lists)

Build
-----

    $ rebar3 escriptize

Run
---

    $ _build/default/bin/clunk
