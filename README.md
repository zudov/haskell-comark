# haskell-commonmark

[![Build Status](https://travis-ci.org/zudov/haskell-commonmark.svg?branch=master)](https://travis-ci.org/zudov/haskell-commonmark)

[![commonmark on Hackage](https://img.shields.io/hackage/v/commonmark.svg)](http://hackage.haskell.org/package/commonmark)

[![commonmark on Stackage LTS 2](http://stackage.org/package/commonmark/badge/lts-2)](http://stackage.org/lts-2/package/commonmark)
[![commonmark on Stackage LTS 3](http://stackage.org/package/commonmark/badge/lts-3)](http://stackage.org/lts-3/package/commonmark)
[![commonmark on Stackage Nightly](http://stackage.org/package/commonmark/badge/nightly)](http://stackage.org/nightly/package/commonmark)

The functionality is split across several libraries which are contained in this
repo:

- **commonmark-ast** -- definition of CommonMark's AST;
- **commonmark-parser** -- responsible for parsing into the AST;
- **commonmark-html** -- responsible for rendering of AST into html;
- **commonmark-testutils** -- utilities for testing these packages.

# TODO

- [X] Working parser implementation, pass all tests
- [X] Fast and accurate renderer, renders things exactly as in spec.txt.
- [X] Make full use of spec examples.
      Both renderer and parser are separately tested against the spec relying only on it
	  and the reference implementation (libcmark).
- [X] Handle pathological inputs. Benchmarks are run against `markdown-it`'s samples
      and over notoriously known nested parenthesis/brackets (`"[" * 50000 + "foo" + "]" * 50000`)
- [ ] Revise module structure, names, and (re-)exports.
- [ ] Add to hackage and stackage.
- [ ] Document things up.
- [ ] Extend ecosystem with additional libraries that ease the integration
      (commonmark-blaze, commonmark-lucid, commonmark-json)
- [ ] Add helpers for processing/walking the AST and examples of typical manipulations.
- [ ] Work on extensibility.
- [ ] Compile with ghcjs. If performance/size isn't great, consider making bindings to
      `commonmark.js` but provide `commonmark-ast` based interface.

# License

This library is released under BSD-3-Clause license. See LICENSE for terms and copyright notice.

Custom parser combinators and CommonMark block structure parser are largely based on the ones found
in [Cheapskate](https://github.com/jgm/cheapskate) (Copyright © 2012, 2013, 2014 John MacFarlane).

