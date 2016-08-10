# commonmark-haskell

The functionality is split across several libraries which are contained in this
repo:

- **commonmark-ast** -- definition of CommonMark's AST;
- **commonmark-parser** -- responsible for parsing into the AST;
- **commonmark-html** -- responsible for rendering of AST into html;
- **commonmark-testutils** -- utilities for testing these packages.

# License

This library is released under BSD-3-Clause license. See LICENSE for terms and copyright notice.

Custom parser combinators and CommonMark block structure parser are largely based on the ones found
in [Cheapskate](https://github.com/jgm/cheapskate) (Copyright © 2012, 2013, 2014 John MacFarlane).

