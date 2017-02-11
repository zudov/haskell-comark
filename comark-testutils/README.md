# commonmark-testutils

When this package is compiled it reads file `spec.json` parses and provides the content
as a compiled-in value. To generate `spec.json`:

1. Fetch "spec" submodule:

   ```shell
   ~/haskell-commonmark $ git submodule update --init
   Cloning into '~/haskell-commonmark/spec'...
   Submodule path 'spec': checked out 'd64dc449407cbdde777096e82f6ca65a39b21bfa'
   ```
2. Dump the tests to the `spec.json`:

   ```shell
   ~/haskell-commonmark $ python3 spec/test/spec_tests.py -s spec/spec.txt --dump-tests > commonmark-testutils/spec.json
   ```


