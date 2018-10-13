## hs-src-tool

This tool downloads sources of Stack project's dependencies to a `.hs-src-tool` folder and uses https://github.com/alexwl/haskell-code-explorer to index and browse them.

### How to use it

1. Make sure that `stack`, `cabal`, `haskell-code-index`, `haskell-code-serve`, and of course `hs-src-tool` are on your `$PATH`
1. The tool is designed to be run from the root of Stack project:
   ```shell
   $ cd $PROJECT_ROOT
   ```
1. Make sure that your project **uses the same version of GHC** as `haskell-code-index` and `haskell-code-serve`!
1. Build your project first:
   ```shell
   $ stack build
   ```
1. Now fetch the sources:
   ```shell
   $ hs-src-tool fetch
   ```
1. Sources shold be in `$PROJECT_ROOT/.hs-src-tool` folder
1. Index your project and its dependencies
   ``` shell
   $ hs-src-tool index
   ```
1. Start the sources explorer
   ```shell
   $ hs-src-tool explore --port=8080 --ui=$PATH_TO_HASKELL_CODE_EXPLORER_REPO/javascript/release
   ```
1. Open http://localhost:8080 and enjoy!

Note that `hs-src-tool` tries its best to build and index as many packages as it can. Meaning that failing packages are skipped. If you don't see some specific package in the explorer - take a look on `hs-src-tool index` output.
