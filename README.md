## hs-src-tool

This tool downloads sources of Stack project's dependencies to a `.hs-src-tool` folder.
After sources are downloaded you can use something like https://github.com/alexwl/haskell-code-explorer to index and browse them.

### How to use it

1. The tool is designed to be run from the root of Stack project:
   ```shell
   $ cd $PROJECT_ROOT
   ```
1. Build your project first:
   ```shell
   $ stack build
   ```
1. Now fetch the sources:
   ```shell
   $ hs-src-tool
   ```
1. Sources shold be in `$PROJECT_ROOT/.hs-src-tool` folder now.
