This repository contains various samples, experiments and notes relating to Haskell.

# Environment Setup Notes

```bash
# Install base stack package with `apt`
sudo apt install haskell-stack

# On OS X, install Homebrew, then:
brew install stack

# Install exuberant ctags and hasktags for tagbar
brew install ctags
stack install hasktags

# Installing `fzf`
brew install fzf
/usr/local/opt/fzf/install

# Install `hspec-discover` for discovering spec files
cd haskell-sandbox
stack install hspec-discover
```


# Setting up a new `stack` project

## Step 1 - Basic Project

- Create new empty project based on `stack` `new-template`:
    ```bash
    stack new haskell-project
    cd haskell-project
    ```

- Check that everything builds and tests:
    ```bash
    # If necessary, this might install a new GHC
    stack build
    stack test
    ```

- Create new repository on Github and use default `.gitignore` file.


## Step 2 - Testing with `hspec-discover`

- Add function definition to `src/Lib.hs`:
    ```haskell
    add :: Int -> Int -> Int
    add x y = x + y
    ```

- Make sure the existing module declaration exports this new function:
    ```haskell
    module Lib
        ( someFunc,
          add           -- Add this line
        ) where
    ```

- Add new file `test/LibSpec.hs` with contents:
    ```haskell
    module LibSpec where

    import Test.Hspec
    import Lib

    testAdd :: SpecWith ()
    testAdd = describe "Test addition" $
        it "Adds two numbers" $
            add 1 1 `shouldBe` 2

    spec :: Spec
    spec = testAdd
    ```

- Add `hspec` as dependency of test target in `package.yaml`

- Replace contents of `test/Spec.hs` with:
    ```haskell
    {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
    ```

- Retest to confirm that new test now runs and passes:
    ```bash
    stack test
    ```

- Change source and test definitions to make sure failures are shown when
  re-running `stack test`


## Step 3 - Setting up `ghcid`

- Install `ghcid` in global project:
    ```bash
    cd
    stack install ghcid
    ```

- Create `.ghcid` file in top level of project, with following content:
    ```
    --command "stack ghci haskell-project:test:haskell-project-test haskell-project:lib"
    --test "Main.main"
    ```

- Run `stack test` and ensure manual run of test still works as expected

- Run `ghcid` and check for passed test case

- Change source and test definitions to make sure failures are shown without
  manual reloading


## Step 4 - Setting up `hlint`

- Install `hlint` in global project:
    ```bash
    cd
    stack install hlint
    ```

- Add `--lint` switch to end of `.ghcid` file

- Check Hlint integration by adding definition to `Lib.hs` and checking that
  `ghcid` outputs the "Use newtype instead of data` suggestion:
    ```haskell
    data Identity a = Identity a
    ```

- Try disabling specific `hlint` hint:
    - Add `-Wno-unreognised-pragmas` GHC option to `package.yaml`:
        ```yaml
        library:
          source-dirs: src
          ghc-options:
            - -Wno-unreognised-pragmas
        ```

    - Add pragma before `Identity` declaration:
        ```haskell
        {-# HLINT ignore Identity "Use newtype instead of data" #-}
        data Identity a = Identity a
        ```


## Step 5 - Setting up `hoogle`

- *TODO*


# IDE support

## [`haskell-language-server`](https://github.com/haskell/haskell-language-server)

- Install latest stable version of language server:
    - Download binaries from Github
    - Extract binaries and move to `~/.local/bin`
    - Remove `macOS` from extracted binary filenames
    - `chmod a+x` on extracted binaries

- Alternatively, build from source (workaround for
  https://github.com/haskell/vscode-haskell/issues/323):

    ```
    git clone https://github.com/haskell/haskell-language-server.git --recurse-submodules
    cd haskell-language-server
    # Switch to tag for required stable version here

    stack ./install.hs help              # To force GHC installation and see options
    stack ./install.hs hls-x.y.z         # Update based on GHC version required
    stack ./install.hs data              # To generate Hoogle DB
    ```

- Run `haskell-language-server-wrapper` to check for no errors on startup:
    ```bash
    # We need to do this first, to avoid errors
    rm Setup.hs
    haskell-language-server-wrapper
    ...output ...
    ```

- Open `src/Lib.hs` in `vim` and check CoC works:
    - Run `:CocConfig` and uncomment line to enable HLS language server
    - Check 'K' gives docs on hover over `putStrLn`
    - Delete `putStrLn` and check autocomplete works and produces type
      signatures and documentation in autocomplete options
    - Create type errors and check they are reported


## IntelliJ

- Open project

- In _Import Project_ dialogue, select _Import project from external model ->
  Haskell Stack_

- In _Project SDK_, select _Haskell Tool Stack 2.3.1_

- Project opens and output starts appearing in _Event Log_ tool window

- If no output appears in _Event Log_ tool window:
    - Click configuration wrench to left of _Event Log_ tool window
    - Scroll down to _Haskell Log_ entry and ensure _Log_ column is checked

- Wait for following output to appear in _Event Log_ tool window:
    ```
    [global-stack-repl] Stack REPL is started
    ```

- Test common features are working:
    - Open `src/Lib.hs` and test hover produces documentation for `Int` and
      `putStrLn`
    - Remove `putStrLn` and test autocompletion shows function names and
      signatures (note that documentation doesn't seem to show)
    - Test navigating from `putStrLn` into library code
    - Test removal of `{-# HLINT ... #-}` pragma causes hlint suggestion to
      appear


# Common Commands

## Running tests with `stack`
```
stack test --fast --file-watch
```

## Running tests with `ghcid`
Firstly, install `ghcid`:
```
stack install ghcid
```

To run:
```
ghcid --command "stack ghci haskell-sandbox:lib haskell-sandbox:test:haskell-sandbox-test" \
      --test "Main.main"
      --lint
```

or, add the following to [`.ghcid`](.ghcid), to set default options and allow running with just `ghcid`:
```
--command "stack ghci haskell-sandbox:lib haskell-sandbox:test:haskell-sandbox-test" \
--test "Main.main"
--lint
```


## Build `haddock` docs for dependencies
Need to do this for projects so that we get documentation for external dependencies in VS Code / `haskell-ide-engine`:
```
cd haskell-sandbox
stack haddock --keep-going
```


## Updating `haddock` docs for local code
Need to do this for VS Code / `haskell-ide-engine` to start showing up updated documentation on hover:
```
cd haskell-sandbox
stack haddock --haddock-internal --file-watch
```
Note: `--file-watch` allows docs to be regenerated when the source changes.



# IDE support

*TODO*: Needs more work to create a table with common IDE functions and which
ones I've got working on which IDEs, including:

- `haskell-language-server` + nvim / coc
- `haskell-language-server` + VSCode
- IntelliJ-Haskell

Features to include:

- Hover docs
    - Internal declarations in same module
    - Internal declarations in different module
    - External declarations in `base`
    - External declarations in other 3rd party dependencies

- Autocompletion
    - List of functions / types etc
    - Type signatures
    - Documentation (subcategories as above)

- Go to definition
    - Within module
    - Across modules
    - `app` -> `lib`
    - `test` -> `lib`
    - To dependencies in `base`
    - To dependencies in 3rd party libraries

- HLint
    - Showing warnings
    - Disabling using HLINT pragmas
    - Code action to automatically correct

- Show type
    - Single identifier
    - Selection

- Code actions
    - Add import
    - Correct typo