Our project is to build a web application using Haskell to provide an auto-correct function for misspelled English words and sentences. The user will be able to enter an arbitrary word/sentence in the text box at their discretion and our algorithm will examine if it is a valid, and provide reommendations. It will be built on Yesod. We use persistent as our data model and edit distance as our recommendation algorithm.

## Quick Start Guide
1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Install the `yesod` command line tool. 
```stack install yesod-bin --install-ghc```
3. Build libraries. 
```stack build```
4. To run this server, user should add a dotenv (.env) file in the root dictionary with local dictionary db path.
```
Dictionary_DB=<local path>/shu_haimeng_xingxing/config/Dictionary.db
```
5. Start a development server with:
```
stack exec -- yesod devel
```

### Useful commands
Executable named yesod not found on path

```
stack build yesod-bin
```

Add new handler

```
stack exec -- yesod add-handler
```

### Tests

```
stack test --flag autograd:library-only --flag autograd:dev
```

(Because `yesod devel` passes the `library-only` and `dev` flags, matching those flags means you don't need to recompile between tests and development, and it disables optimization to speed up your test compile times).
