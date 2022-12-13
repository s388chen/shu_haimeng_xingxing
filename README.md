# Autocorrection
## Overview
Our project is to build a web application using Haskell to provide an auto-correct function for misspelled English words and sentences. Users are able to enter an arbitrary word/sentence in the text box at their discretion and our algorithm will examine if it is a valid, and provide reommendations. The whole structure is built on Yesod, which is a lightweight Haskell-based web framework. We also use persistent as our data model and edit distance as our recommendation algorithm.

## Team
* Haimeng Wang (pennkey: whm7)
* Shu Chen (pennkey: s388chen)
* Yuxin Kan (pennkey: irenekxx)

## Components
1. A Widget's Html, CSS and Javascript are separated in three files with the .hamlet, .lucius and .julius extensions. All the files for templates and widgets are in `templates` folder.
2. In Yesod framework, routing and handlers make up the controller. Routing is abstracted in `config/routes.yesodroutes` and all of the route handlers are in `src/Hander`.
3. We define database entities in the entities file `config/models.persistentmodels`. We also enable connecting to an additional existing sqlite database by creating a standalone module `src/DitionaryDB.hs`.
4. We have integration tests, quicktests and unit tests in `test/`.

## Quick Start Guide
1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually 
		```
		curl -sSL https://get.haskellstack.org/ | sh
		```
2. Install the `yesod` command line tool. 
	```
	stack install yesod-bin --install-ghc
	```
3. Build libraries. 
	```
	stack build
	```
4. To keep sensitive data out of code, we add extra package `dotenv`. To run this server, user should add a dotenv (.env) file in the root dictionary with local dictionary db path.
	```
	Dictionary_DB=<local path>/shu_haimeng_xingxing/config/Dictionary.db
	```
5. Start a development server with:
	```
	stack exec -- yesod devel
	```

### Useful commands
Executable named yesod not found on path.

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
