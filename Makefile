.PHONY: test build

default: test

test:
	cabal configure --enable-tests && cabal build && cabal test

build:
	cabal configure --enable-tests && cabal build && cabal test
