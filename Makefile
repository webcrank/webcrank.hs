.PHONY: config build test install-deps

default: test

install-deps:
	cabal install --enable-tests --only-dependencies

config: 
	cabal configure --enable-tests

build: config
	cabal build

test: build
	cabal test

