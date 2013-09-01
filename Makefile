.PHONY: build clean config install-deps test

default: test

install-deps:
	cabal install --enable-tests --only-dependencies

config: 
	cabal configure --enable-tests

build: config
	cabal build

test: build
	cabal test

clean:
	cabal clean
