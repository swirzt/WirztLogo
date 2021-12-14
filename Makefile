compile:
	stack build --ghc-options -threaded --ghc-options -O3

pedantic:
	stack build --pedantic

test:
	stack run -- test.logo