compile:
	stack build --ghc-options -threaded

pedantic:
	stack build --pedantic

test:
	stack run -- test.logo