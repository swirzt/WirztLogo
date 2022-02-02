env:
	sudo apt install ghc haskell-stack freeglut3 freeglut3-dev libxrandr-dev libxinerama-dev libxcursor-dev libxxf86vm-dev libxi-dev libz-dev
	stack upgrade
	stack install happy

compile:
	stack build --ghc-options -threaded --ghc-options -O3

pedantic:
	stack build --pedantic

test:
	stack run -- test.logo