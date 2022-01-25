env:
	sudo apt install git haskell-stack freeglut3 freeglut3-dev libxrandr-dev libxinerama-dev libxcursor-dev libxxf86vm-dev libxi-dev libz-dev
	
compile:
	stack build --ghc-options -threaded --ghc-options -O3

pedantic:
	stack build --pedantic

test:
	stack run -- test.logo