setup: 
	ghc -package Cabal Setup.lhs -o setup.exe

configure: 
	./setup.exe configure --libdir=f:/work/softs/ghc682/extlib --bindir=f:/work/softs/ghc682/extbin

build:
	./setup.exe build
	
install:
	./setup.exe install
	
all: build install


