# Commands:

.PHONY: build init test clean doc deploy stage

build:
	ghc --make -O -o morris Main.hs

prof:
	ghc --make -prof -o morris Main.hs

all: build test

# Cleaning commands:
clean:
	rm -f morris
	rm -f *.hi
	rm -f *.o

setup:
	cabal install ansi-terminal