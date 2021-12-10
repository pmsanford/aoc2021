test:
	cabal run --verbose=0 Day3 -- test.input

solve:
	cabal run --verbose=0 Day3 -- main.input

build: app/Main.hs
	cabal build Day3

snap:
	cabal run --verbose=0 Day3 -- test.input > Day3.snap
	cabal run --verbose=0 Day3 -- main.input >> Day3.snap

check:
	cabal run --verbose=0 Day3 -- test.input > Day3.out
	cabal run --verbose=0 Day3 -- main.input >> Day3.out
	diff Day3.snap Day3.out
	rm Day3.out

clean:
	cabal clean

