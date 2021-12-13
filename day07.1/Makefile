test:
	cabal run --verbose=0 Day7 -- test.input

solve:
	cabal run --verbose=0 Day7 -- main.input

build: app/Main.hs
	cabal build Day7

snap:
	cabal run --verbose=0 Day7 -- test.input > Day7.snap
	cabal run --verbose=0 Day7 -- main.input >> Day7.snap

check:
	cabal run --verbose=0 Day7 -- test.input > Day7.out
	cabal run --verbose=0 Day7 -- main.input >> Day7.out
	diff Day7.snap Day7.out && rm Day7.out

clean:
	cabal clean

