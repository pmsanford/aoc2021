test:
	cabal run --verbose=0 Day9 -- test.input

solve:
	cabal run --verbose=0 Day9 -- main.input

build: app/Main.hs
	cabal build Day9

snap:
	cabal run --verbose=0 Day9 -- test.input > Day9.snap
	cabal run --verbose=0 Day9 -- main.input >> Day9.snap

check:
	cabal run --verbose=0 Day9 -- test.input > Day9.out
	cabal run --verbose=0 Day9 -- main.input >> Day9.out
	diff Day9.snap Day9.out && rm Day9.out

clean:
	cabal clean

