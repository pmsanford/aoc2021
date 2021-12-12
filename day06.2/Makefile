test:
	cabal run --verbose=0 Day6 -- test.input

solve:
	cabal run --verbose=0 Day6 -- main.input

build: app/Main.hs
	cabal build Day6

snap:
	cabal run --verbose=0 Day6 -- test.input > Day6.snap
	cabal run --verbose=0 Day6 -- main.input >> Day6.snap

check:
	cabal run --verbose=0 Day6 -- test.input > Day6.out
	cabal run --verbose=0 Day6 -- main.input >> Day6.out
	diff Day6.snap Day6.out && rm Day6.out

clean:
	cabal clean

