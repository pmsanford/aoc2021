test:
	cabal run --verbose=0 Day8 -- test.input

solve:
	cabal run --verbose=0 Day8 -- main.input

build: app/Main.hs
	cabal build Day8

snap:
	cabal run --verbose=0 Day8 -- test.input > Day8.snap
	cabal run --verbose=0 Day8 -- main.input >> Day8.snap

check:
	cabal run --verbose=0 Day8 -- test.input > Day8.out
	cabal run --verbose=0 Day8 -- main.input >> Day8.out
	diff Day8.snap Day8.out && rm Day8.out

clean:
	cabal clean

