test:
	cabal run --verbose=0 Day4 -- test.input

solve:
	cabal run --verbose=0 Day4 -- main.input

build: app/Main.hs
	cabal build Day4

snap:
	cabal run --verbose=0 Day4 -- test.input > Day4.snap
	cabal run --verbose=0 Day4 -- main.input >> Day4.snap

check:
	cabal run --verbose=0 Day4 -- test.input > Day4.out
	cabal run --verbose=0 Day4 -- main.input >> Day4.out
	diff Day4.snap Day4.out && rm Day4.out

clean:
	cabal clean

