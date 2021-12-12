test:
	cabal run --verbose=0 Day5 -- test.input

solve:
	cabal run --verbose=0 Day5 -- main.input

build: app/Main.hs
	cabal build Day5

snap:
	cabal run --verbose=0 Day5 -- test.input > Day5.snap
	cabal run --verbose=0 Day5 -- main.input >> Day5.snap

check:
	cabal run --verbose=0 Day5 -- test.input > Day5.out
	cabal run --verbose=0 Day5 -- main.input >> Day5.out
	diff Day5.snap Day5.out && rm Day5.out

clean:
	cabal clean

