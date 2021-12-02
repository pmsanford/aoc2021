test:
	runghc Day2.hs test.input

solve:
	runghc Day2.hs main.input

build: Day2.hs
	ghc -outputdir ./build Day2

clean:
	rm ./build/*.hi ./build/*.o Day2
	rmdir build

