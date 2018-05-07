all:
	ghc -o ./macros -hidir build/ -odir build/ -O2 src/macros.hs -isrc 
