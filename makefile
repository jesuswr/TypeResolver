all: typeResolver

typeResolver:
	ghc Main.hs -o TypeResolver

clean:
	rm TypeResolver *.o *.hi 