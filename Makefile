devel:
	cabal-dev clean
	cabal-dev install -fdevelopment
	./dist/build/blog/blog
