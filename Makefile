deploy:
	rm -f dist/build/blog/blog
	git checkout deploy
	git merge master
	make build
	git add -f dist/build/blog/blog
	git commit -m "Deploy: `date`"
	git push heroku deploy:master
	git checkout master

build:
	cabal-dev clean
	cabal-dev install

devel:
	cabal-dev clean
	cabal-dev install -fdevelopment
	./dist/build/blog/blog
