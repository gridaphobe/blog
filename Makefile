deploy:
	git push --force heroku master
	notify-send 'Heroku' 'Finished deploying'

devel:
	cabal-dev install -fdevelopment
	./dist/build/blog/blog

clean:
	cabal-dev clean
