# SERVER = ubuntu@new.eseidel.org

# deploy: build
#	strip dist/build/blog/blog
#	tar czvf blog.tgz blog.cabal dist/build/blog/blog resources src
#	scp blog.tgz $(SERVER):~/
#	ssh $(SERVER) sudo stop blog || true
#	ssh $(SERVER) mkdir -p blog
#	ssh $(SERVER) tar xzvf blog.tgz -C blog --overwrite
#	ssh $(SERVER) sudo cp blog/resources/nginx/blog /etc/nginx/sites-enabled/
#	ssh $(SERVER) sudo cp blog/resources/upstart/blog.conf /etc/init/
#	ssh $(SERVER) sudo start blog

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
	cabal clean
	cabal configure
	cabal build

devel:
	cabal clean
	cabal configure -fdevelopment
	cabal build
	./dist/build/blog/blog -p 9000
