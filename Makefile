SERVER = ubuntu@new.eseidel.org

deploy: build
	strip dist/build/blog/blog
	tar czvf blog.tgz blog.cabal dist/build/blog/blog resources src
	scp blog.tgz $(SERVER):~/
	ssh $(SERVER) sudo stop blog || true
	ssh $(SERVER) mkdir -p blog
	ssh $(SERVER) tar xzvf blog.tgz -C blog --overwrite
	ssh $(SERVER) sudo cp blog/resources/nginx/blog /etc/nginx/sites-enabled/
	ssh $(SERVER) sudo cp blog/resources/upstart/blog.conf /etc/init/
	ssh $(SERVER) sudo start blog

build:
	cabal-dev clean
	cabal-dev install 

devel:
	cabal-dev clean
	cabal-dev install -fdevelopment
	./dist/build/blog/blog -p 9000
