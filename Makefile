SERVER = ubuntu@ec2-107-22-148-48.compute-1.amazonaws.com

deploy: build
	strip dist/build/blog/blog
	tar czvf blog.tgz blog.cabal dist/build/blog/blog resources src
	scp blog.tgz $(SERVER):~/
	ssh $(SERVER) sudo stop blog || true
	ssh $(SERVER) tar xzvf blog.tgz -C blog --overwrite
	ssh $(SERVER) sudo cp blog/resources/nginx/blog /etc/nginx/sites-enabled/
	ssh $(SERVER) sudo cp blog/resources/upstart/blog.conf /etc/init/
	ssh $(SERVER) sudo start blog

build:
	cabal clean
	cabal configure --disable-shared
	cabal build

devel:
	cabal clean
	cabal configure -fdevelopment
	cabal build
	./dist/build/blog/blog -p 9000
