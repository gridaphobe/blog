.PHONY: all build generate clean

all: generate

generate: build
	stack run blog -- rebuild

build:
	stack build

clean:
	stack clean