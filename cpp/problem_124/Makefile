SHELL := /bin/bash

default:
	time g++ -std=c++2a -O3 main.cc

benchmark: default
	hyperfine --warmup 1 './a.out'

debug:
	time g++ -std=c++2a -g main.cc

run: default
	time ./a.out

clean:
	rm -f a.out
